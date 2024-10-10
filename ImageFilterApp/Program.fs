open System.IO
open System.Net
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open System.Text
open System
open System.Text.Json
open HttpMultipartParser

type Pixel = { R: byte; G: byte; B: byte; A: byte }

type ImageData =
    { Width: int
      Height: int
      Pixels: Pixel[,] }

let loadImage (file: Stream) =
    try
        use image = Image.Load<Rgba32>(file)
        let width, height = image.Width, image.Height

        // Convert image to a 2D array of pixels
        let pixels =
            Array2D.init width height (fun x y ->
                let pixel = image.[x, y]

                { R = pixel.R
                  G = pixel.G
                  B = pixel.B
                  A = pixel.A })

        Ok
            { Width = width
              Height = height
              Pixels = pixels }
    with ex ->
        Error(sprintf "Failed to load image: %s" ex.Message)

let applyFilter (filterFn: int -> int -> Pixel -> Pixel) (data: Pixel[,]) = data |> Array2D.mapi filterFn

let prepend preText bodyText = preText + bodyText

let render template (chunks: (string * string) list) =
    let rec renderChunks (chunks: (string * string) list) template : string =
        match chunks with
        | [] -> template
        | chunk :: rest ->
            let (key, str) = chunk
            let updatedTemplate = template.Replace("${" + key + "}", str)
            renderChunks rest updatedTemplate

    template |> prepend "./templates/" |> File.ReadAllText |> renderChunks chunks


let pngify height width pixels =
    use newImage = new Image<Rgba32>(width, height, Rgba32(0uy, 0uy, 0uy, 0uy))
    // sort of side affect. but it stays within this function so thats ok i guess?
    pixels
    |> Array2D.iteri (fun x y pixel -> newImage.[x, y] <- Rgba32(pixel.R, pixel.G, pixel.B, pixel.A))

    use memoryStream = new MemoryStream()
    newImage.Save(memoryStream, SixLabors.ImageSharp.Formats.Png.PngEncoder())
    memoryStream.ToArray()

type FilterRequest = { filter: string; imageData: string }

let parseMultipartFormData inputStream =
    let parser = inputStream |> MultipartFormDataParser.Parse

    try
        Some(parser.Files[0].Data)
    with ex ->
        Console.WriteLine("Failed to parse multipart form data: %s", ex.Message)
        None

let respondWith (handlerFunc: Stream -> Async<Result<string, string>>) (context: HttpListenerContext) =
    async {
        let request = context.Request
        let response = context.Response
        Console.WriteLine("{0} {1} -- Begin", request.HttpMethod, request.Url.LocalPath)

        try
            let stream = request.InputStream
            let! responseResult = handlerFunc stream

            match responseResult with
            | Ok responseText ->
                let buffer = Encoding.UTF8.GetBytes(responseText)
                response.ContentType <- "text/html"
                response.StatusCode <- 200
                response.ContentLength64 <- int64 buffer.Length
                response.OutputStream.Write(buffer, 0, buffer.Length)
            | Error error ->
                Console.WriteLine("Error: %s", error)
                response.StatusCode <- 400
                let buffer = error |> prepend "U dun goofed: " |> Encoding.UTF8.GetBytes
                response.ContentLength64 <- int64 buffer.Length
                response.OutputStream.Write(buffer, 0, buffer.Length)
        with ex ->
            Console.WriteLine("Error: %s", ex.Message)
            response.StatusCode <- 500
            let buffer = Encoding.UTF8.GetBytes("Internal Server Error")
            response.ContentLength64 <- int64 buffer.Length
            response.OutputStream.Write(buffer, 0, buffer.Length)

        Console.WriteLine("{0} {1} -- End: status={2}", request.HttpMethod, request.Url.LocalPath, response.StatusCode)

        response.Close()
    }

let processRequest (context: HttpListenerContext) =
    async {
        match context.Request.HttpMethod, context.Request.Url.LocalPath with
        | "POST", "/filter" ->
            context
            |> respondWith (fun stream ->
                async {
                    let! filterRequest =
                        stream
                        |> JsonSerializer.DeserializeAsync<FilterRequest>
                        |> fun vt -> vt.AsTask()
                        |> Async.AwaitTask

                    let greenfilter x y pixel = { pixel with G = byte 255 }

                    let imageResult =
                        filterRequest.imageData
                        |> Convert.FromBase64String
                        |> fun data -> new MemoryStream(data)
                        |> loadImage

                    match imageResult with
                    | Ok image ->
                        let htmlResponse =
                            image.Pixels
                            |> applyFilter greenfilter
                            |> pngify image.Height image.Width
                            |> Convert.ToBase64String
                            |> (fun img -> render "filter.frag.html" [ ("image", img) ])

                        return Ok(htmlResponse)
                    | Error error -> return Error(error)
                })
        | "POST", "/upload" ->
            context
            |> respondWith (fun stream ->
                async {
                    match stream |> parseMultipartFormData with
                    | Some imageStream ->
                        match imageStream |> loadImage with
                        | Ok image ->
                            let htmlResponse =
                                image.Pixels
                                |> pngify image.Height image.Width
                                |> Convert.ToBase64String
                                |> (fun img -> render "upload.frag.html" [ ("image", img) ])

                            return Ok(htmlResponse)
                        | Error error -> return Error(error)
                    | None -> return Error("Failed to parse image")
                })
        | "GET", "/" ->
            context
            |> respondWith (fun _ ->
                async {
                    let htmlResponse = render "index.html" []
                    return Ok(htmlResponse)
                })
        | _ ->
            async {
                let response = context.Response
                response.StatusCode <- 404
                let buffer = Encoding.UTF8.GetBytes("Not Found")
                response.ContentLength64 <- int64 buffer.Length
                response.OutputStream.Write(buffer, 0, buffer.Length)
                response.Close()
            }
        |> Async.StartAsTask
        |> ignore
    }


let startServer (port: int) =
    let listener = new HttpListener()
    let url = sprintf "http://localhost:%d/" port
    listener.Prefixes.Add(url)
    listener.Start()
    printfn "Listening on %s" url

    let rec loop () =
        async {
            let! context = listener.GetContextAsync() |> Async.AwaitTask
            do! processRequest context
            return! loop ()
        }

    loop () |> Async.RunSynchronously

[<EntryPoint>]
let main argv =
    let port =
        match argv with
        | [| portStr |] when let mutable port = 0 in System.Int32.TryParse(portStr, &port) -> int portStr
        | _ -> 5001

    startServer port
    0
