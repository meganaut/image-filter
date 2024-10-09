open System.IO
open System.Net
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats
open System.Text
open System
open System.Text.Json

// Models
type Pixel = { R: byte; G: byte; B: byte; A: byte }

type ImageData =
    { Width: int
      Height: int
      Pixels: Pixel[,] }

// Load Image Function
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

// Apply Filter Function
let applyFilter (filterFn: int -> int -> Pixel -> Pixel) (data: Pixel[,]) = data |> Array2D.mapi filterFn

let pngify height width pixels =
    use newImage = new Image<Rgba32>(width, height, Rgba32(0uy, 0uy, 0uy, 0uy))
    // sort of side affect. but it stays within this function so thats ok i guess?
    pixels
    |> Array2D.iteri (fun x y pixel -> newImage.[x, y] <- Rgba32(pixel.R, pixel.G, pixel.B, pixel.A))

    use memoryStream = new MemoryStream()
    newImage.Save(memoryStream, SixLabors.ImageSharp.Formats.Png.PngEncoder())
    memoryStream.ToArray()

type FilterRequest = { filter: string; imageData: string }

let parseMultipartFormData (request: HttpListenerRequest) =
    if request.ContentType.StartsWith("multipart/form-data") then
        let boundary = "--" + request.ContentType.Split('=').[1]
        use reader = new StreamReader(request.InputStream)
        let content = reader.ReadToEnd()
        let sections = content.Split(boundary, StringSplitOptions.RemoveEmptyEntries)

        let getFileSection (sections: string array) =
            sections
            |> Seq.tryFind (fun section -> section.Contains("Content-Disposition: form-data; name=\"file\""))

        match getFileSection sections with
        | Some fileSection ->
            // The file content starts after two newlines (headers + empty line)
            let headerEndIndex = fileSection.IndexOf("\r\n\r\n") + 4
            let fileContent = fileSection.Substring(headerEndIndex)
            let trimmedContent = fileContent.TrimEnd([| '\r'; '\n' |])

            // Convert the remaining binary content to a byte array
            let fileBytes = Encoding.Default.GetBytes(trimmedContent)
            Some(fileBytes)
        | None -> None
    else
        None

// Web Application
let processRequest (context: HttpListenerContext) =

    async {
        let request = context.Request
        let response = context.Response


        Console.WriteLine("{0} {1}", request.HttpMethod, request.Url.LocalPath)

        match request.HttpMethod, request.Url.LocalPath with
        | "POST", "/filter" ->
            Console.WriteLine("Processing image filter request...")

            if request.HasEntityBody then
                let! filterRequest =
                    request.InputStream
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
                        |> sprintf
                            """
                            <img id="uploadedImage"  src="data:image/png;base64,%s" alt="Filtered Image" />
                            """

                    let buffer = Encoding.UTF8.GetBytes(htmlResponse)
                    response.ContentType <- "text/html"
                    response.StatusCode <- 200
                    response.ContentLength64 <- int64 buffer.Length
                    response.OutputStream.Write(buffer, 0, buffer.Length)
                | Error error ->
                    response.StatusCode <- 500
                    let buffer = Encoding.UTF8.GetBytes(error)
                    response.ContentLength64 <- int64 buffer.Length
                    response.OutputStream.Write(buffer, 0, buffer.Length)
            else
                response.StatusCode <- 400
                let buffer = Encoding.UTF8.GetBytes("no image to process.")
                response.ContentLength64 <- int64 buffer.Length
                response.OutputStream.Write(buffer, 0, buffer.Length)


        | "POST", "/upload" ->
            Console.WriteLine("Processing image upload request...")


            let redfilter x y pixel = { pixel with R = byte 255 }

            match parseMultipartFormData request with
            | Some fileBytes ->
                let imageResult = new MemoryStream(fileBytes) |> loadImage

                match imageResult with
                | Ok image ->
                    let htmlResponse =
                        image.Pixels
                        |> applyFilter redfilter
                        |> pngify image.Height image.Width
                        |> Convert.ToBase64String
                        |> sprintf
                            """
                            <h1>Upload an Image</h1>
                                                                                      
                            <img id="uploadedImage"  src="data:image/png;base64,%s" alt="Filtered Image" />
                            
                            <div>
                                <label for="description">Image Filter:</label>
                                <input type="text" id="description" name="description" />
                            </div>
                            
                            <button 
                                hx-post="/filter"
                                hx-trigger="click"
                                hx-target="#uploadedImage"
                                hx-vals="{
                                    filter: document.getElementById('description').value
                                    imageData: document.getElementById('uploadedImage').src
                                }">
                                Apply Filter
                            </button>
                            """

                    let buffer = Encoding.UTF8.GetBytes(htmlResponse)
                    response.ContentType <- "text/html"
                    response.StatusCode <- 200
                    response.ContentLength64 <- int64 buffer.Length
                    response.OutputStream.Write(buffer, 0, buffer.Length)
                | Error error ->
                    response.StatusCode <- 500
                    let buffer = Encoding.UTF8.GetBytes(error)
                    response.ContentLength64 <- int64 buffer.Length
                    response.OutputStream.Write(buffer, 0, buffer.Length)

            | None ->
                response.StatusCode <- 400
                let buffer = Encoding.UTF8.GetBytes("No file uploaded.")
                response.ContentLength64 <- int64 buffer.Length
                response.OutputStream.Write(buffer, 0, buffer.Length)
        | "GET", "/" ->
            // Serve the HTML form for uploading an image
            let htmlResponse =
                """
                <html lang="en">
                    <head>
                        <script src="https://unpkg.com/htmx.org"></script>
                    </head>
                    <body>
                        <h1>Upload an Image</h1>

                        <form 
                            action="/upload" 
                            method="POST" 
                            class="form" 
                            enctype="multipart/form-data"
                            hx-post="{{request.path}}"
                            hx-target="this"
                            hx-swap="outerHTML" 
                            hx-encoding="multipart/form-data"
                        >
                            <input type="file" name="file" />
                            <input type="submit" value="Upload" />
                        </form>
                    </body>
                </html>
                """

            let buffer = Encoding.UTF8.GetBytes(htmlResponse)
            response.ContentType <- "text/html"
            response.StatusCode <- 200
            response.ContentLength64 <- int64 buffer.Length
            response.OutputStream.Write(buffer, 0, buffer.Length)
        | _ ->
            response.StatusCode <- 404
            let buffer = Encoding.UTF8.GetBytes("Not Found")
            response.ContentLength64 <- int64 buffer.Length
            response.OutputStream.Write(buffer, 0, buffer.Length)

        response.Close()
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
