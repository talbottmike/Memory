open System.IO
open System.Threading.Tasks

open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared
open System.Security.Claims
open Giraffe.ResponseWriters
open Giraffe.Core
open Microsoft.AspNetCore.Hosting

let tryGetEnv key = 
  match Environment.GetEnvironmentVariable key with
  | x when String.IsNullOrWhiteSpace x -> None 
  | x -> Some x

//let publicPath = Path.GetFullPath "../Client/public"
let publicPath =
  tryGetEnv "public_path"
  |> Option.defaultValue "./public"
  |> Path.GetFullPath

let port =
  "SERVER_PORT"
  |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let googleUserIdForAdmin = "1"
let matchUpUsers : HttpHandler = fun next ctx ->
    // A real implementation would match up user identities with something stored in a database, not hardcoded in Users.fs like this example
    let isAdmin =
        ctx.User.Claims |> Seq.exists (fun claim ->
            claim.Issuer = "Google" && claim.Type = ClaimTypes.NameIdentifier && claim.Value = googleUserIdForAdmin)
    if isAdmin then
        printfn "User is an admin of this demo app, adding admin role to user claims"
        ctx.User.AddIdentity(ClaimsIdentity([Claim(ClaimTypes.Role, "Admin", ClaimValueTypes.String, "MyApplication")]))
    next ctx

let loggedIn = pipeline {
    requires_authentication (Giraffe.Auth.challenge "Google")
    plug matchUpUsers
}

let isAdmin = pipeline {
    plug loggedIn
    requires_role "Admin" (redirectTo false "/fail")
}

let webApp = 
  router {
    pipe_through loggedIn

    get "/" (fun next ctx -> 
        (ctx.User.Identity.Name |> json) next ctx)

    get "/account/login" (redirectTo false "/")
    
    // get "/" (htmlView Index.layout)
    // get "/index.html" (redirectTo false "/")
    // get "/default.html" (redirectTo false "/")
    get "/admin" (isAdmin >=> (fun next ctx -> 
        (ctx.User.Identity.Name |> json) next ctx))

    get "/api/init" (fun next ctx ->
      task {
        let counter = 42
        return! json counter next ctx
      }
    )
  }

// let top = router {
//   pipe_through loggedIn
//   get "/" (fun next ctx -> 
//       (ctx.User.Identity.Name |> json) next ctx)
// }

let clientId =  Environment.GetEnvironmentVariable "google_client_id"
let clientSecret = Environment.GetEnvironmentVariable "google_client_secret"
let authCallBack = "/oauth2callback"
let claims = []//["id", ClaimTypes.NameIdentifier; "displayName", ClaimTypes.Name] 


let configureCors (builder : Microsoft.AspNetCore.Cors.Infrastructure.CorsPolicyBuilder) =
  builder.WithOrigins("http://localhost:8080").AllowAnyMethod().AllowAnyHeader()
  |> ignore
  builder.WithOrigins("http://localhost:8085").AllowAnyMethod().AllowAnyHeader()
  |> ignore
  builder.WithOrigins("https://login.microsoftonline.com").AllowAnyMethod()
         .AllowAnyHeader() |> ignore
  builder.WithOrigins("https://accounts.google.com").AllowAnyMethod()
         .AllowAnyHeader() |> ignore

// let hostConfig (webHost:IWebHostBuilder) =
//   webHost.ConfigureAppConfiguration(fun ctx builder -> builder.AddJsonFile(sprintf "appSettings.%s.json" ctx.HostingEnvironment.EnvironmentName) |> ignore )
let app = 
  application {
    disable_diagnostics
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_cors "corsConfig" configureCors
    use_mime_types [(".webmanifest","application/manifest+json"); ]
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
    use_google_oauth clientId clientSecret authCallBack claims
  }

run app
