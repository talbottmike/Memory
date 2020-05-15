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
open FSharp.Control.Tasks.V2.ContextInsensitive
open Google.Apis.Auth
open FsToolkit.ErrorHandling
open FSharp.CosmosDb
open Azure.Cosmos
open Azure.Cosmos.Serialization
open System.IdentityModel.Tokens.Jwt
open Microsoft.IdentityModel.Tokens
open Microsoft.AspNetCore.Http
open System.Text
open FSharp.Control

let tryGetEnv key = 
  match Environment.GetEnvironmentVariable key with
  | x when String.IsNullOrWhiteSpace x -> None 
  | x -> Some x

let clientId =  Environment.GetEnvironmentVariable "google_client_id"
let clientSecret = Environment.GetEnvironmentVariable "google_client_secret"
let appEnvironment = tryGetEnv "app_environment" |> Option.defaultValue "dev"

module Data =
  let cosmosConnectionString = tryGetEnv "memoria_cosmos_connection_string" |> Option.defaultValue ""

  type CustomCosmosSerializer () =
    inherit CosmosSerializer ()
    let defaultEncoding = UTF8Encoding(false, true)
    let _serializer = Newtonsoft.Json.JsonSerializer.Create(Newtonsoft.Json.JsonSerializerSettings())
    override this.FromStream<'a>(stream : Stream) =
      use reader = new StreamReader(stream)
      let text = reader.ReadToEnd()
      // if (typeof<Stream>.IsAssignableFrom(typeof<'a>))
      // then ('a)(obj)stream
      // else
      use sr = new StringReader(text)
      use jsonTextReader = new Newtonsoft.Json.JsonTextReader(sr)
      _serializer.Deserialize<'a>(jsonTextReader)
    override this.ToStream<'a>(input : 'a) = 
      let streamPayload = new MemoryStream()
      use streamWriter = new StreamWriter(streamPayload, encoding = defaultEncoding, bufferSize = 1024, leaveOpen = true)
      use writer = new Newtonsoft.Json.JsonTextWriter(streamWriter)
      writer.Formatting <- _serializer.Formatting
      _serializer.Serialize(writer, input)
      writer.Flush()
      streamWriter.Flush()
      streamPayload.Position <- 0L
      streamPayload :> Stream

  let databaseName = "memoria"
  let containerName = "Users"
  
  [<CLIMutable>]
  type UserStorageModel =
    { id: string
      [<PartitionKey>]
      partition: string
      Model : StorageUser }

  type UserKey = { id : string; partition : string; }

  let existsUser (emailAddress : string) =
    task {
      let userId = emailAddress.ToLower()
      let partition = sprintf "/%s/%s/%s" appEnvironment "Users" (emailAddress.ToLower())
      let! exists = 
        CosmosClientOptions(Serializer = CustomCosmosSerializer())
        |> Cosmos.fromConnectionStringWithOptions cosmosConnectionString
        |> Cosmos.database databaseName
        |> Cosmos.container containerName
        |> Cosmos.query "SELECT * FROM a WHERE a.id = @userId and a.partition = @partition"
        |> Cosmos.parameters [ ("@userId", box userId); ("@partition", box partition) ]
        |> Cosmos.execAsync<UserKey>
        |> AsyncSeq.exists (fun x -> x.id = userId)
        |> Async.StartAsTask
      return exists
    }

  let getOrAddUser (emailAddress : string) =
    task {
      let userId = emailAddress.ToLower()
      let partition = sprintf "/%s/%s/%s" appEnvironment "Users" (emailAddress.ToLower())
      let container =
        CosmosClientOptions(Serializer = CustomCosmosSerializer())
        |> Cosmos.fromConnectionStringWithOptions cosmosConnectionString
        |> Cosmos.database databaseName
        |> Cosmos.container containerName
      let! existingUser = 
        container
        |> Cosmos.query "SELECT * FROM a WHERE a.id = @userId and a.partition = @partition"
        |> Cosmos.parameters [ ("@userId", box userId); ("@partition", box partition) ]
        |> Cosmos.execAsync<UserStorageModel>
        |> AsyncSeq.tryFirst
        |> Async.StartAsTask
      let! result = 
        existingUser
        |> Option.map Task.singleton
        |> Option.defaultWith (fun () -> 
          let newUser = { id = userId; partition = partition; Model = { EmailAddress = userId; Role = None; Entries = []; } }
          container
          |> Cosmos.insert<UserStorageModel> newUser
          |> Cosmos.execAsync
          |> AsyncSeq.firstOrDefault newUser
          |> Async.StartAsTask
        )
      return result
    }
        
  let addEntry (emailAddress : string) (request : MemorizationEntry) =
    task {
      let userId = emailAddress.ToLower()
      let partition = sprintf "/%s/%s/%s" appEnvironment "Users" (emailAddress.ToLower())
      let updateFn (m : UserStorageModel) = 
        let exists = m.Model.Entries |> List.exists (fun x -> x.Id = request.Id)
        let newModel =
          if exists
          then 
            m.Model
          else
            { m.Model with Entries = m.Model.Entries |> List.append [request] }
        { m with Model = newModel }
      let! result = 
        CosmosClientOptions(Serializer = CustomCosmosSerializer())
        |> Cosmos.fromConnectionStringWithOptions cosmosConnectionString
        |> Cosmos.database databaseName
        |> Cosmos.container containerName
        |> Cosmos.update userId partition updateFn
        |> Cosmos.execAsync
        |> AsyncSeq.iter ignore
        |> Async.StartAsTask
      return result
    }

  let getEntries () =
    task {
      let queryString = sprintf "SELECT * FROM a"
      let! result =
        CosmosClientOptions(Serializer = CustomCosmosSerializer())
        |> Cosmos.fromConnectionStringWithOptions cosmosConnectionString
        |> Cosmos.database databaseName
        |> Cosmos.container containerName
        |> Cosmos.query queryString
        |> Cosmos.execAsync<UserStorageModel>
        |> AsyncSeq.map (fun x -> x.Model.Entries)
        |> AsyncSeq.toListAsync
        |> Async.map (fun x -> x |> List.collect id)
        |> Async.StartAsTask
      return result
    }

//let publicPath = Path.GetFullPath "../Client/public"
let publicPath =
  tryGetEnv "public_path"
  |> Option.defaultValue "./public"
  |> Path.GetFullPath

let port = "SERVER_PORT" |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let jwtSecret = Environment.GetEnvironmentVariable "jwt_secret"
let jwtIssuer = "memoriamastered"

let generateToken email =
  let claims = 
    [| Claim(JwtRegisteredClaimNames.Sub, email);
       Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) |]
  claims
  |> Auth.generateJWT (jwtSecret, SecurityAlgorithms.HmacSha256) jwtIssuer (DateTime.UtcNow.AddHours(1.0))

let getSecuredUserId (ctx : HttpContext) =
  let email = ctx.User.FindFirst ClaimTypes.NameIdentifier
  email.Value.ToLower()

let handlePostToken =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let! request = ctx.BindJsonAsync<GoogleLoginRequest>()
      // authenticate user
      let! p = GoogleJsonWebSignature.ValidateAsync(request.IdToken, GoogleJsonWebSignature.ValidationSettings(Audience = [ clientId ]))
      let! u = Data.getOrAddUser p.Email
      let tokenResult = { TokenResult.Token = generateToken u.id }
      return! json tokenResult next ctx
    }

let securedRouter = 
  router {
    pipe_through (Auth.requireAuthentication JWT)
    //get "/" handleGetSecured
    get "/init" (fun next ctx ->
      task {
        let! entries = Data.getEntries ()
        return! json entries next ctx
      }
    )
    post "/add" (fun next ctx ->
      task {
        let! request = ctx.BindJsonAsync<Shared.MemorizationEntry>()
        let userId = getSecuredUserId ctx
        do! Data.addEntry userId request
        return! json request next ctx
      }
    )
  }

let topRouter = 
  router {
    not_found_handler (setStatusCode 404 >=> text "Not Found")
    //get "/" (text "public route")
    post "/api/token" handlePostToken
    forward "/api" securedRouter
  }

let configureCors (builder : Microsoft.AspNetCore.Cors.Infrastructure.CorsPolicyBuilder) =
  builder.WithOrigins("http://localhost:8080").AllowAnyMethod().AllowAnyHeader() |> ignore
  builder.WithOrigins("http://localhost:8085").AllowAnyMethod().AllowAnyHeader() |> ignore
  builder.WithOrigins("https://login.microsoftonline.com").AllowAnyMethod().AllowAnyHeader() |> ignore
  builder.WithOrigins("https://accounts.google.com").AllowAnyMethod().AllowAnyHeader() |> ignore
  builder.WithOrigins("https://memoriamastered.com").AllowAnyMethod().AllowAnyHeader() |> ignore

// let hostConfig (webHost:IWebHostBuilder) =
//   webHost.ConfigureAppConfiguration(fun ctx builder -> builder.AddJsonFile(sprintf "appSettings.%s.json" ctx.HostingEnvironment.EnvironmentName) |> ignore )
let app = 
  application {
    disable_diagnostics
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_cors "corsConfig" configureCors
    use_mime_types [(".webmanifest","application/manifest+json"); ]
    use_router topRouter
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
    use_jwt_authentication jwtSecret jwtIssuer
  }

run app
