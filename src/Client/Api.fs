module Client.Api
open Client
open Fetch.Types
open Thoth.Fetch
open Thoth.Json
open Fable.Core
open Shared.Domain
open Shared.Helpers

let getToken (tokenRequest : GoogleLoginRequest) = 
  let url = Shared.baseUrl + "api/token"
  JS.console.log (sprintf "Get token run %s" url)
  let r = {| IdToken = tokenRequest.IdToken |}
  Fetch.post<_, TokenResult> (url, data = r)

let sampleEntries () = Fetch.fetchAs<unit, MemorizationEntry list> "/sample.json"

let getEntries (g : AppUser) = 
  let authenticatedJsonHeaders =
      g.MemoriaToken 
      |> Option.map (sprintf "Bearer %s" >> HttpRequestHeaders.Authorization) 
      |> Option.toList 
      |> List.append [ HttpRequestHeaders.ContentType "application/json" ]
  Fetch.fetchAs<unit, MemorizationEntry list>(Shared.baseUrl + "api/init",headers = authenticatedJsonHeaders)


let getSample (id : System.Guid) = 
  let headers = [ HttpRequestHeaders.ContentType "application/json" ]
  Fetch.fetchAs<unit, MemorizationEntry option>(Shared.baseUrl + (sprintf "api/samples/%s" (id.ToString())),headers = headers)