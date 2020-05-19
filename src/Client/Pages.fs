module Client.Pages
open System
open Elmish.UrlParser

/// The different pages of the application. If you add a new page, then add an entry here.
[<RequireQualifiedAccess>]
type Page =
  | Home
  | Editor of Guid option
  | Entries
  | Practice of Guid option

let toHash =
  function
  | Page.Home -> "#home"
  | Page.Editor id -> id |> Option.map (fun x -> sprintf "#editor?id=%s" (x.ToString())) |> Option.defaultValue "#editor"
  | Page.Entries -> "#entries"
  | Page.Practice id -> id |> Option.map (fun x -> sprintf "#practice?id=%s" (x.ToString())) |> Option.defaultValue "#practice"

let guidParam name =
  (fun x -> 
    x
    |> Option.bind (fun v ->
      match System.Guid.TryParse v with
      | false, _ -> None
      | true, guid -> Some guid
    )
  )
  |> customParam name
/// The URL is turned into a Result.
let pageParser : Parser<Page -> Page,_> =
  oneOf
    [ map Page.Home (s "home")
      map Page.Editor (s "editor" <?> guidParam "id")
      map Page.Entries (s "entries")
      map Page.Practice (s "practice" <?> guidParam "id") ]

let urlParser location = parseHash pageParser location
