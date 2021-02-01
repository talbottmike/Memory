module Client.Pages
open System
// open Elmish.UrlParser
open Feliz.Router

/// The different pages of the application. If you add a new page, then add an entry here.
[<RequireQualifiedAccess>]
type Page =
  | Home
  | Editor of Guid option
  | Entries
  | Practice of Guid option
  | FlashCards

let toHash =
  function
  | Page.Home -> "home"
  | Page.Editor id -> id |> Option.map (fun x -> sprintf "editor?id=%s" (x.ToString())) |> Option.defaultValue "editor"
  | Page.Entries -> "entries"
  | Page.Practice id -> id |> Option.map (fun x -> sprintf "practice?id=%s" (x.ToString())) |> Option.defaultValue "practice"
  | Page.FlashCards -> "flashcards"

// string list -> Page
let parseUrl = function
    // matches #/ or #
    | [ ] ->  Page.Home
    // matches #/editor?id={id} where id is an Guid
    | [ "editor"; Route.Query [ "id", Route.Guid id ] ] -> Page.Editor (Some id)
    // matches #/editor or #/editor/ or #editor
    | [ "editor" ] -> Page.Editor None
    // matches #/entries or #/entries/ or #entries
    | [ "entries" ] -> Page.Entries
    // matches #/practice?id={id} where id is an Guid
    | [ "practice"; Route.Query [ "id", Route.Guid id ] ] -> Page.Practice (Some id)
    // matches #/practice or #/practice/ or #practice
    | [ "practice" ] -> Page.Practice None
    // matches #/flashcards or #/flashcards/ or #flashcards
    | [ "flashcards" ] -> Page.FlashCards
    // // matches #/users/{userId}
    // | [ "users"; Route.Int userId ] -> Page.User userId
    // matches everything else
    | _ -> Page.Home
