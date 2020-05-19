module WebStorage

open Thoth.Json

let load (decoder: Decoder<'T>) key: Result<'T,string> =
  let o = Browser.WebStorage.localStorage.getItem key
  if isNull o then 
    "No item found in local storage with key " + key |> Error
  else 
    Decode.fromString decoder o

let tryLoad (decoder: Decoder<'T>) key: Option<'T> =
  let o = Browser.WebStorage.localStorage.getItem key
  if isNull o then 
    None
  else 
    match Decode.fromString decoder o with
    | Ok v -> Some v
    | Error _ -> None
    
let delete key =
  Browser.WebStorage.localStorage.removeItem(key)

let inline save key (data: 'T) =
  Browser.WebStorage.localStorage.setItem(key, Encode.Auto.toString(0, data))

module Entries =
  open Shared.Domain
  open System
  
  let private entryListDecoder = Decode.Auto.generateDecoder<MemorizationEntryDisplay list>()
  let private entryDecoder = Decode.Auto.generateDecoder<MemorizationEntryDisplay>()

  let load () : MemorizationEntryDisplay list =
    match load entryListDecoder "entryList" with
    | Ok entries -> entries
    | Error _ -> [ ]

  let saveEntry (entry : MemorizationEntryDisplay) : unit =
    let existingEntries = load ()
    let existingEntryIdOption = existingEntries |> List.tryFind (fun x -> x.Id = entry.Id) |> Option.map (fun x -> x.Id)
    let updatedEntries =
      match existingEntryIdOption with
      | None -> entry :: existingEntries
      | Some existingEntryId -> existingEntries |> List.map (fun x -> if x.Id = entry.Id then entry else x)
    save "entryList" updatedEntries
    save (entry.Id.ToString()) entry

  let removeEntry (entryId : Guid) : unit =
    let existingEntries = load ()
    let updatedEntries = existingEntries |> List.filter (fun x -> x.Id <> entryId)
    save "entryList" updatedEntries