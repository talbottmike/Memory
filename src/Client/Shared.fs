module Client.Shared

open Shared.Domain

let baseUrl =
  if Browser.Dom.window.location.host.ToUpper().Contains("MEMORIAMASTERED")
  then "https://memoria.azurewebsites.net/"
  else Browser.Dom.window.location.origin + "/"
  