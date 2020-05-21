module Client.Auth
open Fable.Core.JsInterop
open Shared.Domain
open Fable.Core

let init (authConfiguredFn : unit -> unit) (signedInFn : UserProvider -> unit) = 
  let onSignIn g = 
    // JS.console.log("on sign in, granted scopes: ")
    // JS.console.log(g?getGrantedScopes())
    let token = g?getAuthResponse()?id_token
    let profile = g?getBasicProfile()
    let profileId = profile?getId()
    let profileName =  profile?getName()
    let profileEmail =  profile?getEmail()
    // JS.console.log(profile)
    let user = 
      { Token = token.ToString()
        Id = profileId.ToString()
        Name = profileName.ToString()
        Email = profileEmail.ToString() }
      |> Google
    signedInFn user
  let configureAuth () =
    Browser.Dom.window?gapi?auth2?init(
      {|
        client_id = "189067839764-1i0jqhpp1igdf0cdenghhs6f09spbseu.apps.googleusercontent.com"
        fetch_basic_profile = false
        scope = "profile email openid"
      |}
    )
    let config = 
      {|
        scope = "profile email openid"
        width = 200
        height = 36
        longtitle = true
        theme = "light"
        onsuccess = onSignIn
        onfailure = null
      |}
    Browser.Dom.window?gapi?signin2?render("g-signin-btn", config);
  Browser.Dom.window?gapi?load("auth2", configureAuth)
  authConfiguredFn ()
  ()
let disconnect disconnectedFn =
  let auth2 = Browser.Dom.window?gapi?auth2?getAuthInstance()
  let isUserSignedIn = auth2?isSignedIn?get()
  if ((bool) isUserSignedIn)
  then
    auth2?disconnect()
  else 
    //JS.console.log("Not signed in, cannot disconnect")
    ()
  disconnectedFn ()
let signOut signedOutFn =
  let auth2 = Browser.Dom.window?gapi?auth2?getAuthInstance()
  auth2?signOut()?``then``(signedOutFn)