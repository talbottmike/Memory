module Client.Menu

open Fable.React
open Client.Styles
open Client.Pages
open Shared.Domain
open Shared.Domain.Menu
open Client.Utils
open Fulma
open Fable.MaterialUI.MaterialDesignIcons
open Fable.React.Props
open Fable.Core

let renderFn (props:Props) =
  let homePage = if props.Model.User.IsSome then "#entries" else "#home"
  Navbar.navbar [ Navbar.Color IsPrimary ]
    [ Navbar.Brand.div [ ]
        [ Navbar.Item.a [ Navbar.Item.Props [ Href homePage ] ]
            [ img [ Style [ Width "2.5em" ] // Force svg display
                    Src "shape.svg" ] ]
          Navbar.Item.div [ ]
            [ Heading.h2 [ ]
                [ str "memoria" ] ] 
          Navbar.burger [ Navbar.Burger.IsActive props.Model.IsBurgerOpen; Navbar.Burger.OnClick (fun _ -> props.OnToggleBurger()) ]
                  [ span [] []
                    span [] []
                    span [] [] ] ]
      
      Navbar.menu [ Navbar.Menu.IsActive props.Model.IsBurgerOpen ]
        [ Navbar.Start.div []
            [ Navbar.Item.div [ ] [ ] ]
          Navbar.End.div [ ]
            [ //Styles.iconButton "Demote user" (fun _ -> dispatch DemoteUser) boomGateDownIcon |> Styles.adminOnly props.Model.User 
              
              match props.Model.User with
              | Some _ -> Navbar.Item.div [ ] [ Button.button [ Button.OnClick (fun _ -> Auth.signOut props.OnLogout) ] [ str "Sign out"] ]
              | None -> Navbar.Item.a [ Navbar.Item.Props [ Href homePage ] ] [ Button.button [ ] [ str "Sign In"] ] ] ] ]

let view = elmishView "Menu" renderFn