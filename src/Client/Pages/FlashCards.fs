module Client.FlashCards

open Elmish
// open Fable.Remoting.Client
open Shared
open Shared.Domain.FlashCards
open Fable.React
open Fable.React.Helpers
open Fable.React.Standard
open Fable.React.Props
open Fulma
open Thoth.Fetch

let getRandomFlashCard (lessons : string list) =  
  let rnd = System.Random()  
  let filteredCards = Client.FlashCardInfo.allFlashCards |> List.filter (fun x -> lessons |> List.contains x.Lesson) 
  filteredCards |> List.item (rnd.Next(filteredCards.Length))

let flashCardsApi =
    { getAll = fun (lessons : string list) -> async { return Client.FlashCardInfo.allFlashCards |> List.filter (fun x -> lessons |> List.contains x.Lesson) }
      get = fun (lessons : string list) -> async { return getRandomFlashCard lessons }
      getLessons = fun () -> async { return Client.FlashCardInfo.allFlashCards |> List.map (fun x -> x.Lesson) |> List.distinct } }

//let flashCardsApi =
    // Remoting.createApi()
    // |> Remoting.withRouteBuilder Route.builder
    // |> Remoting.buildProxy<IFlashCardsApi>

let getAll lessons =
  Cmd.OfAsync.perform flashCardsApi.getAll lessons SetList

let get lessons =
  Cmd.OfAsync.perform flashCardsApi.get lessons SetCard

let getLessons () =
  Cmd.OfAsync.perform flashCardsApi.getLessons () SetLessons

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Lessons = []; State = FullList [] }
    let loadLessonsCmd = getLessons ()
    initialModel, loadLessonsCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    Fable.Core.JS.console.log msg
    Fable.Core.JS.console.log currentModel
    let selectedLessons = currentModel.Lessons |> List.filter (fun x -> x.Selected) |> List.map (fun x -> x.Lesson)
    match currentModel.State, msg with    
    | CardState.FlashCard x, ShowAnswer ->
        let nextModel = { x with ShowAnswer = true }
        { currentModel with State = FlashCard nextModel}, Cmd.none
    | _, SetCard cardInfo ->
        let nextModel = { FlashCard = cardInfo; ShowAnswer = false; }
        { currentModel with State = FlashCard nextModel}, Cmd.none
    | _, FetchQuestion ->
        currentModel, Cmd.OfAsync.perform flashCardsApi.get selectedLessons SetCard
    | CardState.FlashCard x, ToggleList ->
        currentModel, getAll selectedLessons
    | CardState.FullList x, ToggleList ->
        currentModel, Cmd.OfAsync.perform flashCardsApi.get selectedLessons SetCard
    | _, SetList newList ->
        { currentModel with State = CardState.FullList newList}, Cmd.none
    | CardState.FullList _, ShowAnswer -> currentModel, Cmd.none
    | _, SetLessons lessons ->
      let newLessons = lessons |> List.map (fun x -> { Lesson = x; Selected = true; })
      { currentModel with Lessons = newLessons }, getAll lessons
    | _, ToggleLessonSelection lesson ->
      let newLessons = currentModel.Lessons |> List.map (fun x -> { x with Selected = if x.Lesson = lesson then not x.Selected else x.Selected; })
      let cmd =
        let lessonsToGet = (newLessons |> List.filter (fun x -> x.Selected) |> List.map (fun x -> x.Lesson))
        match currentModel.State with
        | CardState.FullList _ -> getAll lessonsToGet
        | CardState.FlashCard _ -> get lessonsToGet

      { currentModel with Lessons = newLessons }, cmd

let show = function
| CardState.FlashCard x when x.ShowAnswer -> "Click for next card"
| CardState.FlashCard x when not x.ShowAnswer -> "Click to reveal answer"
| CardState.FullList x when x.Length = 0 -> "Loading..."
| _ -> ""

let latinTextToElements items =
  items
  |> List.map (fun s ->
    match s with
    | LatinText.Normal v -> str v
    | LatinText.Macron v -> span [(*(DangerouslySetInnerHTML { __html = "&" + v.ToString() + "macr;" }) :> IHTMLProp*)] []
  )
  
let showCard x (dispatch : Msg -> unit) =
  
  match x.ShowAnswer with
  | true ->
      Hero.hero [ Hero.Color IsInfo; Hero.IsHalfHeight; ] [
        Hero.head [][
          str x.FlashCard.Lesson
        ]
        
        Hero.body [ Props[ Fable.React.Props.OnClick (fun _ -> dispatch FetchQuestion);] ][
          Container.container [][
            Columns.columns [ Columns.IsCentered; Columns.IsMultiline; ]
              [ 
                Column.column [ 
                  Column.Width(Screen.All, Column.IsHalf); 
                ] [ 
                  Heading.h1 [ Heading.Modifiers[ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]][str (x.FlashCard.Back.Answer)]
                ]

              ]
          ] 
        ]
        Hero.foot [][
        ]
      ]
      
  | false ->

      let question = x.FlashCard.Front.Question
      Hero.hero [ Hero.Color IsPrimary; Hero.IsHalfHeight; ] [
        Hero.head [][str " "]
        Hero.body [ Props[ Fable.React.Props.OnClick (fun _ -> dispatch ShowAnswer);] ][
          Container.container [][
            Heading.h1 [ Heading.Modifiers[ Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]][yield str question]
        ]
        ]
        Hero.foot [][str " "]
      ]

let showCardList cards (dispatch : Msg -> unit) =
  [ 
    for card in cards do
      yield Columns.columns [] [
        Column.column [] [str card.Front.Question]
        Column.column [] [str card.Back.Answer]
      ]
  ]

let containerBox (model : Model) (dispatch : Msg -> unit) =
    let toggleButtonLabel = 
      match model.State with
      | CardState.FlashCard _ -> "List"
      | CardState.FullList _ -> "Card"

    Box.box' [ ] [
        Content.content [ ] [
        Columns.columns [] [
          Column.column [] [ 
            Heading.h3 [] [ str (show model.State) ]
            Button.button [Button.OnClick (fun _ -> dispatch ToggleList)] [str toggleButtonLabel] ] ]
          
        Columns.columns [] [
          Column.column [] [ 
            match model.State with
            | CardState.FlashCard v -> yield showCard v dispatch
            | CardState.FullList v -> yield! showCardList v dispatch
          ]]]]

let view (model : Model) (dispatch : Msg -> unit) =
  Hero.hero [
      Hero.Color IsPrimary
      Hero.IsFullHeight
      Hero.Props [
          Style [
              Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
              BackgroundSize "cover"
          ]
      ]
  ] [
      Hero.head [ ] [
          Navbar.navbar [ ] [
              Container.container [ ] [ ]
          ]
      ]

      Hero.body [ ] [
          Container.container [ ] [
              Column.column [
              ] [
                  Heading.p [ ] [ str "Lessons" ]
                  for l in model.Lessons do
                    Button.button 
                      [ Button.Color (if l.Selected then IsPrimary else IsGreyDark)
                        Button.OnClick (fun _ -> ToggleLessonSelection l.Lesson |> dispatch) ] 
                      [ str l.Lesson ]
              ]
              Column.column [
                  Column.Width (Screen.All, Column.Is6)
                  Column.Offset (Screen.All, Column.Is3)
              ] [
                  Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "Flash cards" ]
                  containerBox model dispatch
              ]
          ]
      ]
  ]