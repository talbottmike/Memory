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

let getRandomFlashCard (lessons : int list) =  
  let rnd = System.Random()  
  let filteredCards = Client.FlashCardInfo.allFlashCards |> List.filter (fun x -> lessons |> List.contains x.Lesson) 
  filteredCards |> List.item (rnd.Next(filteredCards.Length))

let allLessons = 
  Client.FlashCardInfo.allFlashCards 
  |> List.map (fun x -> { Lesson = x.Lesson; Selected = false; }) 
  |> List.distinct
  |> List.sortBy (fun x -> x.Lesson)

let flashCardsApi =
    { getAll = fun (lessons : int list) -> 
        async { 
          let rnd = System.Random()
          let filteredCards = 
            Client.FlashCardInfo.allFlashCards 
            |> List.filter (fun x -> lessons |> List.contains x.Lesson)
          let selectedCards =
            filteredCards
            |> List.map (fun x -> (rnd.Next(filteredCards.Length),x))
            |> List.sortBy (fun (x,_) -> x)
            |> List.map snd
          return selectedCards
        }
      get = fun (lessons : int list) -> async { return getRandomFlashCard lessons }
      getLessons = fun () -> async { return Client.FlashCardInfo.allFlashCards |> List.map (fun x -> x.Lesson) |> List.distinct } }

//let flashCardsApi =
    // Remoting.createApi()
    // |> Remoting.withRouteBuilder Route.builder
    // |> Remoting.buildProxy<IFlashCardsApi>

let getAll lessons =
  Cmd.OfAsync.perform flashCardsApi.getAll lessons SetList

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Lessons = allLessons; FlashCardData = []; PracticeDirection = PracticeDirection.Forwards; CardState = None }
    // let loadLessonsCmd = getLessons ()
    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    Fable.Core.JS.console.log msg
    Fable.Core.JS.console.log currentModel
    let selectedLessons = currentModel.Lessons |> List.filter (fun x -> x.Selected) |> List.map (fun x -> x.Lesson)
    match currentModel.CardState, msg with    
    | Some x, ShowAnswer ->
        let nextModel = { x with ShowAnswer = true }
        { currentModel with CardState = Some nextModel}, Cmd.none
    | _, ShowAnswer -> 
      currentModel, Cmd.none
    | _, SetCard cardInfo ->
        let nextModel = { FlashCard = cardInfo; ShowAnswer = false; }
        { currentModel with CardState = Some nextModel}, Cmd.none
    | None, ShowNextCard -> currentModel, Cmd.none
    | Some selectedCard, ShowNextCard ->
        let selectedCardIndex = currentModel.FlashCardData |> List.tryFindIndex (fun x -> x = selectedCard.FlashCard)
        let maxIndex = currentModel.FlashCardData.Length - 1
        let nextCardIndex =
          selectedCardIndex
          |> Option.map (fun x -> 
            if x = maxIndex then 0 else x + 1
          )
          |> Option.defaultValue 0
        let cmd =
          currentModel.FlashCardData 
          |> List.tryItem nextCardIndex
          |> Option.map (SetCard >> Cmd.ofMsg)
          |> Option.defaultValue Cmd.none
        currentModel, cmd
    | _, ShowList ->
        { currentModel with CardState = None }, Cmd.none
    | None, Practice practiceDirection ->
        let cmd =
          currentModel.FlashCardData 
          |> List.tryHead
          |> Option.map (SetCard >> Cmd.ofMsg)
          |> Option.defaultValue Cmd.none
        { currentModel with PracticeDirection = practiceDirection }, cmd
    | _, SetList newList ->
        { currentModel with CardState = None; FlashCardData = newList}, Cmd.none
    | _, SetLessons lessons ->
      let newLessons = lessons |> List.map (fun x -> { Lesson = x; Selected = true; })
      { currentModel with Lessons = newLessons }, getAll lessons
    | _, ToggleLessonSelection lesson ->
      let newLessons = currentModel.Lessons |> List.map (fun x -> { x with Selected = if x.Lesson = lesson then not x.Selected else x.Selected; })
      let cmd =
        let lessonsToGet = (newLessons |> List.filter (fun x -> x.Selected) |> List.map (fun x -> x.Lesson))
        getAll lessonsToGet
      { currentModel with Lessons = newLessons }, cmd
    | _, ToggleAllLessonSelection ->
      let countSelected = currentModel.Lessons |> List.filter (fun x -> x.Selected) |> (fun x -> x.Length)
      let newLessons =
        if countSelected = currentModel.Lessons.Length
        then currentModel.Lessons |> List.map (fun x -> { x with Selected = false; })
        else currentModel.Lessons |> List.map (fun x -> { x with Selected = true; })
        
      let cmd =
        let lessonsToGet = (newLessons |> List.filter (fun x -> x.Selected) |> List.map (fun x -> x.Lesson))
        getAll lessonsToGet
      { currentModel with Lessons = newLessons }, cmd

let show cardState = 
  match cardState with
  | Some x when x.ShowAnswer -> "Click for next card"
  | Some x when not x.ShowAnswer -> "Click to reveal answer"
  | None -> ""
  | _ -> ""

let latinTextToElements items =
  items
  |> List.map (fun s ->
    match s with
    | LatinText.Normal v -> str v
    | LatinText.Macron v -> span [(*(DangerouslySetInnerHTML { __html = "&" + v.ToString() + "macr;" }) :> IHTMLProp*)] []
  )
  
let showCard practiceDirection x (dispatch : Msg -> unit) =
  let (question, answer) = 
    match practiceDirection with
    | PracticeDirection.Forwards -> (x.FlashCard.Front.Question, x.FlashCard.Back.Answer)
    | PracticeDirection.Backwards -> (x.FlashCard.Back.Answer, x.FlashCard.Front.Question)
  div []
    [ match x.ShowAnswer with
      | true ->
          Hero.hero [ Hero.Color IsInfo; Hero.IsHalfHeight; ] [
            Hero.head [][
              str (sprintf "Lesson %i" x.FlashCard.Lesson)
            ]
            
            Hero.body [ Props[ Fable.React.Props.OnClick (fun _ -> dispatch ShowNextCard);] ][
              Container.container [][
                Columns.columns [ Columns.IsCentered; Columns.IsMultiline; ]
                  [ 
                    Column.column [ 
                      Column.Width(Screen.All, Column.IsFull); 
                    ] [ 
                      Heading.h1 [ Heading.Modifiers[ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]][str answer]
                    ]

                  ]
              ] 
            ]
            Hero.foot [][
            ]
          ]
          
      | false ->
          Hero.hero [ Hero.Color IsPrimary; Hero.IsHalfHeight; ] [
            Hero.head [][str " "]
            Hero.body [ Props[ Fable.React.Props.OnClick (fun _ -> dispatch ShowAnswer);] ][
              Container.container [][
                Heading.h1 [ Heading.Modifiers[ Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]][yield str question] ] ]
            Hero.foot [][str " "] ] ]

let showCardList cards (dispatch : Msg -> unit) =
  div []
    [ 
      Columns.columns [] [
        Column.column [Column.Width(Screen.All, Column.Is5);] [ Text.span [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ] [ str "Latin"] ]
        Column.column [Column.Width(Screen.All, Column.Is5);] [ Text.span [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ] [ str "English" ] ]
        Column.column [Column.Width(Screen.All, Column.Is2);] [ Text.span [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ] [ str "Lesson" ] ]
      ]
      for card in cards do
        Columns.columns [] [
          Column.column [Column.Width(Screen.All, Column.Is5);] [str card.Front.Question]
          Column.column [Column.Width(Screen.All, Column.Is5);] [str card.Back.Answer]
          Column.column [Column.Width(Screen.All, Column.Is2);] [str ( sprintf "%i" card.Lesson )]
        ]
    ]

let containerBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        Card.content [ ] [  
        match model.CardState with
        | None -> ()
        | Some _ ->   
          Columns.columns [] [
            Button.button [ Button.OnClick (fun _ -> dispatch ShowList)] 
              [ str "View full list" ] ]      
        Columns.columns [] [
          Column.column [] [ 
            str (show model.CardState)
            match model.CardState with
            | Some v -> showCard model.PracticeDirection v dispatch
            | None -> showCardList model.FlashCardData dispatch
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
              Column.column [] 
                [
                  match model.CardState with
                  | Some _ -> ()
                  | None ->
                    Heading.p [ ] [ str "Lessons" ]
                    text [] [ str "Select one or more lessons to practice"]
                    div [ ClassName "block" ]
                      [ Button.list [ ]
                          [ for l in model.Lessons do
                              Button.span 
                                [ Button.Color (if l.Selected then IsPrimary else IsGreyDark)
                                  Button.OnClick (fun _ -> ToggleLessonSelection l.Lesson |> dispatch) ] 
                                [ str (sprintf "%i" l.Lesson) ]
                            Button.span 
                              [ Button.Color (if model.Lessons |> List.exists (fun x -> not x.Selected) then IsGreyDark else IsPrimary)
                                Button.OnClick (fun _ -> ToggleAllLessonSelection |> dispatch) ] 
                              [ str "All" ] ] ]
                    if model.FlashCardData.Length > 0 then
                      div []
                        [ Heading.p [ ] [ str "Practice" ]
                          match model.CardState with
                          | Some _ -> ()
                          | None ->
                            Button.list [] 
                              [ Button.span [Button.Color IsPrimary; Button.OnClick (fun _ -> Practice PracticeDirection.Forwards |> dispatch)] 
                                  [ str "Latin to English" ]
                                Button.span [Button.Color IsPrimary; Button.OnClick (fun _ -> Practice PracticeDirection.Backwards |> dispatch)] 
                                  [ str "English to Latin" ] ] ] ]
              if model.FlashCardData.Length = 0 
              then ()
              else
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    containerBox model dispatch
                ]
          ]
      ]
  ]