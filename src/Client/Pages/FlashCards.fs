module Client.FlashCards

open Elmish
open Shared
open Shared.Domain.FlashCards
open Fable.React
open Feliz
open Feliz.UseElmish
open Feliz.MaterialUI
open Feliz.MaterialUI.MaterialTable
open Fable.Core.Experimental
open Fable.Core
open Fable.MaterialUI.Icons

type FlashCardMode =
  | SelectionMode
  | DictionaryMode
  | PracticeMode of FlashCardPractice.InitialState

type Model = 
  { Lessons : LessonSelection list
    FlashCardData : FlashCardData list
    Mode : FlashCardMode }

type Msg =
  | SetList of FlashCardData list
  | SetLessons of int list
  | ToggleLessonSelection of int
  | ToggleAllLessonSelection
  | ShowList
  | ReInitialize
  | Practice of FlashCardPractice.InitialState
  
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

let getAll lessons =
  Cmd.OfAsync.perform flashCardsApi.getAll lessons SetList

let init () : Model * Cmd<Msg> =
    let initialModel = { Mode = SelectionMode; Lessons = allLessons; FlashCardData = []; }
    initialModel, Cmd.none

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    Fable.Core.JS.console.log msg
    Fable.Core.JS.console.log currentModel
    let selectedLessons = currentModel.Lessons |> List.filter (fun x -> x.Selected) |> List.map (fun x -> x.Lesson)
    match msg with    
    | ReInitialize ->
      init ()
    | ShowList ->
      { currentModel with Mode = FlashCardMode.DictionaryMode }, Cmd.none
    | Practice v ->
      { currentModel with Mode = FlashCardMode.PracticeMode v; }, Cmd.none
    | SetList newList ->
        { currentModel with FlashCardData = newList}, Cmd.none
    | SetLessons lessons ->
      let newLessons = lessons |> List.map (fun x -> { Lesson = x; Selected = true; })
      { currentModel with Lessons = newLessons }, getAll lessons
    | ToggleLessonSelection lesson ->
      let newLessons = currentModel.Lessons |> List.map (fun x -> { x with Selected = if x.Lesson = lesson then not x.Selected else x.Selected; })
      let cmd =
        let lessonsToGet = (newLessons |> List.filter (fun x -> x.Selected) |> List.map (fun x -> x.Lesson))
        getAll lessonsToGet
      { currentModel with Lessons = newLessons }, cmd
    | ToggleAllLessonSelection ->
      let countSelected = currentModel.Lessons |> List.filter (fun x -> x.Selected) |> (fun x -> x.Length)
      let newLessons =
        if countSelected = currentModel.Lessons.Length
        then currentModel.Lessons |> List.map (fun x -> { x with Selected = false; })
        else currentModel.Lessons |> List.map (fun x -> { x with Selected = true; })
        
      let cmd =
        let lessonsToGet = (newLessons |> List.filter (fun x -> x.Selected) |> List.map (fun x -> x.Lesson))
        getAll lessonsToGet
      { currentModel with Lessons = newLessons }, cmd

let latinTextToElements items =
  items
  |> List.map (fun s ->
    match s with
    | LatinText.Normal v -> str v
    | LatinText.Macron v -> span [(*(DangerouslySetInnerHTML { __html = "&" + v.ToString() + "macr;" }) :> IHTMLProp*)] []
  )
  
let showCardList cards (dispatch : Msg -> unit) =
  Mui.container [
    container.children [
      Mui.grid [
        grid.container true
        grid.spacing._1
        // grid.justify.flexStart
        // grid.alignItems.center
        grid.children [
          Mui.grid [
            grid.item true
            grid.xs._5
            grid.children [ 
              Mui.typography [
                typography.variant.h5
                typography.children "Latin"
              ]
            ]
          ]
          Mui.grid [
            grid.item true
            grid.xs._5
            grid.children [ 
              Mui.typography [
                typography.variant.h5
                typography.children "English"
              ]
            ]
          ]
          Mui.grid [
            grid.item true
            grid.xs._2
            grid.children [ 
              Mui.typography [
                typography.variant.h5
                typography.children "Lesson"
              ]
            ]
          ]
        ]
      ]
      for card in cards do
        Mui.grid [
          grid.container true
          grid.spacing._1
          // grid.justify.flexStart
          // grid.alignItems.center
          grid.children [
            Mui.grid [
              grid.item true
              grid.xs._5
              grid.children [ 
                Mui.typography [
                  typography.variant.h6
                  typography.children card.Front.Question
                ]
              ]
            ]
            Mui.grid [
              grid.item true
              grid.xs._5
              grid.children [ 
                Mui.typography [
                  typography.variant.h6
                  typography.children card.Back.Answer
                ]
              ]
            ]
            Mui.grid [
              grid.item true
              grid.xs._2
              grid.children [ 
                Mui.typography [
                  typography.variant.h6
                  typography.children ( sprintf "%i" card.Lesson )
                ]
              ]
            ]
          ]
        ]
    ]
  ]

let view = React.functionComponent (fun (input: {| abandonComponent: unit -> unit; |}) ->
  let model, dispatch = React.useElmish(init (), update, [| |])
  Html.div [
    // prop.style [
    //   // style.backgroundImageUrl "https://unsplash.it/1200/900?random"
    //   // style.backgroundPosition.fixedNoScroll
    //   // style.backgroundRepeat.noRepeat
      

    //   //style.custom ("linear-gradient","(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5))")
    //   // style.custom ("background-attachment", "fixed")
    //   style.custom ("background","""linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed""")
    //   style.backgroundSize.cover
    //   style.color.white
    // ]
    prop.children [
      Mui.container [
        container.children [
          match model.Mode with
          | SelectionMode ->
            Mui.grid [
              grid.container true
              grid.spacing._1
              grid.justify.flexStart
              grid.alignItems.center
              grid.children [
                Mui.grid [
                  grid.item true
                  grid.children [ 
                    Mui.typography [ 
                      typography.variant.h5
                      typography.children "Dictionary"
                    ]
                  ]
                ]
              ]
            ]
            Mui.grid [
              grid.container true
              grid.spacing._1
              grid.justify.flexStart
              grid.alignItems.center
              grid.children [
                Mui.grid [
                  grid.item true
                  grid.children [ 
                    Mui.typography [ 
                      typography.children "Select to view dictionary"
                    ]
                  ]
                ]
              ]
            ]
            Mui.grid [
              grid.container true
              grid.spacing._1
              grid.justify.flexStart
              grid.alignItems.center
              grid.children [
                Mui.grid [
                  grid.item true
                  grid.children [
                    Mui.button [
                      prop.type'.submit
                      button.color.default'
                      button.variant.contained
                      button.children ("View dictionary")
                      prop.onClick (fun _ -> ShowList |> dispatch)
                    ]
                  ]
                ]
              ]
            ]
            Mui.grid [
              grid.container true
              grid.spacing._1
              grid.justify.flexStart
              grid.alignItems.center
              grid.children [
                Mui.grid [
                  grid.item true
                  grid.children [ 
                    Mui.typography [ 
                      typography.variant.h5
                      typography.children "Lessons"
                    ]
                  ]
                ]
              ]
            ]
            Mui.grid [
              grid.container true
              grid.spacing._1
              grid.justify.flexStart
              grid.alignItems.center
              grid.children [
                Mui.grid [
                  grid.item true
                  grid.children [ 
                    Mui.typography [ 
                      typography.children "Select one or more lessons to practice"
                    ]
                  ]
                ]
              ]
            ]
            Mui.grid [
              grid.container true
              grid.spacing._1
              grid.justify.flexStart
              grid.alignItems.center
              grid.children [
                for l in model.Lessons do
                  Mui.grid [
                    grid.item true
                    grid.children [
                      Mui.button [
                        prop.type'.submit
                        if l.Selected then button.color.primary else button.color.default'
                        button.variant.contained
                        button.children (sprintf "%i" l.Lesson)
                        prop.onClick (fun _ -> ToggleLessonSelection l.Lesson |> dispatch)
                      ]
                    ]
                  ]
                Mui.grid [
                  grid.item true
                  grid.children [
                    Mui.button [
                      prop.type'.submit
                      if model.Lessons |> List.exists (fun x -> not x.Selected) then button.color.default' else button.color.primary
                      button.variant.contained
                      button.children "All"
                      prop.onClick (fun _ -> ToggleAllLessonSelection |> dispatch)
                    ]
                  ]
                ]
              ]
            ]
            let flashCardsForSelectedLessons = model.FlashCardData |> List.filter (fun x -> model.Lessons |> List.filter (fun l -> l.Selected) |> List.exists (fun l -> l.Lesson = x.Lesson))
            match flashCardsForSelectedLessons with
            | [] -> ()
            | _ ->
              Html.div [
                Mui.grid [
                  grid.container true
                  grid.spacing._1
                  grid.justify.flexStart
                  grid.alignItems.center
                  grid.children [
                    Mui.grid [
                      grid.item true
                      grid.children [ 
                        Mui.typography [ 
                          typography.variant.h5
                          typography.children "Practice"
                        ]
                      ]
                    ]
                  ]
                ]
                Mui.grid [
                  grid.container true
                  grid.spacing._1
                  grid.justify.flexStart
                  grid.alignItems.center
                  grid.children [
                    Mui.grid [
                      grid.item true
                      grid.children [
                        Mui.button [
                          prop.type'.submit
                          button.color.primary
                          button.variant.contained
                          button.color.primary
                          button.children "Latin to English"
                          prop.onClick (fun _ -> Practice { FlashCardData = flashCardsForSelectedLessons; PracticeDirection = PracticeDirection.Forwards; } |> dispatch)
                        ]
                      ]
                    ]
                    Mui.grid [
                      grid.item true
                      grid.children [
                        Mui.button [
                          prop.type'.submit
                          button.color.primary
                          button.variant.contained
                          button.color.primary
                          button.children "English to Latin"
                          prop.onClick (fun _ -> Practice { FlashCardData = flashCardsForSelectedLessons; PracticeDirection = PracticeDirection.Backwards; } |> dispatch)
                        ]
                      ]
                    ]
                  ]
                ]
              ]

          | DictionaryMode ->
            Client.FlashCardDictionary.view {| flashCards = Client.FlashCardInfo.allFlashCards; finishCallBack = (fun () -> dispatch ReInitialize); practiceCallBack = (fun x -> dispatch (Practice x)) |}
          | PracticeMode v ->
            Client.FlashCardPractice.view {| initialState = v; finishPracticeCallBack = (fun () -> dispatch ReInitialize) |}

        ]
      ]
    ]
  ]
)