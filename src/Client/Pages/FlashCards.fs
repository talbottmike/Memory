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
    let initialModel = { Lessons = allLessons; FlashCardData = []; PracticeDirection = PracticeDirection.Forwards; CardState = None }
    initialModel, Cmd.none

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
    | _, Practice practiceDirection ->
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
  // let classes = Styles.useStyles()
  let (question, answer) = 
    match practiceDirection with
    | PracticeDirection.Forwards -> (x.FlashCard.Front.Question, x.FlashCard.Back.Answer)
    | PracticeDirection.Backwards -> (x.FlashCard.Back.Answer, x.FlashCard.Front.Question)
  Html.div [
    prop.children [
      match x.ShowAnswer with
      | true ->
        Mui.card [
          // prop.className classes.flashcardBack
          prop.onClick (fun _ -> dispatch ShowNextCard)
          prop.style [
            // style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            // style.backgroundPosition.fixedNoScroll
            // style.backgroundRepeat.noRepeat
            //style.custom ("linear-gradient","(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5))")
            // style.custom ("background-attachment", "fixed")
            style.custom ("width","70vw")
            style.custom ("height","70vh")
            style.custom ("background",sprintf """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://source.unsplash.com/featured/?chicken,%s") no-repeat center center fixed""" (System.Guid.NewGuid().ToString()))
            style.backgroundSize.cover
            style.color.white
          ]
          prop.children [
            Mui.cardHeader [
              cardHeader.title (sprintf "Lesson %i" x.FlashCard.Lesson)
            ]
            Mui.cardActionArea [
              prop.style [
                style.custom ("width","100%")
                style.custom ("height","100%")
              ]
              cardActionArea.children [
                Mui.cardContent [
                  prop.children [
                    Mui.typography [
                      typography.variant.h1 
                      typography.align.center
                      typography.children answer
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      | false ->
        Mui.card [
          prop.onClick (fun _ -> dispatch ShowAnswer)
          // prop.className classes.flashcardFront
          prop.style [
            style.custom ("width","70vw")
            style.custom ("height","70vh")
            style.custom ("background",sprintf """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://source.unsplash.com/featured/?chicken,%s") no-repeat center center fixed""" (System.Guid.NewGuid().ToString()))
            style.backgroundSize.cover
            style.color.white
          ]
          prop.children [
            Mui.cardHeader [
              cardHeader.title (sprintf "Lesson %i" x.FlashCard.Lesson)
            ]
            Mui.cardActionArea [
              prop.style [
                style.custom ("width","100%")
                style.custom ("height","100%")
              ]
              cardActionArea.children [
                Mui.cardContent [
                  prop.children [
                    Mui.typography [
                      typography.variant.h1 
                      typography.align.center
                      typography.children question
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
    ]
  ]

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

let containerBox (model : Model) (dispatch : Msg -> unit) =
    Html.div [
      prop.children [
        match model.CardState with
        | None -> ()
        | Some _ ->  
          Mui.button [
            prop.type'.submit
            button.color.primary
            button.variant.contained
            button.color.primary
            button.children "View full list"
            prop.onClick (fun _ -> dispatch ShowList)
          ]
        Mui.typography [
          typography.children (show model.CardState)
        ] 
        match model.CardState with
        | Some v -> showCard model.PracticeDirection v dispatch
        | None -> showCardList model.FlashCardData dispatch
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
          match model.CardState with
          | Some _ -> ()
          | None ->
            Client.FlashCardDictionary.view {| flashCards = Client.FlashCardInfo.allFlashCards |}
          //   Mui.grid [
          //     grid.container true
          //     grid.spacing._1
          //     grid.justify.flexStart
          //     grid.alignItems.center
          //     grid.children [
          //       Mui.grid [
          //         grid.item true
          //         grid.children [ 
          //           Mui.typography [ 
          //             typography.variant.h5
          //             typography.children "Search"
          //           ]
          //         ]
          //       ]
          //     ]
          //   ]
          //   Mui.grid [
          //     grid.container true
          //     grid.spacing._1
          //     grid.justify.flexStart
          //     grid.alignItems.center
          //     grid.children [
          //       Mui.grid [
          //         grid.item true
          //         grid.children [ 
          //           Mui.typography [ 
          //             typography.children "Search to narrow selection"
          //           ]
          //         ]
          //       ]
          //     ]
          //   ]
          //   Mui.grid [
          //     grid.container true
          //     grid.spacing._1
          //     grid.justify.flexStart
          //     grid.alignItems.center
          //     grid.children [
          //       Mui.grid [
          //         grid.item true
          //         grid.children [ 
          //           Mui.textField [
          //             textField.value ""
          //             // textField.onChange (UpdateTitle >> dispatch)
          //             textField.variant.outlined
          //             textField.margin.normal
          //             textField.required false
          //             textField.fullWidth true
          //             textField.id "Search"
          //             textField.label "Search"
          //             textField.name "search"
          //             textField.autoComplete "search"
          //             textField.autoFocus true
          //             textField.placeholder "Search for cards containing word(s)"
          //           ]
          //         ]
          //       ]
          //     ]
          //   ]
          //   Mui.grid [
          //     grid.container true
          //     grid.spacing._1
          //     grid.justify.flexStart
          //     grid.alignItems.center
          //     grid.children [
          //       for l in model.Lessons do
          //         Mui.grid [
          //           grid.item true
          //           grid.children [
          //             Mui.button [
          //               prop.type'.submit
          //               if l.Selected then button.color.primary else button.color.default'
          //               button.variant.contained
          //               button.children (sprintf "%i" l.Lesson)
          //               prop.onClick (fun _ -> ToggleLessonSelection l.Lesson |> dispatch)
          //             ]
          //           ]
          //         ]
          //       Mui.grid [
          //         grid.item true
          //         grid.children [
          //           Mui.button [
          //             prop.type'.submit
          //             if model.Lessons |> List.exists (fun x -> not x.Selected) then button.color.default' else button.color.primary
          //             button.variant.contained
          //             button.children "All"
          //             prop.onClick (fun _ -> ToggleAllLessonSelection |> dispatch)
          //           ]
          //         ]
          //       ]
          //     ]
          //   ]

          // if model.FlashCardData.Length > 0 then
          //   Html.div [
          //     match model.CardState with
          //     | Some _ -> ()
          //     | None ->
          //       Mui.grid [
          //         grid.container true
          //         grid.spacing._1
          //         grid.justify.flexStart
          //         grid.alignItems.center
          //         grid.children [
          //           Mui.grid [
          //             grid.item true
          //             grid.children [ 
          //               Mui.typography [ 
          //                 typography.variant.h5
          //                 typography.children "Practice"
          //               ]
          //             ]
          //           ]
          //         ]
          //       ]
          //       Mui.grid [
          //         grid.container true
          //         grid.spacing._1
          //         grid.justify.flexStart
          //         grid.alignItems.center
          //         grid.children [
          //           Mui.grid [
          //             grid.item true
          //             grid.children [
          //               Mui.button [
          //                 prop.type'.submit
          //                 button.color.primary
          //                 button.variant.contained
          //                 button.color.primary
          //                 button.children "Latin to English"
          //                 prop.onClick (fun _ -> Practice PracticeDirection.Forwards |> dispatch)
          //               ]
          //             ]
          //           ]
          //           Mui.grid [
          //             grid.item true
          //             grid.children [
          //               Mui.button [
          //                 prop.type'.submit
          //                 button.color.primary
          //                 button.variant.contained
          //                 button.color.primary
          //                 button.children "English to Latin"
          //                 prop.onClick (fun _ -> Practice PracticeDirection.Backwards |> dispatch)
          //               ]
          //             ]
          //           ]
          //         ]
          //       ]
          //   ]
          // if model.FlashCardData.Length = 0 
          // then ()
          // else containerBox model dispatch
        ]
      ]
    ]
  ]
)