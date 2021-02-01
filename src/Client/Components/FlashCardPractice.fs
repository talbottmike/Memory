module Client.FlashCardPractice

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

type InitialState = 
  { FlashCardData : FlashCardData list
    PracticeDirection : PracticeDirection }

type Model = 
  { FlashCardData : FlashCardData list
    CardState : FlashCard option
    PracticeDirection : PracticeDirection }

type Msg =
  | ShowAnswer
  | SetCard of FlashCardData
  | ShowNextCard

let init (initialState : InitialState): Model * Cmd<Msg> =
  let initialModel = { FlashCardData = initialState.FlashCardData; PracticeDirection = initialState.PracticeDirection; CardState = None }
  let cmd = initialState.FlashCardData |> Seq.tryHead |> Option.map (SetCard >> Cmd.ofMsg) |> Option.defaultValue Cmd.none
  initialModel, cmd

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with    
    | ShowAnswer ->
      let nextModel = currentModel.CardState |> Option.map (fun x -> { x with ShowAnswer = true })
      { currentModel with CardState = nextModel}, Cmd.none
    | SetCard cardInfo ->
      let nextModel = { FlashCard = cardInfo; ShowAnswer = false; }
      { currentModel with CardState = Some nextModel}, Cmd.none
    | ShowNextCard ->
      let selectedCardIndex = 
        currentModel.CardState |> Option.bind (fun selectedCard -> currentModel.FlashCardData |> List.tryFindIndex (fun x -> x = selectedCard.FlashCard))
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
        
let show cardState = 
  match cardState with
  | Some x when x.ShowAnswer -> "Click card for next card"
  | Some x when not x.ShowAnswer -> "Click card to reveal answer"
  | None -> ""
  | _ -> ""

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
                      typography.variant.h2
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
                      typography.variant.h2
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

let view = React.functionComponent("FlashCardDictionary", fun (input: {| initialState : InitialState; finishPracticeCallBack : unit -> unit; |}) -> 
  let model, dispatch = React.useElmish(init input.initialState, update, [| |])
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
          button.children "Done"
          prop.onClick (fun _ -> input.finishPracticeCallBack ())
        ]
      Mui.typography [
        typography.children (show model.CardState)
      ] 
      match model.CardState with
      | Some v -> showCard model.PracticeDirection v dispatch
      | None -> ()
    ]
  ]
)