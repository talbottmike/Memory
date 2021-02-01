module Client.FlashCardDictionary

open Client
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

type private RowData =
  { latin: string
    english: string
    lesson: string
    lessonNumber: int
    FlashCardData : FlashCardData }

let view = React.functionComponent("FlashCardDictionary", fun (input: {| flashCards : FlashCardData list; finishCallBack : unit -> unit; practiceCallBack : FlashCardPractice.InitialState -> unit; |}) ->
  let allLessons = 
    input.flashCards 
    |> List.map (fun x -> { Lesson = x.Lesson; Selected = false; }) 
    |> List.distinct
    |> List.sortBy (fun x -> x.Lesson)

  Mui.materialTable [
    materialTable.title (
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
                  typography.variant.h6
                  typography.children "Flashcard dictionary"
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
                  typography.variant.body1
                  typography.children "To practice, select rows and then click the play button."
                ]
              ]
            ]
          ]
        ]
      ]
    )
    materialTable.columns [
      columns.column [
        column.title "Latin"
        column.field<RowData> (fun rd -> nameof rd.latin)
        // column.customFilterAndSearch<RowData> (fun term rowData _ -> match term with | Int i -> i = rowData.name.Length | _ -> false)
      ]
      columns.column [
        column.title "English"
        column.field<RowData> (fun rd -> nameof rd.english)
      ]
      columns.column [
        column.title "Lesson"
        column.field<RowData> (fun rd -> nameof rd.lesson)
        // column.filtering false
        column.lookup<string,string> (allLessons |> List.map (fun x -> (sprintf "%i" x.Lesson,sprintf "%i" x.Lesson)))
      ]
    ]
    materialTable.data (Client.FlashCardInfo.allFlashCards |> List.map (fun x -> { english = x.Back.Answer; latin = x.Front.Question; lesson = sprintf "%i" x.Lesson; lessonNumber = x.Lesson; FlashCardData = x; }))
    materialTable.actions [
        actions.action [
            action.icon (Mui.icon [ playCircleFilledIcon [] ])
            action.tooltip "Practice Latin to English"
            //action.isFreeAction true            
            action.onClick<U2<RowData,ResizeArray<RowData>>> (fun _ x -> 
                match x with
                | U2.Case1 a -> ()
                | U2.Case2 b -> input.practiceCallBack { FlashCardPractice.InitialState.FlashCardData = b |> Seq.map (fun x -> x.FlashCardData) |> Seq.toList; PracticeDirection = PracticeDirection.Forwards }
              )
        ]
        actions.action [
            action.icon (Mui.icon [ playCircleFilledIcon [] ])
            action.tooltip "Practice English to Latin"
            //action.isFreeAction true            
            action.onClick<U2<RowData,ResizeArray<RowData>>> (fun _ x -> 
                match x with
                | U2.Case1 a -> ()
                | U2.Case2 b -> input.practiceCallBack { FlashCardPractice.InitialState.FlashCardData = b |> Seq.map (fun x -> x.FlashCardData) |> Seq.toList; PracticeDirection = PracticeDirection.Backwards }
              )
        ]
        // actions.action [
        //     action.icon (Mui.icon [ saveIcon [] ])
        //     action.tooltip "Save User"
        //     action.onClick<RowData> (fun _ rowData -> input.dispatch (SaveRow rowData.name))
        // ]
    ]
        
    materialTable.options [
        options.filtering true
        options.selection true
        options.pageSize 20
    ]
  ]
)