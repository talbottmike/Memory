namespace Shared
open System
module Domain =
  type TextType = | Word | Punctuation | Number
  type TextView = | FullText | Letters of int | NoText 
  type TextPart = { Id : int; Text : string; TextType : TextType; TextView : TextView; HasSpaceBefore : bool; }
  type MemorizationEntryDisplay = { Id : Guid; Title : string; Text : string; TextParts : TextPart list; HintLevel : int option; }
  type MemorizationEntry = { Id : Guid; Title : string; Text : string; }
  type EditorValues = { EntryId : Guid option; Title : string; Text : string; }
  type GoogleUser = { Token : string; Id : string; Name : string; Email : string; }
  type UserRole = | Admin | Subscriber
  type TokenResult = { Token : string; Role : UserRole option; }
  type UserProvider =
    | Google of GoogleUser
    | Sample
  type AppUser = { Provider : UserProvider; Role : UserRole option; MemoriaToken : string option; }
  type TextViewRequest = { Id : int; TextView : TextView; }
  type GoogleLoginRequest = { IdToken : string }
  type StorageUser = { EmailAddress : string; Role : UserRole option; Entries : MemorizationEntry list; }

  module Menu =
    type Model = 
      { User : AppUser option
        IsBurgerOpen : bool }
    type Props = 
      { Model : Model
        OnLogout : unit -> unit
        OnToggleBurger: unit -> unit }

  module Home =
    type Model = 
      { User : AppUser option; } 
        static member Empty : Model = { User = None }
    type Msg =
      | ToRemove
    type Props = 
      { Model: Model
        Dispatch: Msg -> unit }

  module Practice =
    type Model = { User : AppUser option; CurrentEntry : MemorizationEntryDisplay option; } 
    type Msg =
      | ToggleTextView of TextViewRequest
      | ViewList
      | BulkToggleTextView of TextView
      | SavedEntryPracticeState
      | SampleEntryFetchResult of MemorizationEntry option
    type Props = 
      { Model: Model
        Dispatch: Msg -> unit }

  module FlashCards =
    type LatinText =
      | Normal of string
      | Macron of char

    type FlashCardFront = { Question: string }
    type Gender = | Feminine | Masculine | Neuter
    type Declension = | First | Second | Third | Fourth | Fifth
    type Conjugation = | First | Second | Third | Fourth
    type EndingType = 
      | NotApplicable
      | Conjugation of Conjugation
      | Declension of Declension

    type FlashCardBack = {Answer : string; }

    type FlashCardData = {Lesson:int; Front:FlashCardFront; Back:FlashCardBack; }
    type FlashCardDatas = { Data : FlashCardData list }
    type IFlashCardsApi =
        { getAll : int list -> Async<FlashCardData list>
          get : int list -> Async<FlashCardData>
          getLessons : unit -> Async<int list> }

    type PracticeDirection = Forwards | Backwards
    type FlashCard = { FlashCard: FlashCardData; ShowAnswer:bool; }

    type LessonSelection =
      { Lesson : int
        Selected : bool }

    type Model = 
      { Lessons : LessonSelection list
        FlashCardData : FlashCardData list
        CardState : FlashCard option
        PracticeDirection : PracticeDirection }

    type Msg =
      | ShowAnswer
      | SetCard of FlashCardData
      | ShowNextCard
      | SetList of FlashCardData list
      | SetLessons of int list
      | ToggleLessonSelection of int
      | ToggleAllLessonSelection
      | ShowList
      | Practice of PracticeDirection

  type PageModel =
    | EditorModel of Guid option
    | EntriesModel
    | HomeModel// of Home.Model
    | PracticeModel of Guid option
    | FlashCardsModel
      
  type Model = 
    { MenuModel : Menu.Model
      IsLoading : bool
      Entries : MemorizationEntryDisplay list
      PageModel : PageModel }

  /// The composed set of messages that update the state of the application
  type Msg =
    // | EntriesMsg of Entries.Msg
    // | HomeMsg of Home.Msg
    // | PracticeMsg of Practice.Msg
    | DemoteUser
    | SignedIn of UserProvider
    | SignedOut
    | LoggedOut of unit
    | AuthDisconnected
    // | StorageFailure of exn
    | TokenReceived of TokenResult
    | MenuBurgerToggled of unit
    | AuthConfigured

module Helpers =
  open Domain
  open System.Text.RegularExpressions
  let (|FirstRegexGroup|_|) pattern input =
    let m = Regex.Match(input,pattern) 
    if (m.Success) then Some m.Groups.[1].Value else None  
  let identifyTextType id (s:string) =
    match s with
    | FirstRegexGroup "[\d]" v -> { Id = id; Text = s; TextType = TextType.Number; TextView = FullText; HasSpaceBefore = false; }
    | FirstRegexGroup "[^\w\s']" v -> { Id = id; Text = s; TextType = TextType.Punctuation; TextView = FullText; HasSpaceBefore = false; }
    | _ -> { Id = id; Text = s; TextType = TextType.Word; TextView = FullText; HasSpaceBefore = false; }
  let getTextParts (t : string) = 
    Regex.Split(t, @"(\b[^\s]+\b)")
    |> Seq.mapi identifyTextType
    |> Seq.pairwise
    |> Seq.collect (fun (x,y) -> 
      [ (if x.Text = " " then { y with HasSpaceBefore = true; } else y) ]
    )
    |> Seq.filter (fun x -> x.Text.Trim() <> "")
    |> Seq.toList
    
  let strOption (s : string) = if String.IsNullOrWhiteSpace s then None else Some s
  let incrementTextView (x : TextPart) =
    match x.TextView with
    | TextView.FullText -> TextView.NoText
    | TextView.NoText -> TextView.Letters 1
    | TextView.Letters v -> 
      if x.Text.Length <= v then TextView.Letters (v + 1) else TextView.FullText
  let toggleTextView (x : TextPart) =
    match x.TextView with
    | TextView.FullText -> TextView.NoText
    | TextView.NoText -> TextView.Letters 1
    | TextView.Letters _ -> TextView.FullText
  let toDisplayModel (m : MemorizationEntry) =
    { MemorizationEntryDisplay.Id = m.Id; Title = m.Title; Text = m.Text; TextParts = getTextParts m.Text; HintLevel = None; }
   
