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

  module Editor =
    type Model = { User : AppUser option; EntryId : Guid option; Text : string; Title : string; } 
    type Msg =
      | UpdateText of string
      | UpdateTitle of string
      | AddOrUpdateEntry
      | SaveEntry of MemorizationEntryDisplay
      | SaveSampleEntry of MemorizationEntry
      | SavedEntry
      | CancelEntry
      | RemoveEntry of Guid
      | RemovedEntry
      | EntryAddedToDatabase
      | EntryRemovedFromDatabase
    type Props = 
      { Model: Model
        Dispatch: Msg -> unit }

  module Entries =
    type Model = { User : AppUser option; Entries : MemorizationEntryDisplay list; }
    type Msg =
      | SelectEntry of Guid
      | AddEntry
      | UpdateEntry of Guid
      | BrowserEntriesLoaded of MemorizationEntryDisplay list
      | EntriesLoaded of MemorizationEntry list
      | SavedEntries
    type Props = 
      { Model: Model
        Dispatch: Msg -> unit }

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

    type FlashCardData = {Lesson:string; Front:FlashCardFront; Back:FlashCardBack; }
    type FlashCardDatas = { Data : FlashCardData list }
    type IFlashCardsApi =
        { getAll : string list -> Async<FlashCardData list>
          get : string list -> Async<FlashCardData>
          getLessons : unit -> Async<string list> }

    type FlashCard = { FlashCard: FlashCardData; ShowAnswer:bool; }
    type CardState =
      | FlashCard of FlashCard
      | FullList of FlashCardData list

    type LessonSelection =
      { Lesson : string
        Selected : bool }

    type Model = 
      { Lessons : LessonSelection list
        State : CardState }

    type Msg =
      | ShowAnswer
      | SetCard of FlashCardData
      | FetchQuestion
      | SetList of FlashCardData list
      | SetLessons of string list
      | ToggleLessonSelection of string
      | ToggleList

  type PageModel =
    | EditorModel of Editor.Model
    | EntriesModel of Entries.Model
    | HomeModel of Home.Model
    | PracticeModel of Practice.Model
    | FlashCardsModel of FlashCards.Model
      
  type Model = 
    { MenuModel : Menu.Model
      IsLoading : bool
      PageModel : PageModel }

  /// The composed set of messages that update the state of the application
  type Msg =
    | EditorMsg of Editor.Msg
    | EntriesMsg of Entries.Msg
    | HomeMsg of Home.Msg
    | PracticeMsg of Practice.Msg
    | FlashCardsMsg of FlashCards.Msg
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
   
