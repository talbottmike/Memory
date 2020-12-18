module Client.FlashCardPractice

open Feliz

type Greeting = { Name: string option }

let greeting = React.functionComponent("Greeting", fun (props: {| greeting : Greeting; |}) ->
  Html.div [
    Html.span "Hello, "
    Html.span (Option.defaultValue "World" props.greeting.Name)
  ])