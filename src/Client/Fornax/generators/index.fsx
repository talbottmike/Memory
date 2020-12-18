#r "../_lib/Fornax.Core.dll"
#load "layout.fsx"

open Html

let generate' (ctx : SiteContents) (_: string) =
  let posts = ctx.TryGetValues<Postloader.Post> () |> Option.defaultValue Seq.empty
  let siteInfo = ctx.TryGetValue<Globalloader.SiteInfo> ()
  let desc =
    siteInfo
    |> Option.map (fun si -> si.description)
    |> Option.defaultValue ""

  let psts =
    posts
    |> Seq.sortByDescending Layout.published
    |> Seq.toList
    |> List.map (Layout.postLayout true)

  Layout.layout ctx "Home" [
    div [] [
      nav [Class "navbar is-primary"] [
        div [Class "navbar-brand"] [
          a [Class "navbar-item"; Href "/"] [ img [ Model.HtmlProperties.Style [ CSSProperties.Width "2.5em" ]; Src "/shape.svg"; Alt "Logo"] ]
          div [Class "navbar-item"; ] [ h2 [Class "title is-2" ] [ string "memoria" ] ]
        ]
        //div [Id "navbarMenu"; Class "navbar-menu"] menuEntries
      ]
      section [Class "hero is-fullheight-with-navbar is-light is-bold"] [
        div [Class "hero-body"] [
          div [Class "columns"] [
            div [Class "column is-half"] [
              div [Class "columns"] [
                div [Class "column"] [
                  div [Class "box"] [
                    div [Class "container"] [
                      h1 [Class "title"] [!!desc]
                      h3 [] [string "memoria is a collection of tools to help with learning and memorization."]
                    ]
                  ]
                ]
              ]
              div [Class "columns"] [
                div [Class "column"] [
                  div [Class "box"] [
                    div [Class "container"] [
                      a [ Class "button is-primary"; Href "app.html"] [ string "Text memorization"]
                      h3 [] [string "Memorize any text passage."]
                    ]
                  ]
                ]
              ]
              div [Class "columns"] [
                div [Class "column"] [
                  div [Class "box"] [
                    div [Class "container"] [
                      a [ Class "button is-primary"; Href "app.html#flashcards"] [ string "Latin Flashcards"]
                      h3 [] [string "Memorize latin vocabulary."]
                    ]
                  ]
                ]
              ]
            ]
            div [Class "column is-half"] [
              img [Src "/images/floppy.png"; Alt "floppy"]
            ]
          ]
        ]
      ]
    ]
    // div [Class "container"] [
    //   section [Class "articles"] [
    //     div [Class "column is-8 is-offset-2"] psts
    //   ]
    // ] 
  ]

let generate (ctx : SiteContents) (projectRoot: string) (page: string) =
  generate' ctx page
  |> Layout.render ctx