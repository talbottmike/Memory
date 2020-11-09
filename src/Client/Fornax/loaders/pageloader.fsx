#r "../_lib/Fornax.Core.dll"

type Page = {
    title: string
    link: string
}

let loader (projectRoot: string) (siteContent: SiteContents) =
    siteContent.Add({title = "Home"; link = "/"})
    siteContent.Add({title = "App"; link = "/app.html"})
    siteContent.Add({title = "Flash Cards"; link = "/app.html#flashcards"})
    // siteContent.Add({title = "About"; link = "/about.html"})
    // siteContent.Add({title = "Contact"; link = "/contact.html"})

    siteContent
