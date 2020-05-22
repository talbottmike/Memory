#r "../_lib/Fornax.Core.dll"

type SiteInfo = {
    title: string
    description: string
}

let loader (projectRoot: string) (siteContent: SiteContents) =
    siteContent.Add({title = "memoria"; description = "Memory mastered"})

    siteContent
