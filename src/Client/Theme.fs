module Client.Theme
open Feliz
open Feliz.MaterialUI

let defaultTheme = Styles.createMuiTheme()
let memoriaTheme = 
  Styles.createMuiTheme (
    [ theme.palette.type'.light
      theme.palette.background.default' "#fff"
      // theme.typography.h1.fontSize "3rem"
      // theme.typography.h2.fontSize "2rem"
      // theme.typography.h3.fontSize "1.5rem"
      theme.overrides.muiAppBar.colorDefault [
        style.backgroundColor "#F80505"
      ]
    ]
  )


//   let dark = Styles.createMuiTheme([
//     theme.palette.type'.dark
//     theme.palette.primary Colors.lightBlue
//     theme.palette.secondary Colors.pink
//     theme.palette.background.default' defaultTheme.palette.grey.``900``
//     theme.typography.h1.fontSize "3rem"
//     theme.typography.h2.fontSize "2rem"
//     theme.typography.h3.fontSize "1.5rem"

//     theme.overrides.muiAppBar.colorDefault [
//       style.backgroundColor defaultTheme.palette.grey.A400
//     ]
//     theme.overrides.muiPaper.root [
//       style.backgroundColor defaultTheme.palette.grey.A400
//     ]
//     theme.overrides.muiDrawer.paper [
//       style.backgroundColor defaultTheme.palette.grey.``900``
//     ]

//     theme.props.muiAppBar [
//       appBar.color.default'
//     ]
//   ])