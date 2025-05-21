#' specs_tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# this script outline the module that conditionally renders the tab_bottom (lower nav_panel) on the
# lower left hand side of the "Explore the Data" page
# this is paired with a conditional panel in app_server.R
# this renders when tab_top "By Species" are selected (not "Summary" or "By Product Type" which is mod_other_tabs.R)

mod_specs_tabs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # START navset_card_pill
    bslib::navset_card_pill(
      # START nav_panel
      bslib::nav_panel(
        "Product Type",
        class = "custom-card",
        prodtype_func(inputID = ns("protype2Input"), label = NULL)
      ), # END nav_panel
      bslib::nav_panel(
        "Region",
        class = "custom-card",
        reg_func(inputID = ns("reg2Input"))
      ),

      bslib::nav_panel(
        "Processor Size/Type",
        class = "custom-card",
        size2_func(inputID = ns("size2Input"))
      ), # END nav_panel
      id = "tab_specs_bottom"
    ) # END navset_card_pill
  )
}

#' specs_tabs Server Functions
#'
#' @noRd
mod_specs_tabs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # return a list of reactive values to be used in the global app_server.R file
    # called in reactive data frames for filtering and plotting

    return(reactive({
      list(
        prodtype = input$protype2Input,
        regtype = input$reg2Input,
        sizetype = input$size2Input
      )
    }))
  })
}

## To be copied in the UI
# mod_specs_tabs_ui("specs_tabs_1")

## To be copied in the server
# mod_specs_tabs_server("specs_tabs_1")
