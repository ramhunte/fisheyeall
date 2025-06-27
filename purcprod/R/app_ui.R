#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # START page_navbar
    bslib::page_navbar(
      # fillable = FALSE,
      # calling themes for the page elements
      theme = bslib::bs_theme(
        bootswatch = "materia",
        secondary = "#5EB6D9",
        fg = "#001743",
        bg = "#C2D9E3",
        primary = "#00559B",
        'navbar-bg' = "#001743"
      ),

      id = "navbar",
      title = "Purchase Production App",

      # NOAA Fisheries logo
      header = header(),

      #################################################################################
      #################################### Overview ###################################
      #################################################################################

      # Overview tab
      bslib::nav_panel(
        "Overview",
        mod_overview_ui("overview_1") # calling overview module UI
      ),

      #################################################################################
      ################################ Explore the Data ###############################
      #################################################################################

      # Explore the Data tab
      bslib::nav_panel(
        "Explore the Data",

        # START page_sidebar
        bslib::page_sidebar(
          # START sidebar
          sidebar = bslib::sidebar(
            width = 500,
            # START "Metric" nav_panel

            div("Compare data by:", style = "margin-bottom: 0.5rem; font-size: 1.5rem;"),

            bslib::navset_card_pill(
              ########################### Top nav_panels ######################################

              bslib::nav_panel(
                "Metric",
                class = "custom-card",
                mod_summary_ui("met_1")
              ), # END Metric nav_panel

              # START By Product Type nav_panel
              bslib::nav_panel(
                "Product Type",
                class = "custom-card",
                # Metric
                mod_prod_type_ui("prod_type_1")
              ), # END By Product Type nav_panel

              # START By Species nav_panel
              bslib::nav_panel(
                "Species",
                class = "custom-card",
                mod_specs_ui("specs_1")
              ), # END By Species nav_panel
              id = "tab_top"
            ), #END navset_card_pill

            ############################### Bottom nav_panels ###################################

            # START lower nav_panel

            # condition for lower tabs if By Species is NOT selected
            conditionalPanel(
              condition = "input.tab_top != 'Species'",
              uiOutput("otherTabs")
            ),

            # condition for lower tabs if By Species IS selected
            conditionalPanel(
              condition = "input.tab_top == 'Species'",
              uiOutput("speciesTabs")
            ), # END lower nav_panel

            # Deflator picker
            defl_func(
              inputID = "deflInput",
              label = "Deflator Year",
              choices = unique(met_prac$year),
              selected = 2023,
              width = "130px",
              options = list(`style` = "btn-year1")
            ),

            # downloadButton
            down_func(outputID = "downloadData")
          ), # END sidebar,

          ########################### main panel #######################################

          # START main panel navset_card_pill
          bslib::navset_card_pill(
            full_screen = TRUE,
            # START Plot nav_panel
            bslib::nav_panel(
              title = "Plot",
              class = "custom-card",
                plotOutput("exp_plot_ui") |>
                shinycssloaders::withSpinner(type = 4, color = pal[[1]], size = 1) |>
                bslib::as_fill_carrier()),

            # START Table nav_panel
            bslib::nav_panel(
              "Table",
              class = "custom-card",
                # adding a cool loader
                DT::DTOutput("table") |>
                shinycssloaders::withSpinner(type = 4, color = pal[[1]], size = 1)
            ) # END  Table nav_panel
          ) # END main panel navset_card_pill
        ) # END page_sidebar
      ), # END Explore the Data nav_panel

      #################################################################################
      ################################ Information Page ###############################
      #################################################################################

      # START Information Page nav_panel
      bslib::nav_panel(
        "Information Page",
        div(
          class = "scrollable-markdown",
          fluidRow(
            column(2),
            column(8, bslib::card(
              style = "background-color: #89b5c7; color: #1c4252;",
              shiny::includeMarkdown(app_sys("app/text/info.md")))
              ),
            column(2)
          )
        )
      ), # END "Information Page" nav_panel

      #################################################################################
      ################################ Contact US ###############################
      #################################################################################

      # START Contact Us nav_panel
      bslib::nav_panel(
        "Contact Us",
        div(
          class = "scrollable-markdown",
          fluidRow(
            column(2),
            column(8, bslib::card(
              style = "background-color: #89b5c7; color: #1c4252;",
              includeMarkdown(app_sys("app/text/contact.md")))),
            column(2)
          )
        )
      ), # END Contact Us nav_panel

      footer = footer() # footer
    ) # END page_navbar
  ) # END tagList
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "purchprod"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
