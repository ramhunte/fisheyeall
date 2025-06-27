#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #################################################################################
  #################################### Overview ###################################
  #################################################################################

  # Loading Module Server Outputs
  mod_overview_server("overview_1")

  #################################################################################
  ################################ Explore the Data ###############################
  #################################################################################

  # NOTE: so much of the "Explore the Data" tab's server is in this app_server.R and not
  # the module_*_.R servers because the inputs for the graphs/tables/data frames come
  # from various modules. Therefore, components of each of the modules themselves all
  # feed into global server file (app_server.R) components

  # Loading Module Server Outputs

  # top tabs
  met_inputs <- mod_summary_server("met_1")
  prod_type_inputs <- mod_prod_type_server("prod_type_1")
  specs_inputs <- mod_specs_server("specs_1")

  # bottom tabs
  other_tabs_inputs <- mod_other_tabs_server("other_tabs_1")
  specs_tabs_inputs <- mod_specs_tabs_server("specs_tabs_1")

  # output for conditional bottom tabs

  # if top nav_panels are "Metric" or "By Product Type", then "other_tabs_1"
  output$otherTabs <- renderUI({
    mod_other_tabs_ui("other_tabs_1")
  })

  # if top nav_panels are "By Species", then "specs_tabs_1"
  output$speciesTabs <- renderUI({
    mod_specs_tabs_ui("specs_tabs_1")
  })

  ############################# Deflator value  #################################

  defl_val <- reactive({
    gdp_defl |>
      dplyr::filter(.data$year == input$deflInput) |>
      dplyr::pull(.data$defl)
  })

  ############################# "Metric" tab: reactive data frame  #################################

  met_plot_df <- reactive({
    req(
      input$tab_top == "Metric",
      input$tab_bottom %in%
        c("Production Activities", "Region", "Processor Size"),
      met_inputs(),
      other_tabs_inputs()
    )

    # Conditional rendering depending on the tab_bottom select
    if (
      # "Metric" and "Production Activities"
      input$tab_top == "Metric" && input$tab_bottom == "Production Activities"
    ) {
      df <- met_prac |>
        dplyr::filter(
          # "Metric" tab filters
          .data$metric %in% met_inputs()$metric,
          # "Production Activities"
          .data$production_activity %in%
            c(other_tabs_inputs()$prodac, other_tabs_inputs()$osps)
        ) |>
        dplyr::mutate(
          # adjusting price related cols for deflation value
          value = dplyr::case_when(
            .data$metric %in%
              c(
                "Production price (per lb)",
                "Production value",
                "Purchase price (per lb)",
                "Purchase cost"
              ) ~
              .data$value * defl_val() / .data$defl,

            # for metrics that dont have price involved
            TRUE ~ .data$value
          ),
          unit_lab = dplyr::case_when(
            .data$metric == "Markup" ~ .data$metric,
            .data$metric %in%
              c("Production weight", "Purchase weight") ~
              paste0(
                .data$metric,
                ": ",
                .data$unit,
                " lbs"
              ),
            .data$metric == "Recovery rate" ~
              paste0(
                .data$metric
              ),
            TRUE ~
              paste0(
                .data$metric,
                ": ",
                .data$unit,
                " ",
                input$deflInput,
                " $"
              )
          )
        )
    } else if (
      # "Metric" and "Region"
      input$tab_top == "Metric" && input$tab_bottom == "Region"
    ) {
      df <- met_reg |>
        dplyr::filter(
          # "Metric" tab filters
          .data$metric %in% met_inputs()$metric,
          # .data$statistic %in% met_inputs()$stat,
          # "Region" tab filters
          .data$characteristic %in% other_tabs_inputs()$reg,
          .data$production_activity %in% other_tabs_inputs()$pracs1
        ) |>
        dplyr::mutate(
          # adjusting price realted cols for deflation value
          value = dplyr::case_when(
            .data$metric %in%
              c(
                "Production price (per lb)",
                "Production value",
                "Purchase price (per lb)",
                "Purchase cost"
              ) ~
              .data$value * defl_val() / .data$defl,

            # for metrics that dont have price involved
            TRUE ~ .data$value
          ),
          unit_lab = dplyr::case_when(
            .data$metric == "Markup" ~ .data$metric,
            .data$metric %in%
              c("Production weight", "Purchase weight") ~
              paste0(
                .data$metric,
                ": ",
                .data$unit,
                " lbs"
              ),
            .data$metric == "Recovery rate" ~
              paste0(
                .data$metric
              ),
            TRUE ~
              paste0(
                .data$metric,
                ": ",
                .data$unit,
                " ",
                input$deflInput,
                " $"
              )
          )
        )
    } else if (
      # "Metric" and "Processor Size"
      input$tab_top == "Metric" && input$tab_bottom == "Processor Size"
    ) {
      df <- met_size |>
        dplyr::filter(
          # "Metric" tab filters
          .data$metric %in% met_inputs()$metric,
          # "Processor Size" tab filters
          .data$characteristic %in% other_tabs_inputs()$size,
          .data$production_activity %in% other_tabs_inputs()$pracs2
        ) |>
        dplyr::mutate(
          # adjusting price realted cols for deflation value
          value = dplyr::case_when(
            .data$metric %in%
              c(
                "Production price (per lb)",
                "Production value",
                "Purchase price (per lb)",
                "Purchase cost"
              ) ~
              .data$value * defl_val() / .data$defl,

            # for metrics that dont have price involved
            TRUE ~ .data$value
          ),

          unit_lab = dplyr::case_when(
            .data$metric == "Markup" ~ .data$metric,
            .data$metric %in%
              c("Production weight", "Purchase weight") ~
              paste0(
                .data$metric,
                ": ",
                .data$unit,
                " lbs"
              ),
            .data$metric == "Recovery rate" ~
              paste0(
                .data$metric
              ),
            TRUE ~
              paste0(
                .data$metric,
                ": ",
                .data$unit,
                " ",
                input$deflInput,
                " $"
              )
          )
        )
    }

    return(df)
  })

  ############################ "By Product Type" tab: reactive data frame  ###############################

  # reactive data frame for "By Product Type" tab
  prod_plot_df <- reactive({
    # "By Product Type" and "Production Acitivities"
    if (input$tab_top == "Product Type") {
      if (input$tab_bottom == "Production Activities") {
        df <- prod_prac |>
          dplyr::filter(
            # "By Product Type" tab filters
            .data$metric %in% prod_type_inputs()$metric,
            .data$type %in% prod_type_inputs()$prod_type,
            # "Production Activities" tab filters
            .data$production_activity %in%
              c(other_tabs_inputs()$prodac, other_tabs_inputs()$osps)
          ) |>
          dplyr::mutate(
            # adjusting price related cols for deflation value
            value = dplyr::case_when(
              .data$metric %in%
                c(
                  "Production price (per lb)",
                  "Production value",
                  "Purchase price (per lb)",
                  "Purchase cost"
                ) ~
                .data$value * defl_val() / .data$defl,

              # for metrics that dont have price involved
              TRUE ~ .data$value
            ),
            unit_lab = dplyr::case_when(
              .data$metric == "Production weight" ~
                paste0(
                  .data$type,
                  ": ",
                  .data$unit,
                  " lbs"
                ),
              TRUE ~
                paste0(
                  .data$type,
                  ": ",
                  .data$unit,
                  " ",
                  input$deflInput,
                  " $"
                )
            )
          )
      } else if (
        # "By Product Type" and "Region"
        input$tab_bottom == "Region"
      ) {
        df <- prod_reg |>
          dplyr::filter(
            # "By Product Type" tab filters
            .data$metric %in% prod_type_inputs()$metric,
            .data$type %in% prod_type_inputs()$prod_type,
            # "Region" tab filters
            .data$characteristic %in% other_tabs_inputs()$reg,
            .data$production_activity %in% other_tabs_inputs()$pracs1
          ) |>
          dplyr::mutate(
            # adjusting price realted cols for deflation value
            value = dplyr::case_when(
              .data$metric %in%
                c(
                  "Production price (per lb)",
                  "Production value",
                  "Purchase price (per lb)",
                  "Purchase cost"
                ) ~
                .data$value * defl_val() / .data$defl,

              # for metrics that dont have price involved
              TRUE ~ .data$value
            ),
            unit_lab = dplyr::case_when(
              .data$metric == "Production weight" ~
                paste0(
                  .data$type,
                  ": ",
                  .data$unit,
                  " lbs"
                ),
              TRUE ~
                paste0(
                  .data$type,
                  ": ",
                  .data$unit,
                  " ",
                  input$deflInput,
                  " $"
                )
            )
          )
      } else if (
        # "By Product Type" and "Processor Size"
        input$tab_bottom == "Processor Size"
      ) {
        df <- prod_size |>
          dplyr::filter(
            # "By Product Type" tab filters
            .data$metric %in% prod_type_inputs()$metric,
            .data$type %in% prod_type_inputs()$prod_type,
            # "Processor Size" tab filters
            .data$characteristic %in% other_tabs_inputs()$size,
            .data$production_activity %in% other_tabs_inputs()$pracs2
          ) |>
          dplyr::mutate(
            # adjusting price related cols for deflation value
            value = dplyr::case_when(
              .data$metric %in%
                c(
                  "Production price (per lb)",
                  "Production value",
                  "Purchase price (per lb)",
                  "Purchase cost"
                ) ~
                .data$value * defl_val() / .data$defl,

              # for metrics that dont have price involved
              TRUE ~ .data$value
            ),
            unit_lab = dplyr::case_when(
              .data$metric == "Production weight" ~
                paste0(
                  .data$type,
                  ": ",
                  .data$unit,
                  " lbs"
                ),
              TRUE ~
                paste0(
                  .data$type,
                  ": ",
                  .data$unit,
                  " ",
                  input$deflInput,
                  " $"
                )
            )
          )
      }

      df # Return the filtered data frame
    }
  })

  ########################### "By Species" tab: reactive data frame #################################

  specs_plot_df <- reactive({
    if (input$tab_top == "Species") {
      if (input$tab_specs_bottom == "Product Type") {
        df <- specs_prod |>
          dplyr::filter(
            # "By Species" tab filters
            .data$metric %in% specs_inputs()$metric,
            .data$production_activity %in% c(specs_inputs()$specs, specs_inputs()$os),
            # bottom tab filters
            .data$type %in% specs_tabs_inputs()$prodtype
          ) |>
          dplyr::mutate(
            # adjusting price-related cols for deflation value
            value = dplyr::case_when(
              .data$metric %in%
                c(
                  "Production price (per lb)",
                  "Production value",
                  "Purchase price (per lb)",
                  "Purchase cost"
                ) ~
                .data$value * defl_val() / .data$defl,
              TRUE ~ .data$value
            ),
            unit_lab = dplyr::case_when(
              .data$metric == "Production weight" ~
                paste0(
                  .data$production_activity,
                  ": ",
                  .data$unit,
                  " lbs"
                ),
              TRUE ~
                paste0(
                  .data$production_activity,
                  ": ",
                  .data$unit,
                  " ",
                  input$deflInput,
                  " $"
                )
            )
          )
      } else if (
        # "By Species" and "Region"
        input$tab_specs_bottom == "Region"
      ) {
        df <- specs_reg |>
          dplyr::filter(
            # "By Species" tab filters
            .data$metric %in% specs_inputs()$metric,
            .data$production_activity %in% c(specs_inputs()$specs, specs_inputs()$os),
            # bottom tab filters
            .data$characteristic %in% specs_tabs_inputs()$regtype,
            .data$type == "All product types"
          ) |>
          dplyr::mutate(
            # adjusting price-related cols for deflation value
            value = dplyr::case_when(
              .data$metric %in%
                c(
                  "Production price (per lb)",
                  "Production value",
                  "Purchase price (per lb)",
                  "Purchase cost"
                ) ~
                .data$value * defl_val() / .data$defl,
              TRUE ~ .data$value
            ),
            unit_lab = dplyr::case_when(
              .data$metric == "Production weight" ~
                paste0(
                  .data$production_activity,
                  ": ",
                  .data$unit,
                  " lbs"
                ),
              TRUE ~
                paste0(
                  .data$production_activity,
                  ": ",
                  .data$unit,
                  " ",
                  input$deflInput,
                  " $"
                )
            )
          )
      } else if (
        # "By Species" and "Processor Size"
        input$tab_specs_bottom == "Processor Size"
      ) {
        df <- specs_size |>
          dplyr::filter(
            # "By Species" tab filters
            .data$metric %in% specs_inputs()$metric,
            .data$production_activity %in% c(specs_inputs()$specs, specs_inputs()$os),
            # bottom tab filters
            .data$characteristic %in% specs_tabs_inputs()$sizetype,
            .data$type == "All product types"
          ) |>
          dplyr::mutate(
            # adjusting price-related cols for deflation value
            value = dplyr::case_when(
              .data$metric %in%
                c(
                  "Production price (per lb)",
                  "Production value",
                  "Purchase price (per lb)",
                  "Purchase cost"
                ) ~
                .data$value * defl_val() / .data$defl,
              TRUE ~ .data$value
            ),
            unit_lab = dplyr::case_when(
              .data$metric == "Production weight" ~
                paste0(
                  .data$production_activity,
                  ": ",
                  .data$unit,
                  " lbs"
                ),
              TRUE ~
                paste0(
                  .data$production_activity,
                  ": ",
                  .data$unit,
                  " ",
                  input$deflInput,
                  " $"
                )
            )
          )
      }

      return(df)
    }
  })


  ############################################ Plots  ############################################

  # this chunk renders the the main panel "Plot" tab plot in the UI
  # conditional render depending on which tab_top is selected

  output$exp_plot_ui <- renderPlot({
######## Metric tab #########
    if (input$tab_top == "Metric") {
      plot_func(
        # plot functionjnn8888rf
        data = met_plot_df(),
        lab = NULL,
        group = if (input$tab_bottom == "Production Activities") "production_activity" else "characteristic",
        facet = "unit_lab" # faceting by unit label
      )

      ######## Product Type tab ########
    } else if (input$tab_top == "Product Type") {
      plot_func(
        data = prod_plot_df(), # same steps as above for "Metric" ^^
        lab = prod_type_inputs()$metric,
        group = if (input$tab_bottom == "Production Activities") "production_activity" else "characteristic",
        facet = "unit_lab"
      )
      ######## Species ########
    } else if (input$tab_top == "Species") {
      req(input$tab_specs_bottom)

      plot_func(
        data = specs_plot_df(),
        lab = specs_inputs()$metric,
        group = if (input$tab_specs_bottom == "Product Type") "type" else "characteristic",
        facet = "unit_lab",
      )}
  })

  ############################################ Table  ############################################

  # this chunk renders the the main panel "Table" tab table in the UI
  # conditional render depending on which tab_top is selected
  output$table <- DT::renderDT(
    {
      if (input$tab_top == "Metric") {
        df <- met_plot_df() #render "Metric" tab table
      } else if (input$tab_top == "Product Type") {
        df <- prod_plot_df() #render "By Product Type" tab table
      } else if (input$tab_top == "Species") {
        req(input$tab_specs_bottom)
        df <- specs_plot_df() #render "By Species" tab table
      }

      # Process the data using the function to make it pretty for the table
      process_df(df)
    },
    options = list(
      scrollX = TRUE, # Enable horizontal scroll
      scrollY = "680px" # setting vertical scroll height
    )
  )

  ####################################### Data Table Download #######################################

  # download data button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("purchprod", input$tab_top, "data.csv", sep = "_") # title for csv
    },
    content = function(file) {
      # conditional table render depending on tab_top selection
      if (input$tab_top == "Metric") {
        utils::write.csv(met_plot_df(), file)
      } else if (input$tab_top == "Product Type") {
        utils::write.csv(prod_type_plot_df(), file)
      } else if (input$tab_top == "Species") {
        utils::write.csv(specs_plot_df(), file)
      }
    }
  )
}
