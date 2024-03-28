#= = = = = = = = = = = = = = = = = = = = = =
# this page handles all of the reactive
# expressions for the dynamic user interface
#= = = = = = = = = = = = = = = = = = = = = =

# Defines the name of the tab, the names of the lists of metrics, and the names of the lists of the statistics
output$metrics <- renderUI({
    if (input$Sect_sel == 'FR') {
        tabsetPanel(
            tabPanel('Processor characteristics', uiOutput("demSelect"),   uiOutput("demStats")),
            tabPanel("Economic", uiOutput("econSelect"),  uiOutput("econStats")),
            tabPanel("Labor",    uiOutput("crewSelect"),  uiOutput("crewStats")),
            tabPanel("Cost",     uiOutput("costSelect"),  uiOutput("costStats")),
            tabPanel("Other",    uiOutput("otherSelect"), uiOutput("otherStats")),
            id = "Ind_sel", type = c("tabs"))
    } else {
        tabsetPanel(
            tabPanel('Vessel characteristics', uiOutput("demSelect"),   uiOutput("demStats")),
            tabPanel("Economic", uiOutput("econSelect"),  uiOutput("econStats")),
            tabPanel("Labor",    uiOutput("crewSelect"),  uiOutput("crewStats")),
            tabPanel("Cost",     uiOutput("costSelect"),  uiOutput("costStats")),
            tabPanel("Impacts", uiOutput("impactSelect"), uiOutput("impactStats")),
            tabPanel("Other",    uiOutput("otherSelect"), uiOutput("otherStats")),
            id = "Ind_sel", type = c("tabs"))
    }
})

# SET UP THE METRIC CHECKBOXES/RADIO BUTTONS FOR EACH TAB ####
##DatVars() is defined in ex.reactives##
##Checkbox types are defined in ui.R
##Note: See statistic selection below for settings of 'Mean' 'Median' 'Total' when grouping by vessels##
# Based on the layout, the sector, and whether demStats is total, determine the checkbox type and which value is selected
# Characteristics tab: checkbox/radiobutton set up ####


# Adding list of hyperlinks for sidebar components to be used in the sidebar selectors#####
source("external/explorer/explorerSourceFiles/metric_names/metric_names.R")

# IMPACT METRICS ####
output$impactSelect <- renderUI({
    ##settings for grouping by 'Metrics'
    if(input$LayoutSelect) {
        tags$div(
            class = 'ckbox',
            checkboxGroupInput("impactSelect", NULL,
                choiceNames = lapply(impacts$metric_link, HTML),
                choiceValues = impacts$metric_value,
                selected = 'Income impacts'))
    } else {
        tags$div(
            class = "ckbox",
            radioButtons("impactSelect", NULL,
                choiceNames = lapply(impacts$metric_link, HTML),
                choiceValues = impacts$metric_value,
                selected = 'Income impacts'))
    }
})

# CHARACTERISTI METRICS #####
output$demSelect <- renderUI({
    ##Settings for grouping by 'Metrics'
    if (input$LayoutSelect) {
        if (input$Sect_sel == 'FR') {
            tags$div(
                class = "ckbox",
                checkboxGroupInput("demSelect", NULL,
                    choiceNames = lapply(dem_fr$metric_link, HTML),
                    choiceValues = dem_fr$metric_value,
                    selected = 'Number of processors'))
        } else if (input$Sect_sel == 'CV') {
            tags$div(
                class = "ckbox",
                checkboxGroupInput("demSelect", NULL,
                    choiceNames = lapply(dem_cv$metric_link, HTML),
                    choiceValues = dem_cv$metric_value,
                    selected = "Number of vessels"))
        } else {
            tags$div(
                class = "ckbox",
                checkboxGroupInput("demSelect", NULL,
                    choiceNames = lapply(dem_cpms$metric_link, HTML),
                    choiceValues = demhtml_cpms$metric_value,
                    selected = "Number of vessels"))
        }
        #Settings for 'Group by vessels' or 'Group by processors'
    } else {
        if (input$Sect_sel == 'FR') {
            tags$div(
                class = "ckbox",
                radioButtons("demSelect", NULL,
                    choiceNames = lapply(dem_fr$metric_link, HTML),
                    choiceValues = dem_fr$metric_value,
                    selected = 'Number of processors'))
        } else if (input$Sect_sel == 'CV') {
            tags$div(
                class = "ckbox",
                radioButtons("demSelect", NULL,
                    choiceNames = lapply(dem_cv$metric_link, HTML),
                    choiceValues = dem_cv$metric_value,
                    selected = "Number of vessels"))
        } else {
            tags$div(
                class = "ckbox",
                radioButtons("demSelect", NULL,
                    choiceNames = lapply(dem_cpms$metric_link, HTML),
                    choiceValues = dem_cpms$metric_value,
                    selected = "Number of vessels"))
        }
    }
})

# Economics tab: metric checkbox/radiobutton set up ####
# (doesn't need all of the customization as characteristics because all of the metrics have the same characteristics)
# We added offloading revenue and custom processing revenue for FR so the metrics and layout will be different
output$econSelect <- renderUI({
    if (input$LayoutSelect) {
        if (input$Sect_sel == 'FR') {
            tags$div(
                class = "econfr",
                checkboxGroupInput("econSelect", NULL,
                    choiceNames = lapply(econ_fr$metric_link, HTML),
                    choiceValues = econ_fr$metric_value,
                    selected = 'Revenue')
            )
        } else {
            tags$div(
                class = "ckbox",
                checkboxGroupInput("econSelect", NULL,
                    choiceNames = lapply(econ$metric_link, HTML),
                    choiceValues = econ$metric_value,
                    selected = 'Revenue')
            )
        }
    } else {
        if (input$Sect_sel == 'FR') {
            tags$div(
                class = "econfr",
                radioButtons("econSelect", NULL,
                    choiceNames = lapply(econ_fr$metric_link, HTML),
                    choiceValues = econ_fr$metric_value,
                    selected = 'Revenue'))
        } else {
            tags$div(
                class = "ckbox",
                radioButtons("econSelect", NULL,
                    choiceNames = lapply(econ$metric_link, HTML),
                    choiceValues = econ$metric_value,
                    selected = 'Revenue')
            )
        }
    }
})

##select input box for processing and non-processing crew for CP/MS
selectinputprnprcrew <- selectInput(
    inputId = "PR_NPR_CREW",
    HTML(""),
    c(`Processing crew` = "PR", `Non-processing crew` = 'NPR'),
    selectize = F)

#select input box for captain and crew
selectinputcptcrew <- selectInput(
    inputId = 'CPT_CREW',
    HTML(""),
    c(`Crew` = 'CREW', `Captain` = 'CPT'),
    selectize = F
)
# select input box for production/non-production employees
selectinputprdnonprod <- selectInput(
    inputId = "PRD_NONPRD",
    HTML(""),
    c(`Production employees` = "PRD", `Non-production employees` = "NONPRD"),
    selectize = F
)
# Labor tab: metric checkbox/radiobutton set up ####
output$crewSelect <- renderUI({
    if (input$LayoutSelect) {
        if (input$Sect_sel == 'CV') {
            tagList(
                selectinputcptcrew,
                tags$div(
                    class = "ckbox",
                    checkboxGroupInput("crewSelect", NULL,
                        choiceNames = lappy(labor_cv_crew$metric_link, HTML),
                        choiceValues = labor_cv_crew$metric_value,
                        selected = "Number of crew")))
        } else if (input$Sect_sel == 'M' | input$Sect_sel == 'CP') {
            tagList(
                selectinputprnprcrew,
                tags$div(
                    class = "ckbox",
                    checkboxGroupInput("crewSelect", NULL,
                        choiceNames = lapply(labor_cpms_proc$metric_link, HTML),
                        choiceValues = labor_cpms_proc$metric_value,
                        selected = "Number of processing crew")))
        } else {
            tagList(
                selectinputprdnonprod,
                tags$div(
                    class = 'ckbox',
                    checkboxGroupInput("crewSelect", NULL,
                        choiceNames = lapply(labor_fr_prod$metric_link, HTML),
                        choiceValues = labor_fr_prod$metric_value,
                        selected = 'Average monthly number of production employees')))
        }}
    else {
        if (input$Sect_sel == 'CV') {
            tagList(
                selectinputcptcrew,
                tags$div(
                    class = "ckbox",
                    radioButtons("crewSelect", NULL,
                        choiceNames = lapply(labor_cv_crew$metric_link, HTML),
                        choiceValues = labor_cv_crew$metric_value,
                        selected = "Number of crew")))
        }
        else if (input$Sect_sel == 'M' | input$Sect_sel == 'CP') {
            tagList(
                selectinputprnprcrew,
                tags$div(
                    class = "ckbox",
                    radioButtons("crewSelect", NULL,
                        choiceNames = lapply(labor_cpms_proc$metric_link, HTML),
                        choiceValues = labor_cpms_proc$metric_value,
                        selected = "Number of processing crew")))
        } else {
            tagList(
                selectinputprdnonprod,
                tags$div(
                    class = "ckbox",
                    radioButtons("crewSelect", NULL,
                        choiceNames = lapply(labor_fr_prod$metric_link, HTML),
                        choiceValues = labor_fr_prod$metric_value,
                        selected = 'Average monthly number of production employees')))
        }
    }
})
# Cost tab: metric checkbox/radiobutton set up ####
output$costSelect <- renderUI({

    class4costs <- case_when(
        input$Sect_sel == 'CV' ~ 'costscv',
        input$Sect_sel == 'FR' ~ 'costsfr',
        T ~ 'costscpms')

    if (input$LayoutSelect) {
        if (input$Sect_sel == 'CV') {
            tags$div(
                class = class4costs,
                checkboxGroupInput("costSelect", 'Cost categories:',
                    choiceNames = lapply(costs_cv$metric_link, HTML),
                    choiceValues = costs_cv$metric_value,
                    selected = c('All fixed costs','All variable costs'))
            )
        } else if (input$Sect_sel == 'FR') {
            tags$div(
                class = class4costs,
                checkboxGroupInput("costSelect", 'Cost categories:',
                    choiceNames = lapply(costs_fr$metric_link, HTML),
                    choiceValues = costs_fr$metric_value,
                    selected = c('All fixed costs','All variable costs'))
            )
        } else if (input$Sect_sel == 'CP') {
            tags$div(
                class = class4costs,
                checkboxGroupInput("costSelect", 'Cost categories:',
                    choiceNames = lapply(costs_cp$metric_link, HTML),
                    choiceValues = costs_cp$metric_value,
                    selected = c('All fixed costs','All variable costs'))
            )
        } else {
            tags$div(
                class = class4costs,
                checkboxGroupInput("costSelect", 'Cost categories:',
                    choiceNames = lapply(costs_ms$metric_link, HTML),
                    choiceValues = costs_ms$metric_value,
                    selected = c('All fixed costs','All variable costs'))
            )
        }
    } else if (input$Sect_sel == 'CV') {
        tags$div(
            class = class4costs,
            radioButtons("costSelect", 'Cost categories:',
                choiceNames = lapply(costs_cv$metric_link, HTML),
                choiceValues = costs_cv$metric_value,
                selected = c('All variable costs')))
    } else if (input$Sect_sel == 'FR') {
        tags$div(
            class = class4costs,
            radioButtons("costSelect", 'Cost categories:',
                choiceNames = lapply(costs_fr$metric_link, HTML),
                choiceValues = costs_fr$metric_value,
                selected = c('All variable costs')))
    } else if (input$Sect_sel == 'CP') {
        tags$div(
            class = class4costs,
            radioButtons("costSelect", 'Cost categories:',
                choiceNames = lapply(costs_cp$metric_link, HTML),
                choiceValues = costs_cp$metric_value,
                selected = c('All variable costs')))
    } else {
        tags$div(
            class = class4costs,
            radioButtons("costSelect", 'Cost categories:',
                choiceNames = lapply(costs_ms$metric_link, HTML),
                choiceValues = costs_ms$metric_value,
                selected = c('All variable costs')))
    }
})


# Other tab: metric checkbox/radiobutton set up ####
output$otherSelect <- renderUI({
    # Setting when grouping by Metrics
    if (input$LayoutSelect) {
        if(input$Sect_sel == 'CV') {
            tags$div(class = "ckbox",
                checkboxGroupInput("otherSelect", NULL,
                    choiceNames = lapply(other_cv_layout$metric_link, HTML),
                    choiceValues = other_cv_layout$metric_value,
                    selected = "Days at sea"))

        } else if(input$Sect_sel == 'CP') {
            tags$div(class = "ckbox",
                checkboxGroupInput("otherSelect", NULL,
                    choiceNames = lapply(other_cp_layout$metric_link, HTML),
                    choiceValues = other_cp_layout$metric_value,
                    selected = 'Days fishing, processing, and steaming on the WC'))
        } else if (input$Sect_sel == 'FR') {
            tags$div(class = "ckbox",
                checkboxGroupInput("otherSelect", NULL,
                    choiceNames = lapply(other_fr$metric_link, HTML),
                    choiceValues = other_fr$metric_value,
                    selected = "Gini coefficient"))
        } else {
            tags$div(class = "ckbox",
                checkboxGroupInput("otherSelect", NULL,
                    choiceNames = lapply(other_ms_layout$metric_link, HTML),
                    choiceValues = other_ms_layout$metric_value,
                    selected = 'Days fishing, processing, and steaming on the WC'))
        }
        #Settings when 'Groups of vessels" or 'Groups of processors'
    } else if (input$Sect_sel == 'CV') {
        tags$div(
            class = "ckbox",
            radioButtons("otherSelect", NULL,
                choiceNames = lapply(other_cv$metric_link, HTML),
                choiceValues = other_cv$metric_value,
                selected = "Days at sea"))
    } else if(input$Sect_sel == 'CP') {
        tags$div(
            class = "ckbox",
            radioButtons("otherSelect", NULL,
                choiceNames = lapply(other_cp$metric_link, HTML),
                choiceValues = other_cp$metric_value,
                selected = 'Days fishing, processing, and steaming on the WC'))
    } else if (input$Sect_sel == 'FR') {
        tags$div(
            class = "ckbox",
            radioButtons("otherSelect", NULL,
                choiceNames = lapply(other_fr$metric_link, HTML),
                choiceValues = other_fr$metric_value,
                selected = "Gini coefficient"))
    } else {
        tags$div(
            class = "ckbox",
            radioButtons("otherSelect", NULL,
                choiceNames = lapply(other_ms$metric_link, HTML),
                choiceValues = other_ms$metric_value,
                selected = 'Days fishing, processing, and steaming on the WC'))
    }
})

# SET UP THE STATISTIC RADIOBUTTONS FOR EACH TAB ####
radiobuttonstatistic <- function(inputID, selection = 'Median') {
    radioButtons(inputID,
        HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
        choices = c('Mean', 'Median', 'Total'),
        select = selection)
}


#Impacts tab: statistic radiobuttons#####
#STATISTIC BUTTON FOR IMPACTS, only show total
output$impactStats <- renderUI({
    tagList(
        radioButtons("impactStats",
            HTML("<div> Statistic: <button id='istatimpacts' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
            choices = 'Total',
            select = 'Total'))
})

##Characteristics tab: statistic ratiobuttons ####
output$demStats <- renderUI({
    tagList(radiobuttonstatistic(inputID = "demStats", selection = 'Total'))
})
# Economics tab: statistic radiobuttons ####
output$econStats <- renderUI({
    if (input$Sect_sel == "FR")  {
        tagList(
            selectInput("AVE_MED",
                HTML(
                    "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
                ),
                c(Mean = "A", Median = "M", Total = "T"),
                selected = 'M',
                selectize = F
            ),
            tags$div(class = "statbox",
                radioButtons("econStats", "",  choices = c(DatVars()$STAT[5:6])))
        )
    } else if (input$Sect_sel == "M" || input$Sect_sel == 'CP') {
        tagList(
            selectInput("AVE_MED",
                HTML(
                    "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
                ),
                c(Mean = "A", Median = "M", Total = "T"),
                selected = 'M',
                selectize = F
            ),
            tags$div(
                class = "statbox",
                radioButtons("econStats", "", choices = c(DatVars()$STAT[6:9]), selected = DatVars()$STAT[6])))
    } else {
        tagList(
            selectInput("AVE_MED",
                HTML(
                    "<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
                ),
                c(Mean = "A", Median = "M", Total = "T"),
                selected = 'M',
                selectize = F
            ),
            tags$div(
                class = "statbox",
                radioButtons("econStats", "", choices = c(DatVars()$STAT[5:7]), selected = DatVars()$STAT[7])))
    }
})

# Labor tab: statistic radiobuttons ####
output$crewStats <- renderUI({
    tagList(radiobuttonstatistic(inputID = 'crewStats', selection = 'Median'))
})

selectinputavemedcosts <- selectInput(
    inputId = "AVE_MED_COSTS",
    HTML("<div> Statistic: <button id='istat' type='button' class='btn btn-default action-button shiny-bound-input'>
                                        <i class='fa fa-info-circle fa-fw' ></i></button> </div>"),
    c(Mean="A", Median="M", Total="T"),
    selectize = F,
    selected = 'M')

# Cost tab: statistic radiobuttons ####
output$costStats <- renderUI({
    if(input$Sect_sel=="FR")  {
        tagList(
            selectinputavemedcosts,
            tags$div(class="statbox", radioButtons("costStats","", choices = DatVars()$STAT[4:6])))
    } else if (input$Sect_sel=='CP' || input$Sect_sel == 'M') {
        tagList(
            selectinputavemedcosts,
            tags$div(class="statbox",
                radioButtons("costStats","",  choices = c(DatVars()$STAT[6:10]), selected=DatVars()$STAT[6])))
    }else {
        tagList(
            selectinputavemedcosts,
            tags$div(class="statbox",
                radioButtons("costStats","",  choices = c(DatVars()$STAT[5:8]), selected=DatVars()$STAT[5])))
    }
})

# Other tab: statistic radiobuttons ####
output$otherStats <- renderUI({
    if(input$LayoutSelect) {
        tagList(radiobuttonstatistic(inputID = 'otherStats'))
    } else if(input$otherSelect %in% c(
        "Share of landings by state",
        "Seasonality",
        "Gini coefficient"
    )) {
        tags$div(
            class = 'nostat',
            radioButtons("otherStats", "", choices = ""),
            style = "margin-bottom:20px;margin-top:-32px;margin-left:-15px;padding-top:0;"
        )
    } else {
        tagList(radiobuttonstatistic(inputID = 'otherStats'))
    }
})

# TAB SELECTIION - sets Ind_sel for Vessel/processor characteristics, ECONOMIC, labor, cost, and other ####
htmlindicatorselect <- HTML(
    "<div> Select an indicator category: <button id='ipo' type='button' class='btn btn-default action-button shiny-bound-input'>
        <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
)
output$IndicatorSelect <- renderUI({
    if (input$Sect_sel != 'FR') {
        selectInput("Ind_sel",
            htmlindicatorselect,
            c('Vessel characteristics', "Economic", "Labor", 'Impacts', "Other"),
            selected = 'Vessel characteristics',
            selectize = T
        )
    } else {
        selectInput("Ind_sel",
            htmlindicatorselect,
            c( 'Processor characteristics', "Economic", "Labor", "Other"),
            selected = 'Vessel characteristics',
            selectize = T
        )
    }

    #  }
})

# YEAR slider bar ####


output$Yearselect <- renderUI({
    # create 2 slider bars, one with 2004-current and the other 2009-current
    tagsdiv2009 <- tags$div(
        class = "ckbox",
        sliderInput(
            "YearSelect",
            "Years:",
            min = 2009,
            max = max(DatVars()$YEAR),
            value = c(2009, max(DatVars()$YEAR)),
            step = 1,
            sep = '',
            ticks = F
        )
    )

    tagsdiv2004 <- tags$div(
        class = "ckbox",
        sliderInput(
            "YearSelect",
            "Years:",
            min = 2004,
            max = max(DatVars()$YEAR),
            value = c(2009, max(DatVars()$YEAR)),
            step = 1,
            sep = '',
            ticks = F
        )
    )


    # choose which slider bar to display
    if (is.null(input$Ind_sel) || is.null(input$CategorySelect)) {
        tagsdiv2009
    }
    else if (input$Ind_sel == "Vessel characteristics" ||
            input$Ind_sel == 'Processor characteristics') {
        if (input$Sect_sel == "CV" &
                input$CategorySelect == 'Fisheries' &
                any(input$demSelect %in% c('Number of vessels', 'Vessel length'))) {
            tagsdiv2004
        } else {
            tagsdiv2009
        }
    }
    else if (input$Ind_sel == 'Other') {
        if (input$Sect_sel == "CV" &
                input$CategorySelect == 'Fisheries' &
                input$otherSelect %in% c('Seasonality', 'Share of landings by state', 'Gini coefficient', 'Landed weight', 'Trips')) {
            tagsdiv2004
        }
        else {
            tagsdiv2009
        }
    }
    else if (input$Ind_sel == 'Economic') {
        if (input$Sect_sel == 'CV' &
                input$CategorySelect == 'Fisheries' &
                input$econSelect[1] %in% 'Revenue') {
            tagsdiv2004
        }
        else {
            tagsdiv2009
        }
    }
    else if (input$Ind_sel == 'Labor' || input$Ind_sel == 'Cost' || input$Ind_sel == 'Impacts') {
        tagsdiv2009

    }
})

output$deflYearselect <- renderUI({

    defl_select <- selectInput("deflYearselect", "GDP Deflator Year:",
        c("2022" = 2022,
          "2021" = 2021,
          "2020" = 2020,
          "2019" = 2019))

     if (input$Ind_sel %in% c('Labor', 'Cost', 'Impacts', 'Economic')) {
        defl_select
    }


})

fish.var <- c(
    "All fisheries combined" = "All fisheries",
    " All catch share fisheries combined" = "All catch share fisheries",
    "Trawl only catch share fisheries",
    "Pacific whiting",
    "At-sea Pacific whiting",
    "Shoreside Pacific whiting",
    "Groundfish with trawl gear",
    "DTS trawl with trawl endorsement",
    "Non-whiting midwater trawl",
    "Non-whiting, non-DTS trawl with trawl endorsement",
    "Groundfish fixed gear with trawl endorsement",
    "All non-catch share fisheries combined" = "All non-catch share fisheries",
    "Crab",
    "Shrimp",
    "Other fisheries")
fish.var.grps <- c(
    "All fisheries combined" = "All fisheries",
    " All catch share fisheries combined" = "All catch share fisheries",
    "Trawl only catch share fisheries",
    "Pacific whiting",
    "Groundfish with trawl gear",
    "Groundfish fixed gear with trawl endorsement",
    "All non-catch share fisheries combined" = "All non-catch share fisheries"
)
# list of fisheries to filter by when using other categories (homeport, state, size)
fishgrps4cats <- c(
    "All fisheries",
    "All catch share fisheries",
    "Trawl only catch share fisheries",
    "All non-catch share fisheries")

# production activities for the fishery-equivalent table
prod.var <- c(
    "All production",
    "Groundfish production",
    "Pacific whiting production",
    'Non-whiting groundfish production',
    'Other species production')


# list of production types to filter by when using other categories (region, size)
prod4cats <- c(
    "All production",
    "Groundfish production",
    "Other species production")

# vessel size
vsssize <- c(
    "Large vessel (> 80 ft)",
    "Medium vessel (> 60ft, <= 80ft)",
    "Small vessel (<= 60 ft)")

# state
vss.st <- c('Washington & Alaska','Oregon','California')

# port
vss.port <- c('Puget Sound',
    'South and central WA coast',
    'Astoria',
    'Tillamook',
    'Newport',
    'Coos Bay',
    'Brookings',
    'Crescent City',
    'Eureka',
    'Fort Bragg',
    'San Francisco',
    'Morro Bay-Monterey')

# CATEGORY SELECTION - chooses the grouping filter (fishery, port, size) ####
# I don't think we use this anymore av 08/20/19
output$Categoryselect <- renderUI({
    if (input$Sect_sel != "FR") {
        tags$div(
            class = "ckbox",
            radioButtons("CategorySelect", "Group vessels according to:", choices = DatVars()$CATEGORY, selected = DatVars()$CATEGORY[1]))
    } else {
        tags$div(
            class = "ckbox",
            radioButtons("CategorySelect", "Group processors according to:", choices = DatVars()$CATEGORY, selected = DatVars()$CATEGORY[1]))
    }
})

# The Fishery/production-type choices when category isn't fisheries
output$fisheriesOptions <- renderUI({
    if (input$Sect_sel == "CV") {
        if (!is.null(input$CategorySelect) && input$CategorySelect != "Fisheries") {
            radioButtons("inSelect", "Fisheries", fishgrps4cats)
        }
    }
    else if (input$Sect_sel == "FR") {
        if (!is.null(input$CategorySelect) && input$CategorySelect != "Fisheries") {
            radioButtons( "inSelect", "Production Categories", prod4cats)
        }
    }
})
output$filters <- renderUI({
    if (input$Sect_sel == "CV") {
        tabsetPanel(
            tabPanel(DatVars()$CATEGORY[1]),
            tabPanel(DatVars()$CATEGORY[2]),
            tabPanel(names(DatVars()$CATEGORY[3]), value=DatVars()$CATEGORY[3]),
            tabPanel(DatVars()$CATEGORY[4]),
            id = "CategorySelect"
        )
    } else if (input$Sect_sel == "FR") {
        tabsetPanel(
            tabPanel(names(DatVars()$CATEGORY[1]), value=DatVars()$CATEGORY[1]),
            tabPanel(DatVars()$CATEGORY[2]),
            tabPanel(DatVars()$CATEGORY[3]),
            id = "CategorySelect"
        )
    } else {
    }
})

# Sets the list of variables (fisheries/prod types)
output$Variableselect <- renderUI({
    # There are no filters for Motherships or CP
    if (input$Sect_sel == "M" | input$Sect_sel == "CP") {
        hidden(checkboxGroupInput("VariableSelect", "", choices = "At-sea Pacific whiting", selected = "At-sea Pacific whiting"))
    } else if (!is.null(input$CategorySelect)) {
        # Catcher vessels
        if (input$Sect_sel == "CV") {
            if (input$CategorySelect == "State") {
                if (!input$LayoutSelect) {
                    tagList(
                        checkboxGroupInput("VariableSelect", NULL, choices = vss.st, selected = vss.st[1]))
                } else {
                    tagList(
                        tags$div(
                            class = "rbutton3",
                            radioButtons("VariableSelect", NULL, choices = vss.st, selected = vss.st[1])))
                }
            } #state
            else if (input$CategorySelect == "Vessel length class") {
                if (!input$LayoutSelect) {
                    tagList(
                        checkboxGroupInput("VariableSelect", NULL, choices = vsssize, selected = "Large vessel (> 80 ft)"))
                } else {
                    tagList(
                        tags$div(
                            class = "rbutton3",
                            radioButtons("VariableSelect", NULL, choices = vsssize, selected = "Large vessel (> 80 ft)")))
                }
            } else if (input$CategorySelect == "Homeport") {
                if (!input$LayoutSelect) {
                    tagList(
                        tags$div(
                            checkboxGroupInput("VariableSelect", NULL, choices = vss.port, selected = vss.port[1]))
                    )
                } else {
                    tagList(
                        tags$div(
                            class = "rbutton3",
                            radioButtons("VariableSelect", NULL, choices = vss.port, selected = vss.port[1]))
                    )
                }
            } #end homeport
            else if (input$CategorySelect == "Fisheries") {
                if (!input$LayoutSelect) {
                    if (input$FishWhitingSelect != 'All vessels') {
                        if(input$Ind_sel %in% c('Economic','Cost','Labor')) {
                            tags$div(class = "fishvarshortcv",
                                checkboxGroupInput("VariableSelect", NULL, choices = fish.var.grps, selected = fish.var[1]))
                        } else if(input$Ind_sel == 'Vessel characteristics') {
                            if (input$demSelect %in% c('Vessel replacement value','Vessel market value')) {
                                tags$div(class = "fishvarshortcv",
                                    checkboxGroupInput("VariableSelect", NULL, choices = fish.var.grps, selected = fish.var[1]))
                            } else {
                                tags$div(class = "fishvarcv",
                                    checkboxGroupInput("VariableSelect", NULL, choices = fish.var, selected = fish.var[1]))
                            }} else if(input$Ind_sel == 'Other') {
                                if(input$otherSelect == 'Days at sea') {
                                    tags$div(class = "fishvarshortcv",
                                        checkboxGroupInput("VariableSelect", NULL, choices = fish.var.grps, selected = fish.var[1]))
                                } else {
                                    tags$div(class = "fishvarcv",
                                        checkboxGroupInput("VariableSelect", NULL, choices = fish.var, selected = fish.var[1]))
                                }} else {
                                    tags$div(class = "fishvarcv",
                                        checkboxGroupInput("VariableSelect", NULL, choices = fish.var, selected = fish.var[1]))
                                }} else {
                                    tags$div(class = "fishvarcv",
                                        checkboxGroupInput("VariableSelect", NULL, choices = fish.var, selected = fish.var[1]))
                                }} else if(input$FishWhitingSelect != 'All vessels'){
                                    if(input$Ind_sel %in% c('Economic','Cost','Labor')) {
                                        tags$div(class = "fishvarshortcv",
                                            radioButtons("VariableSelect", NULL, choices = fish.var.grps, selected = fish.var[1]))
                                    } else if (input$Ind_sel == 'Vessel characteristics') {
                                        if (input$demSelect %in% c('Vessel replacement value','Vessel market value')) {
                                            tags$div(class = "fishvarshortcv",
                                                radioButtons("VariableSelect", NULL, choices = fish.var.grps, selected = fish.var[1]))
                                        } else {
                                            tags$div(class = "fishvarcv",
                                                radioButtons("VariableSelect", NULL, choices = fish.var, selected = fish.var[1]))
                                        }} else if (input$Ind_sel == 'Other') {
                                            if(input$otherSelect == 'Days at sea') {
                                                tags$div(class = "fishvarshortcv",
                                                    radioButtons("VariableSelect", NULL, choices = fish.var.grps, selected = fish.var[1]))
                                            } else {
                                                tags$div(class = "fishvarcv",
                                                    radioButtons("VariableSelect", NULL, choices = fish.var, selected = fish.var[1]))
                                            }} else {
                                                tags$div(class = "fishvarcv",
                                                    radioButtons("VariableSelect", NULL, choices = fish.var, selected = fish.var[1]))
                                            }} else {
                                                tags$div(class = "fishvarcv",
                                                    radioButtons("VariableSelect", NULL, choices = fish.var, selected = fish.var[1]))
                                            }
            }#end fisheries
        } else if (input$Sect_sel == "FR") {
            if (input$CategorySelect == "Region") {
                if (!input$LayoutSelect) {
                    tagList(
                        tags$div(
                            class = "ckbox",
                            checkboxGroupInput("VariableSelect", NULL, choices = c('Washington and Oregon', 'California'),
                                selected = "Washington and Oregon")))
                } else {
                    tagList(
                        tags$div(
                            class = "rbutton3",
                            radioButtons("VariableSelect", NULL, choices = c('Washington and Oregon', 'California'),
                                selected = "Washington and Oregon"
                            )))
                }
            } else if (input$CategorySelect == "Processor size") {
                if (!input$LayoutSelect) {
                    tagList(
                        tags$div(
                            checkboxGroupInput("VariableSelect", NULL, choices = c("Large", 'Medium', 'Small'),
                                selected = "Large")))
                } else {
                    tagList(
                        tags$div(
                            class = "rbutton3",
                            radioButtons("VariableSelect", NULL, choices = c("Large", 'Medium', 'Small'), selected = "Large")))
                }
            } else {
                if(input$Ind_sel != 'Labor') {
                    if(input$Ind_sel != 'Other') {
                        if (!input$LayoutSelect) {
                            tagList(
                                tags$div(
                                    class = 'prodfr',
                                    checkboxGroupInput("VariableSelect", NULL, choices = prod.var, selected = 'All production')
                                ))
                        } else {
                            tagList(
                                tags$div(
                                    class = 'prodfr',
                                    radioButtons("VariableSelect", NULL, choices = prod.var, selected = 'All production')))
                        } } else {
                            if(!input$LayoutSelect) {
                                if(input$otherSelect %in% c('Percentage of West Coast groundfish landed value purchased',
                                    'Percentage of West Coast groundfish landed weight purchased')){
                                    tagList(
                                        tags$div(class = 'ckboxFR',
                                            checkboxGroupInput("VariableSelect", NULL, choices = prod.var, selected = "Groundfish production")))
                                } else {
                                    tagList(
                                        tags$div(class = 'ckboxFR',
                                            checkboxGroupInput("VariableSelect", NULL, choices = prod.var, selected = "All production")))
                                } } else {
                                    if(input$otherSelect %in% c('Percentage of West Coast groundfish landed value purchased',
                                        'Percentage of West Coast groundfish landed weight purchased')){
                                        tagList(
                                            tags$div(class = 'rbFR',
                                                checkboxGroupInput("VariableSelect", NULL, choices = prod.var, selected = "Groundfish production")))
                                    } else {
                                        tagList(
                                            tags$div(class = 'rbFR',
                                                checkboxGroupInput("VariableSelect", NULL, choices = prod.var, selected = "All production")))
                                    }
                                } } } else {
                                    if(!input$LayoutSelect) {
                                        if(input$crewSelect %in% c('Total production employee payments',
                                            'Total non-production employee payments')){
                                            tagList(
                                                tags$div(class = 'prodfr',
                                                    checkboxGroupInput("VariableSelect", NULL, choices = prod.var, selected = "All production")))
                                        } else {
                                            tagList(
                                                tags$div(class = 'prodfr',
                                                    checkboxGroupInput("VariableSelect", NULL, choices = "All production", selected = 'All production')))

                                        } }  else {
                                            if(input$crewSelect %in% c('Total production employee payments',
                                                'Total non-production employee payments')){
                                                tagList(
                                                    tags$div(class = 'prodfr',
                                                        radioButtons("VariableSelect", NULL, choices = prod.var, selected = 'All production')))
                                            } else {
                                                tagList(
                                                    tags$div(class = 'prodfr',
                                                        radioButtons("VariableSelect", NULL, choices = "All production", selected = "All production")))
                                            }
                                        }}
            }
        }
    }
})

# Select inclAK ####
output$FishAkselect <- renderUI({
    if(!input$LayoutSelect) {
        if(input$Sect_sel == 'CV') {
            if(!is.null(input[["demSelect"]])) {
                if (input$demSelect %in% c(
                    'Revenue diversification',
                    'Proportion of ex-vessel revenue from CS fishery',
                    'Number of fisheries'
                )) {
                    tagList(
                        tags$div(style = "font-weight:bold; margin-bottom: 7px", "Alaskan Fisheries:"),
                        tags$div(
                            materialSwitch(
                                inputId = "FishAkSelect",
                                label   = "Alaskan fisheries",
                                right   = TRUE)))
                }
            }
        }
    }
})

# whitingv: choose all vessels, whiting vessels or non-whiting vessels ####
output$FishWhitingselectBox <- renderUI({
    if (input$Sect_sel == 'FR') {
        tags$div(
            class = 'ckbox',
            checkboxGroupInput('FishWhitingSelect',
                HTML(
                    "<div> Processor type:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
          </button></div>"
                ),
                choices = DatVars()$whitingv, selected = DatVars()$whitingv[1]))
    } else {
        tags$div(
            class = 'ckbox',
            checkboxGroupInput('FishWhitingSelect',
                HTML(
                    "<div> Vessel type:<button id='iwhiting' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw'></i>
          </button></div>"
                ),
                choices = DatVars()$whitingv, selected = DatVars()$whitingv[1]))
    }
})

# DISPLAY output$s ####
#Show data summed across button ####
# I don't think we need this since we switched to the slider bar
# output$VesSumSelect <- renderUI({
#   if (PermitPlot()) {
#     if (input$VariableSelect != "All Fisheries" &
#         input$VariableSelect != "All catch share fisheries" &
#         input$VariableSelect != "All non-catch share fisheries") {
#       tagList(tags$div(class = "ckbox",
#         radioButtons("VesSum",
#         HTML("<div> Show data summed: <button id='iVesSum' type='button' class='btn btn-default action-button shiny-bound-input'> <i class='fa fa-info-circle fa-fw' ></i></button> </div>"
#         ),
#         choices = c(
#           "within selected fisheries" = "within",
#           "across all catch share fisheries" = "acrossCS",
#           "across all West
# Coast fisheries" = "acrossWC"
#         )
#       )))
#     } else {
#       return()
#     }
#   } else {
#     return()
#   }
# })
# slider bar for layout ####
output$Layoutselect <- renderUI({
    materialSwitch(
        inputId = "LayoutSelect",
        label = "Select multiple metrics",
        right = TRUE,
        value = FALSE
    )
})

#html output####
output$characteristics_html <- renderUI({
    withMathJax(includeHTML("characteristics.html"))
})

output$economic_html <- renderUI({
    withMathJax(includeHTML("economic.html"))
})

output$labor_html <- renderUI({
    withMathJax(includeHTML("labor.html"))
})


output$costs_html <- renderUI({
    withMathJax(includeHTML("costs.html"))
})


output$impacts_html <- renderUI({
    withMathJax(includeHTML("impacts.html"))
})


output$other_html <- renderUI({
    withMathJax(includeHTML("other.html"))
})




# simple text output that has link for the html document ####

# OBSERVER ####
# processing or non-processing crew for cp/ms
observe({
    if(is.null(input$PR_NPR_CREW)) {
        return()
    }
    else if(input$PR_NPR_CREW == 'PR') {
        if(input$LayoutSelect) {
            updateCheckboxGroupInput(session, 'crewSelect',
                choiceNames = lapply(labor_cpms_proc$metric_link, HTML),
                choiceValues = labor_cpms_proc$metric_value)
        }
        else {
            updateRadioButtons(session, "crewSelect",
                choiceNames = lapply(labor_cpms_proc$metric_link, HTML),
                choiceValues = labor_cpms_proc$metric_value)
        }
    } else if(input$LayoutSelect) {
        updateCheckboxGroupInput(session, 'crewSelect',
            choiceNames = lapply(labor_cpms_nonproc$metric_link, HTML),
            choiceValues = labor_cpms_nonproc$metric_value)
    } else {
        updateRadioButtons(session, 'crewSelect',
            choiceNames = lapply(labor_cpms_nonproc$metric_link, HTML),
            choiceValues = labor_cpms_nonproc$metric_value)
    }
})

# observer for labor - captain and crew
observe({
    if(is.null(input$CPT_CREW)) {
        return()
    }
    else if(input$CPT_CREW == 'CREW') {
        if(input$LayoutSelect) {
            updateCheckboxGroupInput(session, 'crewSelect',
                choiceNames = lapply(labor_cv_crew$metric_link, HTML),
                choiceValues = labor_cv_crew$metric_value)
        }
        else {
            updateRadioButtons(session, "crewSelect",
                choiceNames = lapply(labor_cv_crew$metric_link, HTML),
                choiceValues = labor_cv_crew$metric_value)
        }
    } else if(input$LayoutSelect) {
        updateCheckboxGroupInput(session, 'crewSelect',
            choiceNames = lapply(labor_cv_captain$metric_link, HTML),
            choiceValues = labor_cv_captain$metric_value)
    } else {
        updateRadioButtons(session, 'crewSelect',
            choiceNames = lapply(labor_cv_captain$metric_link, HTML),
            choiceValues = labor_cv_captain$metric_value)
    }
})

# observer for labor - captain and crew
observe({
    if(is.null(input$PRD_NONPRD)) {
        return()
    }
    else if(input$PRD_NONPRD == 'PRD') {
        if(input$LayoutSelect) {
            updateCheckboxGroupInput(session, 'crewSelect',
                choiceNames = lapply(labor_fr_prod$metric_link, HTML),
                choiceValues = labor_fr_prod$metric_value)
        }
        else {
            updateRadioButtons(session, "crewSelect",
                choiceNames = lapply(labor_fr_prod$metric_link, HTML),
                choiceValues = labor_fr_prod$metric_value)
        }
    } else if(input$LayoutSelect) {
        updateCheckboxGroupInput(session, 'crewSelect',
            choiceNames = lapply(labor_fr_nonprod$metric_link, HTML),
            choiceValues = labor_fr_nonprod$metric_value)
    } else {
        updateRadioButtons(session, 'crewSelect',
            choiceNames = lapply(labor_fr_nonprod$metric_link, HTML),
            choiceValues = labor_fr_nonprod$metric_value)
    }
})

#select whether to show values per vessel, /vessel/day, or /vessel/metric-ton ECON
observe({
    if (is.null(input$AVE_MED)) {
        return()
    }
    else
        if (input$Sect_sel == 'FR') {
            if (input$AVE_MED == "M") {
                updateRadioButtons(session, "econStats", choices = c(DatVars()$STAT[4:5]))
            }  else if (input$AVE_MED == "A") {
                updateRadioButtons(session, "econStats",   choices = c(DatVars()$STAT[1:2]))
            } else  if (input$AVE_MED == "T") {
                updateRadioButtons(session, "econStats", choices = c(DatVars()$STAT[7:8]))
            }
        } else if (input$Sect_sel == 'M'|| input$Sect_sel == 'CP') {
            if (input$AVE_MED == "M") {
                updateRadioButtons(session, "econStats", choices = c(DatVars()$STAT[6:9]))
            }  else if (input$AVE_MED == "A") {
                updateRadioButtons(session, "econStats",   choices = c(DatVars()$STAT[1:4]))
            } else  if (input$AVE_MED == "T") {
                updateRadioButtons(session, "econStats", choices = c(DatVars()$STAT[11:14]))
            }
        } else {
            if (input$AVE_MED == "M") {
                updateRadioButtons(session, "econStats", choices = c(DatVars()$STAT[5:7]))
            }  else if (input$AVE_MED == "A") {
                updateRadioButtons(session, "econStats",   choices = c(DatVars()$STAT[1:3]))
            } else  if (input$AVE_MED == "T") {
                updateRadioButtons(session, "econStats", choices = c(DatVars()$STAT[9:11]))
            }
        }
})

#select whether to show values per vessel, /vessel/day, or /vessel/metric-ton # COSTS
observe({
    if (is.null(input$AVE_MED_COSTS)) return()
    else
        if(input$Sect_sel=='M' || input$Sect_sel == 'CP') {
            if(input$AVE_MED_COSTS=="M"){
                updateRadioButtons(session,"costStats", choices = c(DatVars()$STAT[6:10]))
            }  else if(input$AVE_MED_COSTS=="A"){
                updateRadioButtons(session,"costStats",   choices = c(DatVars()$STAT[1:5]))
            } else  if(input$AVE_MED_COSTS=="T"){
                updateRadioButtons(session,"costStats", choices = c(DatVars()$STAT[11:15]))
            }
        } else if(input$Sect_sel=='CV'){
            if(input$AVE_MED_COSTS=="M"){
                updateRadioButtons(session,"costStats", choices = c(DatVars()$STAT[5:8]))
            }  else if(input$AVE_MED_COSTS=="A"){
                updateRadioButtons(session,"costStats",   choices = c(DatVars()$STAT[1:4]))
            } else  if(input$AVE_MED_COSTS=="T"){
                updateRadioButtons(session,"costStats", choices = c(DatVars()$STAT[9:12]))
            }
        } else if(input$AVE_MED_COSTS=="M"){
            updateRadioButtons(session,"costStats", choices = c(DatVars()$STAT[4:6]))
        }  else if(input$AVE_MED_COSTS=="A"){
            updateRadioButtons(session,"costStats",   choices = c(DatVars()$STAT[1:3]))
        } else  if(input$AVE_MED_COSTS=="T"){
            updateRadioButtons(session,"costStats", choices = c(DatVars()$STAT[7:9]))
        }
})


observeEvent(input$reset_input, {
    if (input$LayoutSelect) {
        updateRadioButtons(session, "VariableSelect", selected = "")
    } else{
        updateCheckboxGroupInput(session, "VariableSelect", selected = as.character(0))
    }
})

# It's not clear to me that we need this
#= = = = = = = = = = = = = = = = = = = = = =
#Select whiting (data summed across category) ####
#= = = = = = = = = = = = = = = = = = = = = =
output$FishWhitingselect <- renderUI({
    if (input$Sect_sel == "M" || input$Sect_sel == "CP") {
        hidden(uiOutput("FishWhitingselectBox"))
    } else {
        uiOutput("FishWhitingselectBox")
    }
})
