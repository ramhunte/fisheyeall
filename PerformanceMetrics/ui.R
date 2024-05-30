source('appFrame.R')

library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(bsplus)
library(shinycssloaders)

#options(shiny.server = NULL)
#options(shiny.error = browser)
# custom css functions
# calls .css selector for well-sub
wellPanelSub <- function(...){div(class = "well-sub", ...)}
# calls .css selector for radioButton header
wellPanelHeading <- function(...){div(class = "well-radioHeading", ...)}

function(request) {
    fluidPage(title = "FISHEyE",
        useShinyjs(),
        # create a CSS to modify style of validation test error (for Variability analysis)
        tags$head(tags$body(includeHTML("google-analytics.noaa.js"))),
        tags$head(
            tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 120%;}")),
            tags$style(HTML(".select {margin-top:-20px}"),tags$textarea(id="message", rows=3, cols=40, "Default value")),
            #  tags$style(HTML(".navbar .nav > li { position:relative; z-index: 10000;}")),
            tags$style(HTML(".navbar {position:static}")),
            tags$style(HTML(".ckbox {margin-top: 0px; margin-bottom: -15px}")),
            tags$style(HTML(".statbox {margin-top: 0px; margin-bottom: -15px}")),
            tags$style(HTML(".radio:first-child {margin-top: 10px;}")),

            # HOW TO CHANGE STYLE OF CHECKBOXES #####
            # The following code applies 3 types of style options to a specific checkbox/button in the list.
            # e.g. nth-child(5) mean apply to the 5th item in the list whereas nth-child(-n+5) means apply to the first 5 items in the list
            # sometime "nth-child(-n+# items in list)" is applied to make the code shorter. For instance, you can indent all items and then select the few that should not be indented.
            # The order of the style options matters. If you indent costscv to 17px and then on the next line apply 0px, the resulting indentation will be 0px.

            #Formatting of checkboxes and radiobuttons for Cost metrics
            tags$style(HTML("
              .costscv .checkbox:nth-child(-n+14) label,
              .costscv .radio:nth-child(-n+14) label,

              .costsfr .checkbox:nth-child(-n+20) label,
              .costsfr .radio:nth-child(-n+20) label,

              .costscpms .checkbox:nth-child(-n+12) label,
              .costscpms .radio:nth-child(-n+12) label{margin-left: 17px;}

              .costscv  .checkbox:first-child label,
              .costscv  .checkbox:nth-child(5) label,
              .costscv  .radio:first-child label,
              .costscv  .radio:nth-child(5) label,

              .costscpms  .checkbox:first-child label,
              .costscpms  .checkbox:nth-child(6) label,
              .costscpms  .radio:first-child label,
              .costscpms  .radio:nth-child(6) label,

              .costsfr  .checkbox:first-child label,
              .costsfr  .checkbox:nth-child(5) label,
              .costsfr  .radio:first-child label,
              .costsfr  .radio:nth-child(5) label{font-weight:bold; margin-left: 0px}")),

            # Formatting checkboxes and radio buttons for CV Fishery filters
            tags$style(HTML("
              .fishvarcv .checkbox:nth-child(-n+15),
              .fishvarcv .radio:nth-child(-n+15){margin-left: 17px}

              .fishvarcv .checkbox:nth-child(5) label,
              .fishvarcv .checkbox:nth-child(6) label,
              .fishvarcv .checkbox:nth-child(8) label,
              .fishvarcv .checkbox:nth-child(9) label,
              .fishvarcv .checkbox:nth-child(10) label,

              .fishvarcv .radio:nth-child(5) label,
              .fishvarcv .radio:nth-child(6) label,
              .fishvarcv .radio:nth-child(8) label,
              .fishvarcv .radio:nth-child(9) label,
              .fishvarcv .radio:nth-child(10) label{margin-left: 34px}

              .fishvarcv .checkbox:first-child label,
              .fishvarcv .checkbox:nth-child(2) label,
              .fishvarcv .checkbox:nth-child(12) label,

              .fishvarcv .radio:first-child label,
              .fishvarcv .radio:nth-child(2) label,
              .fishvarcv .radio:nth-child(12) label{margin-left: -17px; font-weight:bold; }")),

            # Formatting checkboxes and radio buttons for the short list CV filters
            tags$style(HTML("
              .fishvarshortcv .checkbox:nth-child(-n+8) label,
              .fishvarshortcv .radio:nth-child(-n+8) label{margin-left: 17px}

              .fishvarshortcv .checkbox:nth-child(4) label,
              .fishvarshortcv .checkbox:nth-child(5) label,

              .fishvarshortcv .radio:nth-child(4) label,
              .fishvarshortcv .radio:nth-child(5) label{margin-left: 34px}

              .fishvarshortcv .checkbox:first-child label,
              .fishvarshortcv .checkbox:nth-child(2) label,
              .fishvarshortcv .checkbox:nth-child(7) label,

              .fishvarshortcv .radio:first-child label,
              .fishvarshortcv .radio:nth-child(2) label,
              .fishvarshortcv .radio:nth-child(7) label{font-weight:bold; margin-left: 0px}")),

            # Formating for FR econ list
            tags$style(HTML("
              .econfr .radio:nth-child(2) label,
              .econfr .radio:nth-child(3) label,
              .econfr .radio:nth-child(4) label,
              .econfr .checkbox:nth-child(2) label,
              .econfr .checkbox:nth-child(3) label,
              .econfr .checkbox:nth-child(4) label{margin-left: 17px;}")),

            # Formatting for FR production activities
            tags$style(HTML("
              .prodfr .checkbox:first-child label,
              .prodfr .radio:first-child label{font-weight:bold;}

              .prodfr .checkbox:nth-child(2) label,
              .prodfr .checkbox:nth-child(5) label,

              .prodfr .radio:nth-child(2) label,
              .prodfr .radio:nth-child(5) label{margin-left: 17px;}

              .prodfr .checkbox:nth-child(3) label,
              .prodfr .checkbox:nth-child(4) label,

              .prodfr .radio:nth-child(3) label,
              .prodfr .radio:nth-child(4) label{margin-left:34px;}")),

            # Removes the statistics for other stats i.e. gini coefficient, seasonality
            tags$style(HTML(".nostat  input[type=radio] {border: 0px;    width: 0%;    height:0em;}#")),

            # tool tip style####
            tags$style(HTML('#iof,
                             #isummed,
                             #ifg,
                             #istat,
                             #istatimpacts,
                             #ipo,
                             #ivs,
                             #iem,
                             #ivariance,
                             #FRr,
                             #FRs,
                             #FRi,
                             #icompare,
                             #iwhiting{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px;
                              color:RoyalBlue}'))),
        # java script
        tags$style(type='text/css', "#data2,
                                       #data  { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:170%;left:425%;}"),

        tags$head(includeScript("google-analytics.js")),
        tags$head(tags$script(src = "message-handler.js")),

        tags$head(
            # Main css page, downloaded from bootswatch
            tags$link(rel="stylesheet", type="text/css", href="bootstrap.css"),
            # secondary css page with fisheye specific attributes
            tags$link(rel="stylesheet", type="text/css", href="fisheye.css"),
            tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.4.0/css/font-awesome.min.css"),
            tags$style(type="text/css", ".tab-content {overflow: visible;}")
        ),

        # source("www/shiny_framebuster/framebuster.R")$value,
        ## example R framebusting code
        appFrameHeaderScrolling(),
        ### #-- Government Shutdown banner --- ###
        #tags$div(class='header', #tags$syle(HTML('background-color:white;color:blue;width:auto; text-align:left;float:center;margin:0 auto; padding: 0.3em 0 0.5em;font-size:large">
        #  tags$p("The Federal Government is closed. The site will not be updated; however NOAA websites and social media channels necessary to protect lives and property will be maintained. See", tags$a(href="https://www.weather.gov", "www.weather.gov."), "Learn more at", tags$a(href="https://noaa.gov", "NOAA.gov."))),
        ### #-- Government Shutdown banner --- ###
        # fluidRow(div(style = "padding-bottom: 5px;margin-bottom:0"),
        #   tags$h2(style = "margin-left: 15px",
        #     HTML("<div><p style='font-size:120%'><strong><a style='color:white; background-color:#1a5d99;  font-family:Cambria; border-radius:25px; padding:5px'
        #                           href='https://connect.fisheries.noaa.gov/fisheye/fisheyelandingpage.html'> FISHEyE</a>- Performance Metrics </strong></p>
        #                       </div>")), htmlOutput("SectPrint")
        # ),


        # fluidRow(img(src = 'header_noaa_l.png', height = 87, width = 401)),

        navbarPage(id="page", collapsible=TRUE, inverse=F,
            title="",

            tabPanel("Explore the data", value="results",
                sidebarLayout(

                    sidebarPanel(
                        wellPanel(
                            fluidRow(
                                column(4,
                                    uiOutput("resetButton"),
                                    uiOutput('Button'))),#end fluidRow

                            # Select a vessel/Processor Type
                            radioGroupButtons("Sect_sel", label = NULL,
                                choices = c('Catcher Vessels'="CV", 'Mothership Vessels'="M", 'Catcher-Processor Vessels'="CP", 'Shorebased Processors'="FR"),
                            ),

                            # Metrics
                            tags$div(class="header collapsed", "Metric") %>% bsplus::bs_attach_collapse("collapse1"),
                            bsplus::bs_collapse(id = "collapse1",
                                content = tags$div(column(12, uiOutput('metrics'),
                                    style = "background:white; padding: 10px;margin-below:4px;border-color: #bce8f1;")),
                                show = TRUE
                            ),

                            # Filters
                            conditionalPanel(condition="input.Sect_sel=='CV' || input.Sect_sel=='FR'",
                                tags$div(class="header collapsed", "Filter by: fisheries, location, size")%>% bsplus::bs_attach_collapse("collapse2"),
                                bsplus::bs_collapse(id = "collapse2",
                                    content = tags$div(column(12, uiOutput('filters'),
                                        uiOutput("Variableselect"),
                                        style = "background:white; padding: 10px;margin-below:4px;border-color: #bce8f1;")),
                                    show = TRUE

                                )),

                            # Additional Filters
                            tags$div(class="header collapsed", "Additional Filters")%>% bsplus::bs_attach_collapse("collapse3"),
                            bsplus::bs_collapse(id = "collapse3",
                                content = tags$div(column(12,
                                    fluidRow(
                                        column(6, uiOutput("FishWhitingselect")),
                                        column(6, uiOutput("fisheriesOptions"))
                                    ),
                                    fluidRow(
                                        column(6, uiOutput("Yearselect")),
                                        column(6, uiOutput("deflYearselect")),
                                        column(6, uiOutput("FishAkselect"))
                                    ),
                                    style = "background:white; padding: 10px;margin-below:4px;border-color: #bce8f1;"))
                            ) ,

                            tags$div(style = "font-weight:bold; margin-bottom: 7px", "Display Options:"),

                            # Multiple metrics
                            fluidRow(
                                column(12,
                                    uiOutput('Layoutselect')
                                )),

                            # show variance
                            conditionalPanel(condition="input.Ind_sel!='Economic'&
                                                              (input.demStats!='Total' || crewStats!='Total') &
                                                              input.otherSelect!='Seasonality'&
                                                              input.otherSelect!='Share of landings by state'||
                                                              input.Ind_sel=='Economic'&input.econStats!='T'",
                                uiOutput("Plotselect")),

                            fluidRow(
                                column(4, uiOutput("download_figure")),
                                column(4, uiOutput("download_Table")),
                                column(4, uiOutput("download_RawData")),
                                column(4, bookmarkButton())
                            )

                        ),      style = "padding: 0px;overflow-y:scroll; max-height: 800px;"), # end right side column

                    mainPanel(
                        tabsetPanel(id = "tabs",
                            tabPanel("Visualize the Data", value="Panel1", plotOutput("PlotMain") %>% shinycssloaders::withSpinner(color="#0dc5c1"), style ="min-height: 1600px;"),
                            tabPanel("Dataset", value="Panel2", DT::DTOutput("TableMain")),
                            tabPanel("Description", 
                                conditionalPanel(condition="input.Ind_sel == 'Processor characteristics' || input.Ind_sel == 'Vessel characteristics'",
                                    htmlOutput("characteristics_html")),
                                conditionalPanel(condition="input.Ind_sel == 'Economic'",
                                    htmlOutput("economic_html")),
                                conditionalPanel(condition="input.Ind_sel == 'Labor'",
                                    htmlOutput("labor_html")),
                                conditionalPanel(condition="input.Ind_sel=='Cost'",
                                    htmlOutput("costs_html")),
                                conditionalPanel(condition="input.Ind_sel=='Impacts'",
                                    htmlOutput("impacts_html")),
                                conditionalPanel(condition="input.Ind_sel=='Other'",
                                    htmlOutput("other_html"))

                            )
                        ))

                )),
            #tabPanel(HTML('History'), htmlOutput('HistoryText')),
            tabPanel("Instructions",
                source("external/explorer/explorerSourceFiles/instructions.R")$value),
            # Link to Tech Memo #
            tabPanel(HTML('<a href = "https://repository.library.noaa.gov/view/noaa/31435" target = "_blank"
                    style="margin:-30px -1px">Documentation</a>')), # end right side column
            tabPanel(HTML('Bulletin Board'),
                fluidRow(
                    column(12, htmlOutput("BlogText")),
                    column(5,  htmlOutput("BlogUpdates")),
                    column(1),
                    column(5,  htmlOutput("BlogResponses")))),
            tabPanel(HTML('Contact us'),
                fluidRow(
                    column(12, htmlOutput("Email")))
            ),
            # tabPanel(HTML('<i class="fa fa-folder-open-o fa-fw" style="margin-right:18ex;display:inline-block;vertical-align:bottom;float:left;white-space:nowrap"> FISHEyE Applications</i>'),
            #   fluidRow(
            #     column(12, htmlOutput("ApplicationsText"))
            #   )),
            tabPanel(HTML('<a class="btn btn-warning", href="https://connect.fisheries.noaa.gov/fisheye/fisheyelandingpage.html"
                        style="height:37px;border-radius:25px;margin: -24px -50px; float:top;position:absolute;right:-100px;
                              font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
                        padding-bottom:10px"> FISHEyE Homepage</a>' ),style='width:1000px')
        ), #end app level fluid row#, target="_blank"
        appFrameFooterScrolling()
    ) # end fluid Page
}

