
.libPaths("/usr/lib64/R/shiny_library/fisheye")

#detach(unload = TRUE)


library(shiny)
library(shinyjs)
library(shinyBS)
#library(ggplot2)
library(grid)
#library(dplyr)
# library(scales)
# library(DT)
library(appFrame)

##UI part of the app. The ui piece is not reactive and is used for setting up the permanent pieces of the app.
shinyUI(fluidPage(
  tags$head(
    # Main css page, downloaded from bootswatch
    tags$link(rel="stylesheet", type="text/css", href="bootstrap.css"),
    # secondary css page with fisheye specific attributes
    tags$link(rel="stylesheet", type="text/css", href="fisheye.css"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.4.0/css/font-awesome.min.css"),
    tags$style(type="text/css", ".tab-content {overflow: visible;}")),         
  # Add NOAA header
  appFrameHeaderScrolling(),
  navbarPage(id = "page", collapsible = T, inverse = F,
             title = "",
             tabPanel("Explore the data", value = "results",
                      sidebarLayout(
                        sidebarPanel(uiOutput("tabs"),
                                     uiOutput("download_Table")),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Plot", 
                                               conditionalPanel(condition = "input.tab_type == 'Summary'",
                                                                plotOutput("yearplot")),
                                               conditionalPanel(condition = "input.tab_type == 'By product type'",
                                                                plotOutput("productplot")), style = "min-height: 1000px"),
                                      tabPanel("Table", DT::dataTableOutput("table"))
                                      )
                          )
                        )),
             tabPanel("Information page",
                      source("description.R")$value),
             tabPanel(HTML('<i class="fa fa-thumb-tack" style="margin-right:2ex;display:inline-block;vertical-align:bottom;float:left;white-space:nowrap"> Bulletin Board</i>'),
                      fluidRow(
                        htmlOutput("BlogText")),
                        htmlOutput("BlogUpdates")),
             tabPanel(HTML('<i class="fa fa-envelope-o fa-fw" style="margin-right:9ex;display:inline-block;vertical-align:bottom;float:left;white-space:nowrap"> Contact us</i>'),
                      fluidRow(
                        htmlOutput("Email"))),
             tabPanel(HTML('<a class="btn btn-warning", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/"
                        style="height:37px;border-radius:25px;margin: -24px -50px; float:top;position:absolute;right:-100px;
                              font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
                        padding-bottom:10px"> FISHEyE Homepage</a>' ),style='width:1000px')
), 
                      
  # Add NOAA footer
  appFrameFooterScrolling()
  )
)