.libPaths(c("/usr/lib64/R/shiny_library/fisheye", .libPaths()))

library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(grid)
library(dplyr)
library(scales)
library(DT)

##UI part of the app. The ui piece is not reactive and is used for setting up the permanent pieces of the app.
shinyUI(fluidPage(
  titlePanel("FISHEyE: Shoreside purchase and production mini-app"),
  tags$style(HTML(".prodlist .checkbox:first-child label,
                  .prodlist .checkbox:nth-child(2) label,
                  .prodlist .checkbox:nth-child(4) label{font-weight:bold;}")),
  tags$style(HTML(".prodotrlist .checkbox:first-child label{font-weight:bold}")),
  sidebarLayout(
    sidebarPanel(uiOutput("tabs")),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", 
                           conditionalPanel(condition = "input.tab_type == 'Summary'",
                                            plotOutput("sumplot")),
                           conditionalPanel(condition = "input.tab_type == 'By product type'",
                                            plotOutput("productplot"))),
                  tabPanel("Table", DT::dataTableOutput("table"))
      )
    )
  ))
)