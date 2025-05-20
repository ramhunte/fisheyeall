.libPaths(c("/usr/lib64/R/shiny_library/fisheye", .libPaths()))

library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(grid)
library(dplyr)
library(scales)
library(DT)
library(shinyWidgets)

mini_purcprod <- readRDS("mini_purcprod.RDS")

# Data formatting for plot ####
data <- mini_purcprod %>%
  mutate(Variance = case_when(
    unit == '' ~ Variance,
    unit == 'thousands' ~ Variance/1e3,
    unit == 'millions' ~ Variance/1e6,
    unit == 'billions' ~ Variance/1e9,
    T ~ -999),
    q25 = case_when(
      unit == '' ~ q25,
      unit == 'thousands' ~ q25/1e3,
      unit == 'millions' ~  q25/1e6,
      unit == 'billions' ~  q25/1e9,
      T ~ -999),
    q75 = case_when(
      unit == '' ~ q75,
      unit == 'thousands' ~ q75/1e3,
      unit == 'millions' ~  q75/1e6,
      unit == 'billions' ~  q75/1e9,
      T ~ -999),
    Value = case_when(
      unit == '' ~ Value,
      unit == 'thousands' ~ Value/1e3,
      unit == 'millions' ~ Value/1e6,
      unit == 'billions' ~ Value/1e9,
      T ~ -999),
    upper = case_when(Statistic == 'Mean' ~ Value + Variance,
                      Statistic == 'Median' ~ q75,
                      Statistic == 'Total' ~ Value),
    lower = case_when(Statistic == 'Mean' ~ Value - Variance,
                      Statistic == 'Median' ~ q25,
                      Statistic == 'Total' ~ Value)) %>%
  data.frame()

# Data formatting for table#####
data_table <- mini_purcprod %>%
  mutate(Variance = round(Variance, 2),
         q25 = round(q25,2),
         q75 = round(q75,2),
         Value = round(Value, 2)) %>%
  data.frame()

# Filter by tab and metric for easy use in sidebar setup#####
prodTab <- filter(data, tab == 'Product')
prodTabval <- filter(prodTab, grepl('Production value', Metric))
prodTabwt <- filter(prodTab, grepl('Production weight', Metric))
prodTabprice <- filter(prodTab, grepl('Production price', Metric))
prodTabpval <- filter(prodTab, grepl('Percent by value', Metric))
prodTabpwt <- filter(prodTab, grepl('Percent by weight', Metric))


sumTab <- filter(data, tab == 'Summary')
sumTab$Metric <- sort(sumTab$Metric, decreasing = T)
# Couldn't figure out a better way to order than listing. 
# If ordered as factor didn't work, still listed as alpha
Tabprodgrnd <- c(
  "All production",
  "Groundfish production",
  "Pacific whiting",
  "Non-whiting groundfish", 
  "Sablefish",
  "Rockfish",
  "Dover sole",
  "Petrale sole",
  "Thornyheads",
  "Other groundfish species"
)

Tabprodother <- c(
  "Other species production",
  "Crab",
  "Shrimp",
  "Salmon",
  "Tuna",
  "Coastal pelagics",
  "Other shellfish",
  "Other species"
)

Tabregion <- c('California','Washington and Oregon')
Tabsize <- c('Small','Medium','Large','Non-processor')
TabCS <- c('All production','Groundfish production','Other species production')


## SERVER part of the app.####
# The server piece contains all reactive components.
shinyServer(function(input, output, session) {
  ##Reactive component of the sidebar using renderUI and uiOutput functions####
  ##This allows us to update the sidebar based on other inputs##
  
  ##Summary tab components####
  ##Summary tab: yaxis options
  output$metricInput <- renderUI({
        checkboxGroupInput("metricInput", "Metric", choices = unique(sumTab$Metric), selected = 'Purchase weight')
  })

  ##Summary tab: stat options
  output$statInput <- renderUI({
    radioButtons("statInput","Statistic", choices = unique(sumTab$Statistic), selected = "Median")
  })
  ##Summary tab: production options
  output$prodInput <- renderUI({
    tags$div(class = 'prodlist',
    checkboxGroupInput("prodInput","", choices = Tabprodgrnd, selected = "All production"))
  })
  
  output$prodotrInput <- renderUI({
    tags$div(class = 'prodotrlist',
    checkboxGroupInput("prodotrInput","", choices = Tabprodother, selected = ""))
  })
  
  ##Summary tab: region options
  output$regionInput <- renderUI({
    checkboxGroupInput("regionInput","", choices = Tabregion, selected = "California")
  })
  
  ##Summary tab: size/type options
  output$sizeInput <- renderUI({
    checkboxGroupInput("sizeInput","", choices = Tabsize, selected = "Small")
  })
  
  ##Summary tab: production options for region/size
  output$cs1Input <- renderUI({
    radioButtons("cs1Input","Production activities", choices = TabCS, selected = "All production")
  })
  ##Summary tab: production options for region/size
  output$cs2Input <- renderUI({
    radioButtons("cs2Input","Production activities", choices = TabCS, selected = "All production")
  })
  
  
  
  ##Product tab components####
  ##Product tab: yaxis options (types of products). These are updated with the help of an 'observer'
  output$metric2Input <- renderUI({
    checkboxGroupInput("metric2Input", "Product types", choices = unique(prodTab$Metric) , selected = 'Canned (Production value)')
  })
  ##Product tab: select type of metric (production weight, value, value per lb)
  output$producttypeInput <- renderUI({
    selectInput(inputId = 'producttypeInput',
                label = "Select a metric",
                choices = c('Production value', 'Production weight', 'Production price (per lb)'),
                selectize = F)
  })
  
  ##Product tab: stat options
  output$stat2Input <- renderUI({
    radioButtons("stat2Input","Statistic", choices = unique(prodTab$Statistic), selected = "Median")
  })
 
  ##Product tab: production options
  output$prod2Input <- renderUI({
    tags$div(class = 'prodlist',
             checkboxGroupInput("prod2Input","", choices = Tabprodgrnd, selected = "All production"))
  })
  
  output$prodotr2Input <- renderUI({
    tags$div(class = 'prodotrlist',
             checkboxGroupInput("prodotr2Input","", choices = Tabprodother, selected = ""))
  })
  
  ##Product tab: region options
  output$region2Input <- renderUI({
    checkboxGroupInput("region2Input","", choices = Tabregion, selected = "California")
  })
  
  ##Product tab: size/type options
  output$size2Input <- renderUI({
    checkboxGroupInput("size2Input","", choices = Tabsize, selected = "Small")
  })
  
  ##Product tab: production options for region/size
  output$cs3Input <- renderUI({
    radioButtons("cs3Input","Production activities", choices = TabCS, selected = "All production")
  })
  output$cs4Input <- renderUI({
    radioButtons("cs4Input","Production activities", choices = TabCS, selected = "All production")
  })
  
  ##Putting the summary and product tab together
  output$tabs <- renderUI({
    tabsetPanel(
      tabPanel("Summary", 
               uiOutput("metricInput"),
               uiOutput("statInput"),
               tabsetPanel(
                 tabPanel("Production activities",
                        uiOutput("prodInput"),
                        dropdownButton(
                          label = tags$b("Other species production"),
                          circle = F,
                          uiOutput("prodotrInput")
                 )),
                 tabPanel("Region",
                        uiOutput("regionInput"),
                        uiOutput("cs1Input")),
                 tabPanel("Processor size/type",
                        uiOutput("sizeInput"),
                        uiOutput("cs2Input")),
                 id = "grp_tab_type", type = c("tabs"))),
      tabPanel("By product type",
               uiOutput("producttypeInput"),
               uiOutput("metric2Input"),
               uiOutput("stat2Input"),
               tabsetPanel(
                 tabPanel("Production activities",
                          uiOutput("prod2Input"),
                          dropdownButton(
                            label = tags$b("Other species production"),
                            circle = F,
                            uiOutput("prodotr2Input")
                            )),
                 tabPanel("Region",
                          uiOutput("region2Input"),
                          uiOutput("cs3Input")),
                 tabPanel("Processor size/type",
                          uiOutput("size2Input"),
                          uiOutput("cs4Input")),
                 id = "grp_tab_type2", type = c("tabs"))),
      id = "tab_type", type = c("tabs"))
  })
  
  ##Observer####
  ##Observe the selection of metric within the product type tab
  ##I was unable to get the list to update based on the selectInput without using observe
  observe({
    if(is.null(input$producttypeInput)) {
      return()
    } else if(input$producttypeInput == 'Production value') {
      updateCheckboxGroupInput(session, "metric2Input", "Product types", choices = unique(prodTabval$Metric), selected = 'Canned (Production value)')
    } else if(input$producttypeInput == 'Production weight') {
      updateCheckboxGroupInput(session, "metric2Input", "Product types", choices = unique(prodTabwt$Metric), selected = 'Canned (Production weight)')
    } else if (input$producttypeInput == 'Production price (per lb)') {
      updateCheckboxGroupInput(session, "metric2Input", "Product types", choices = unique(prodTabprice$Metric), selected = 'Canned (Production price (per lb))')
    }
  })
  
  
  ##creating the dataframe for graph#####
  ##Use reactive to reactively filter the dataframe based on inputs
  filtered <- reactive({
    if(input$tab_type == "Summary") {
      if(input$grp_tab_type == 'Production activities') {
        data %>%
          filter(Metric %in% c(input$metricInput),
                 Statistic == input$statInput,
                 Variable %in% c(input$prodInput, input$prodotrInput))
      } else if(input$grp_tab_type == 'Region') {
        data %>%
          filter(Metric %in% c(input$metricInput),
                 Statistic == input$statInput,
                 Variable %in% c(input$regionInput),
                 CS == input$cs1Input)
      } else {
        data %>%
          filter(Metric %in% c(input$metricInput),
                 Statistic == input$statInput,
                 Variable %in% c(input$sizeInput),
                 CS == input$cs2Input)
      }
    } else {
      if(input$grp_tab_type2 == 'Production activities') {
        data %>%
          filter(Metric %in% c(input$metric2Input),
                 Statistic == input$stat2Input,
                 Variable %in% c(input$prod2Input, input$prodotr2Input))
      } else if(input$grp_tab_type2 == 'Region') {
        data %>%
          filter(Metric %in% c(input$metric2Input),
                 Statistic == input$stat2Input,
                 Variable %in% c(input$region2Input),
                 CS == input$cs3Input)
      } else {
        data %>%
          filter(Metric %in% c(input$metric2Input),
                 Statistic == input$stat2Input,
                 Variable %in% c(input$size2Input),
                 CS == input$cs4Input)
      }
    }
  })
  
  #creating the dataframe for data table#####
  ##Use reactive to reactively filter the dataframe based on inputs
  filtered_dt <- reactive({
      if(input$tab_type == "Summary") {
        if(input$grp_tab_type == 'Production activities') {
          data_table %>%
            filter(Metric %in% c(input$metricInput),
                   Statistic == input$statInput,
                   Variable %in% c(input$prodInput, input$prodotrInput))
        } else if(input$grp_tab_type == 'Region') {
          data_table %>%
            filter(Metric %in% c(input$metricInput),
                   Statistic == input$statInput,
                   Variable %in% c(input$regionInput),
                   CS == input$cs1Input)
        } else {
          data_table %>%
            filter(Metric %in% c(input$metricInput),
                   Statistic == input$statInput,
                   Variable %in% c(input$sizeInput),
                   CS == input$cs2Input)
        }
      } else {
        if(input$grp_tab_type2 == 'Production activities') {
          data_table %>%
            filter(Metric %in% c(input$metric2Input),
                   Statistic == input$stat2Input,
                   Variable %in% c(input$prod2Input, input$prodotr2Input))
        } else if(input$grp_tab_type2 == 'Region') {
          data_table %>%
            filter(Metric %in% c(input$metric2Input),
                   Statistic == input$stat2Input,
                   Variable %in% c(input$region2Input),
                   CS == input$cs3Input)
        } else {
          data_table %>%
            filter(Metric %in% c(input$metric2Input),
                   Statistic == input$stat2Input,
                   Variable %in% c(input$size2Input),
                   CS == input$cs4Input)
        }
      }
    })
  
  ##Preparing plots####
  #greyless_ballard_terminal_pal <- c('#4C384C','#BC8787', '#CB6D4F', '#D49F12','#691C32', '#79863C', '#8F9EBD', '#33647F',  '#0C2340')
  greyless_mountains_pal <- c('#C1052F','#D89B2C', '#C0CB81',
                              '#648C1C','#6FB1C9',
                              '#001B70','#595478',
                              '#C0B3B6','#B56C97')
  #plot(rep(1,9), col=(greyless_mountains_pal), pch=19,cex=8)
  
  ##set colors for lines
  lineColor <- c(
    "All production" = "black",
    "Groundfish production" = '#C1052F',
    "Pacific whiting" = '#D89B2C',
    "Non-whiting groundfish" = '#C0CB81', 
    "Sablefish" = '#648C1C',
    "Rockfish" = '#6FB1C9',
    "Dover sole" = '#001B70',
    "Petrale sole" = '#595478',
    "Thornyheads" = '#C0B3B6',
    "Other groundfish species" = '#B56C97',
    "Other species production" = '#C1052F',
    "Crab" = '#D89B2C',
    "Shrimp" = '#C0CB81',
    "Salmon" = '#648C1C',
    "Tuna" = '#6FB1C9',
    "Coastal pelagics" = '#001B70',
    "Other shellfish" =  '#595478',
    "Other species" = '#C0B3B6',
    "California" = '#001B70',
    "Washington and Oregon" = '#C1052F',
    "Small" = '#001B70',
    "Medium" = '#C1052F',
    "Large" = '#648C1C',
    "Non-processor" = '#D89B2C'
  )
  
  lineType <- c(
    "California" = 'solid',
    "Washington and Oregon" = 'solid',
    "Small" = 'solid',
    "Medium" = 'solid',
    "Large" = 'solid',
    "Non-processor" = 'solid',
    "All production" = "solid",
    "Groundfish production" = 'solid',
    "Pacific whiting" = 'solid',
    "Non-whiting groundfish" = 'solid', 
    "Sablefish" = 'solid',
    "Rockfish" = 'solid',
    "Dover sole" = 'solid',
    "Petrale sole" = 'solid',
    "Thornyheads" = 'solid',
    "Other groundfish species" = 'solid',
    "Other species production" = 'dashed',
    "Crab" = 'dashed',
    "Shrimp" = 'dashed',
    "Salmon" = 'dashed',
    "Tuna" = 'dashed',
    "Coastal pelagics" = 'dashed',
    "Other shellfish" =  'dashed',
    "Other species" = 'dashed'
  )
  
  ##Defining standard plot elements
  point_size <- 4
  line_size <- 0.75
  
  ##Plot for the summary tab
  output$sumplot <- renderPlot({
    if(is.null(filtered())){
      return()
    } 
      ggplot(filtered(),
             aes(x = Year,
                 y = Value,
                 group = Variable)) +
        scale_fill_manual(values = lineColor) +
        scale_color_manual(values = lineColor) +
        scale_linetype_manual(values = lineType) +
        theme_minimal() +
        theme(text = element_text(size = 14),
              axis.text = element_text(size = 12),
              strip.text = element_text(size = 14)) +
        geom_point(aes(color = Variable), size = point_size) +
        geom_line(aes(color = Variable, linetype = Variable), size = line_size) +
        geom_ribbon(aes(ymax = upper,
                        ymin = lower,
                        fill = Variable), alpha = .25) +
        facet_wrap(~ylab, scales = 'free_y', ncol = 2) +
        labs(y = input$statInput) +
        scale_x_continuous(breaks= pretty_breaks())
  }, height = 800, width = 1100)
  
  
  ##Plot for the product tab
  output$productplot <- renderPlot({
    if(is.null(filtered())){
      return()
    } 
    ggplot(filtered(),
           aes(x = Year,
               y = Value,
               group = Variable)) +
      scale_fill_manual(values = lineColor) +
      scale_color_manual(values = lineColor) +
      scale_linetype_manual(values = lineType) +
      theme_minimal() +
      theme(text = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14)) +
      geom_point(aes(color = Variable), size = point_size) +
      geom_line(aes(color = Variable, linetype = Variable), size = line_size) +
      geom_ribbon(aes(ymax = upper,
                      ymin = lower,
                      fill = Variable), alpha = .25) +
      facet_wrap(~ylab, scales = 'free_y', ncol = 2) +
      labs(y = input$stat2Input) +
      scale_x_continuous(breaks= pretty_breaks())
  }, height = 800, width = 1100)
  
  ##Creating the data table
  output$table <- DT::renderDataTable({
    filtered_dt() %>%
      select(-c(tab,ylab,unit))
  })
})