.libPaths("/usr/lib64/R/shiny_library/fisheye")

#detach(unload = TRUE)

library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(grid)
library(dplyr)
library(scales)
library(DT)

mini_whiting <- readRDS("mini_whiting.RDS")


# Data formatting for plot ####
data <- mini_whiting %>%
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
data_table <- mini_whiting %>%
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
sumTab <- filter(data, tab == 'Summary')
sumTab$Metric <- sort(sumTab$Metric, decreasing = T)
tacTab <- filter(data, tab == 'TACU' & Statistic %in% c('Utilization by weight', 'Utilization by percent'))
tacTabval <- filter(tacTab, !grepl('percent', Metric))
tacTabperc <- filter(tacTab, grepl('percent', Metric))

unique(tacTab$Metric)




## SERVER part of the app.####
# The server piece contains all reactive components.
shinyServer(function(input, output, session) {
  ##Reactive component of the sidebar using renderUI and uiOutput functions####
  ##This allows us to update the sidebar based on other inputs##
  
  ##Summary tab components####
  ##Summary tab: yaxis options
  output$yaxisInput <- renderUI({
        checkboxGroupInput("yaxisInput", "Metric", choices = unique(sumTab$Metric), selected = c('Production value','Total allowable catch, non-tribal','Purchase (or catch) weight'))
  })

  ##Summary tab: stat options
  output$statInput <- renderUI({
    radioButtons("statInput","Statistic", choices = unique(sumTab$Statistic), selected = "Total")
  })
  ##Summary tab: sector options
  output$sectorInput <- renderUI({
    checkboxGroupInput("sectorInput","Sector", choices = unique(sumTab$Sector), selected = c("Catcher-Processor","Mothership","Shoreside"))
  })
  
  ##Product tab components####
  ##Product tab: yaxis options (types of products). These are updated with the help of an 'observer'
  output$yaxis2Input <- renderUI({
    checkboxGroupInput("yaxis2Input", "Product types", choices = unique(prodTab$Metric) , selected = 'Production value (Surimi)')
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
  ##Product tab: sector options
  output$sector2Input <- renderUI({
    checkboxGroupInput("sector2Input","Sector", choices = unique(prodTab$Sector), selected = c("Catcher-Processor", "Mothership", "Shoreside"))
  })
  
  
  
  ##TAC tab components####
  
  ##TAC tab: select total or percent
  # output$valuetypeInput <- renderUI({
  #   selectInput(inputId = 'valuetypeInput',
  #               label = "Select a metric",
  #               choices = c('Total Utilization', 'Percent Utilization'),
  #               selectize = F)
  # })
  
  ##TAC tab: yaxis options
  output$yaxis3Input <- renderUI({
    checkboxGroupInput("yaxis3Input", "Metric", choices = c('Commercial catch','Unutilized allocation, post-reapportionment', 'Initial allocation'), selected = c('Commercial catch','Unutilized allocation, post-reapportionment'))
  })
  
  ##TAC tab: stat options
  output$stat3Input <- renderUI({
    radioButtons("stat3Input","Statistic", choices = c("Utilization by weight", "Utilization by percent"), selected = "Utilization by weight")
  })
  ##TAC tab: sector options
  output$sector3Input <- renderUI({
    checkboxGroupInput("sector3Input","Sector", choices = unique(tacTab$Sector), selected = c("Catcher-Processor", "Mothership", "Shoreside"))
  })
  
  

  
  # Download button#####
  output$download_Table <- renderUI({
    tags$div(class = "actbutton",
             downloadButton("dlTable", "Download Data Table", class = "btn btn-info"))
  })
  
  ##Putting the summary and product tab together
  output$tabs <- renderUI({
    tabsetPanel(
      tabPanel("Summary", 
               uiOutput("yaxisInput"),
               uiOutput("statInput"),
               uiOutput("sectorInput")),
      tabPanel("By product type",
               uiOutput("producttypeInput"),
               uiOutput("yaxis2Input"),
               uiOutput("stat2Input"),
               uiOutput("sector2Input")),
      tabPanel("Total Allowable Catch Utilization",
               uiOutput("yaxis3Input"),
               uiOutput("stat3Input"),
               uiOutput("sector3Input")),
      id = "tab_type", type = c("tabs"))
  })
  
  ##Observer####
  ##Observe the selection of metric within the product type tab
  ##I was unable to get the list to update based on the selectInput without using observe
  observe({
    if(is.null(input$producttypeInput)) {
        return() 
      } else if(input$producttypeInput == 'Production value') {
      updateCheckboxGroupInput(session, "yaxis2Input", "Product types", choices = unique(prodTabval$Metric), selected = 'Surimi (Production value)')
    } else if(input$producttypeInput == 'Production weight') {
      updateCheckboxGroupInput(session, "yaxis2Input", "Product types", choices = unique(prodTabwt$Metric), selected = 'Surimi (Production weight)')
    } else if (input$producttypeInput == 'Production price (per lb)') {
      updateCheckboxGroupInput(session, "yaxis2Input", "Product types", choices = unique(prodTabprice$Metric), selected = 'Surimi (Production price (per lb))')
    } 
  })
  
  # 
##TAC version  
# 
#   observe({
#    if(input$valuetypeInput == 'Total Utilization') {
#       updateCheckboxGroupInput(session, "yaxis3Input", "Metric", choices = unique(tacTabval$Metric), selected = c('Commercial catch','Unutilized allocation, post-reapportionment'))
#     } else if(input$producttypeInput == 'Percent Utilization') {
#       updateCheckboxGroupInput(session, "yaxis3Input", "Metric", choices = unique(tacTabperc$Metric), selected = c('Percent Commercial catch', 'Percent Unutilized allocation, post-reapportionment'))
#     }
#   })

  
  ##creating the dataframe for graph#####
  ##Use reactive to reactively filter the dataframe based on inputs
  filtered <- reactive({
    if(input$tab_type == "Summary") {
        data %>%
          filter(Metric %in% input$yaxisInput,
                 Statistic == input$statInput,
                 Sector %in% input$sectorInput)
    } else if(input$tab_type == "By product type") {
      data %>%
        filter(Metric %in% input$yaxis2Input,
               Statistic == input$stat2Input,
               Sector %in% input$sector2Input) 
    } else if (input$tab_type == "Total Allowable Catch Utilization") {
      data %>%
        filter(Metric %in% input$yaxis3Input,
               Statistic == input$stat3Input,
               Sector %in% input$sector3Input) 
    }
  }) 
  



  #creating the dataframe for data table#####
  ##Use reactive to reactively filter the dataframe based on inputs
  filtered_dt <- reactive({
      if(input$tab_type == "Summary") {
        data_table %>%
          filter(Metric %in% input$yaxisInput,
                 Statistic == input$statInput,
                 Sector %in% input$sectorInput)
      } else if(input$tab_type == "By product type") {
        data_table %>%
          filter(Metric %in% input$yaxis2Input,
                 Statistic == input$stat2Input,
                 Sector %in% input$sector2Input) 
      } else if(input$tab_type == "Total Allowable Catch Utilization") {
        data_table %>%
          filter(Metric %in% c(c('Commercial catch', 'Initial allocation', 'Final allocation')),
                 Statistic == input$stat3Input,
                 Sector %in% input$sector3Input) %>% 
          dplyr::select(-Variance, -N, -q25, -q75)
      }
    })
  dt_dat <- reactive({
    dat <- filtered_dt()
    tabformatfun <- function(x) {
      rounding <- case_when(
        any(dat$Value < 1) ~ 2, 
        all(dat$unit == '') ~ 1, T ~ 0)
      dollar   <- ifelse(grepl('$', dat$ylab, fixed = T), '$', '')
      
      val = formatC(x, format = 'f', dig = rounding, big.mark = ',')
      
      return(val)
    }
    dat$Value <-    tabformatfun(dat$Value)
    dat$Variance <- tabformatfun(dat$Variance)
    dat$q25 <-      tabformatfun(dat$q25)
    dat$q75 <-      tabformatfun(dat$q75)
    
    valuetitle <- ifelse(any(dat$Statistic == ''), 'Value', as.character(unique(dat$Statistic)))
    vartitle <- ifelse(input$statInput %in% c('Total', ''), 'Variance',
                       ifelse(input$statInput == 'Median', 'Mean average deviation',
                              'Standard deviation'))
    typetitle <- ifelse(input$Sect_sel == "FR", 'Processor type', 'Vessel type')
    
    # rename the columns 
    dat <- dat %>% 
      rename_with(~case_when(. == "q25" ~ "Quartile: 25th",
                             . == "q75" ~ "Quartile: 75th",
                             . == "N" ~ "Number of processors",
                             T ~ .)) %>%
                    rename(  !!quo_name(valuetitle)       := Value,
                             !!quo_name(vartitle)         := Variance)
            
    # rename_with(
    #   ~ case_when(
    #     . == "mpg" ~ "MPG",
    #     . == "cyl" ~ "CYL",
    #     . == "bla" ~ "uyhgfrtgf",
    #     TRUE ~ .))
    
    alwaysexclude <- c('unit', 'tab', 'ylab', 'Order', 'Statistic')
    dat <- select(dat, colnames(dat)[apply(dat, 2, function(x) sum(x != '' & x != ' NA' & !is.na(x) & x != 'NA') > 0 )], 
                     -alwaysexclude) 
    
    return(dat)
  })
  
  
  # 
  # Year                          = Year,
  # Sector                        = Sector,
  # !!quo_name(valuetitle)       := Value,
  # !!quo_name(vartitle)         := Variance,
  # `Quartile: 25th`              = q25,
  # `Quartile: 75th`              = q75,
  # `Number of processors`        = N)
  ##Preparing plots####
  #greyless_ballard_terminal_pal <- c('#4C384C','#BC8787', '#CB6D4F', '#D49F12','#691C32', '#79863C', '#8F9EBD', '#33647F',  '#0C2340')
  greyless_mountains_pal <- c('#C1052F','#D89B2C', '#C0CB81',
                              '#648C1C','#6FB1C9',
                              '#001B70','#595478',
                              '#C0B3B6','#B56C97')
  #plot(rep(1,9), col=(greyless_mountains_pal), pch=19,cex=8)
  
  ##set colors for lines
  lineColor <- c(
    'Catcher-Processor' = '#001B70',
    'Mothership' = '#C1052F',
    'Shoreside' = '#648C1C',
    'All' = '#D89B2C'
    # 'Purchase cost' = '#595478',
    # 'Purchase (or catch) weight' = '#595478',
    # 'Purchase price per lb' = '#595478',
    # 'Production value' = '#D89B2C',
    # 'Production weight' = '#D89B2C',
    # 'Production price per lb' = '#D89B2C',
    # "Surimi (Production value)" = '#C1052F',
    # "Surimi (Production weight)" = '#C1052F',
    # "Surimi (Production price (per lb))" = '#C1052F',
    # "Fillet (Production value)" = '#D89B2C',
    # "Fillet (Production weight)" = '#D89B2C',
    # "Fillet (Production price (per lb))" = '#D89B2C',
    # "Headed-and-gutted (Production value)" = '#C0CB81',
    # "Headed-and-gutted (Production weight)" = '#C0CB81',
    # "Headed-and-gutted (Production price (per lb))" = '#C0CB81',
    # "Unprocessed (Production value)" = '#648C1C',
    # "Unprocessed (Production weight)" = '#648C1C',
    # "Unprocessed (Production price (per lb))" = '#648C1C',
    # "Fishmeal (Production value)" = '#6FB1C9',
    # "Fishmeal (Production weight)" = '#6FB1C9',
    # "Fishmeal (Production price (per lb))" = '#6FB1C9',
    # "Fish oil (Production value)" = '#001B70',
    # "Fish oil (Production weight)" = '#001B70',
    # "Fish oil (Production price (per lb))" = '#001B70',
    # "Minced (Production value)" = '#595478',
    # "Minced (Production weight)" = '#595478',
    # "Minced (Production price (per lb))" = '#595478',
    # "Other (Production value)" = '#C0B3B6',
    # "Other (Production weight)" = '#C0B3B6',
    # "Other (Production price (per lb))" = '#C0B3B6',
    # "Round (Production value)" = '#B56C97',
    # "Round (Production weight)" = '#B56C97',
    # "Round (Production price (per lb))" = '#B56C97'
  )
  
  #tac color 
  tacColor <- c('Unutilized allocation, post-reapportionment' = '#D8DEE9',
                'Commercial catch' = '#4C566A', 
                'Initial allocation' = 'white')
  
  ##Defining standard plot elements
  point_size <- 4
  line_size <- 0.75
  
  ##Plot for when year is on the x-axis in the summary tab
  output$yearplot <- renderPlot({
    if(is.null(filtered())){
      return()
    } 
      ggplot(filtered(),
             aes(x = Year,
                 y = Value,
                 group = Sector)) +
        scale_fill_manual(values = lineColor) +
        scale_color_manual(values = lineColor) +
        theme_minimal() +
        theme(text = element_text(size = 14),
              axis.text = element_text(size = 12),
              strip.text = element_text(size = 14)) +
        geom_point(aes(color = Sector), size = point_size) +
        geom_line(aes(color = Sector), size = line_size) +
        geom_ribbon(aes(ymax = upper,
                        ymin = lower,
                        fill = Sector), alpha = .25) +
        facet_wrap(~ylab, scales = 'free_y', ncol = 2) +
        labs(y = input$statInput) +
        scale_x_continuous(breaks= pretty_breaks())
  }, height = 800, width = 1100)
  
  
  ##Plot for the product tab
  output$productplot <- renderPlot({
    if(is.null(filtered())) {
      return()
    }
      ggplot(filtered(),
             aes(x = Year,
                 y = Value,
                 group = Sector)) +
        scale_fill_manual(values = lineColor) +
        scale_color_manual(values = lineColor) +
        theme_minimal() +
        theme(text = element_text(size = 14),
              axis.text = element_text(size = 12),
              strip.text = element_text(size = 14)) +
        geom_point(aes(color = Sector), size = point_size) +
        geom_line(aes(color = Sector), size = line_size) +
        geom_ribbon(aes(ymax = upper,
                        ymin = lower,
                        fill = Sector), alpha = .25) +
        facet_wrap(~ylab, scales = 'free_y', ncol = 2) +
        labs(y = input$stat2Input) +
        scale_x_continuous(breaks= pretty_breaks())
  }, height = 800, width = 1100)
  
  ##Plot for Utilized TAC

  output$tacplot <- renderPlot({
    if(is.null(filtered())){
      return()
    }
    
    dat <- filtered() 
    
    dat$Metric <- factor(dat$Metric, levels = c('Unutilized allocation, post-reapportionment', 'Commercial catch', 'Initial allocation'))  %>%  droplevels()
    
    ylabel <- dat$ylab
    
    ggplot(dat, aes(x = Year,
                    y = Value,
                    group = Metric, 
                    fill = Metric)) +
      scale_fill_manual(values = tacColor) +
      theme_minimal() +
      theme(text = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14)) +
      geom_bar(data = subset(dat, Metric != 'Initial allocation'), position ="stack", stat="identity", width=0.8) +
      geom_bar(data = subset(dat, Metric == 'Initial allocation'), color = "grey95", position ="stack", stat="identity", alpha = 0.01, size = 1.25, width=0.9) +
      
      facet_wrap(Sector ~ Statistic, ncol = 2) +
      labs(y = ylabel) +
      scale_x_continuous(breaks= pretty_breaks())
  }, height = 800, width = 1100)
  
  
  ##Creating the data table
  output$table <- DT::renderDataTable({
    datatable(dt_dat(), 
              rownames = FALSE)
  })
  
  

  
  
  # Creating download buttons
  output$dlTable <- downloadHandler(
    filename = function() { 'whitingTable.csv' },
    content = function(file) {
      table <- filtered_dt()
      row.names(table) <- NULL
      table$source <- ""
      names(table)[names(table) == 'source'] <- "Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC"
      write.csv(table, file)
    })
  
  output$Email <- renderUI({
    withTags(
      div(style = "margin: 15px 15px 30px; width: 60%",
          h3("Contact us"),
          p("We look forward to receiving feedback and questions.", br()),
          p(h4("Please email us at", strong("nmfs.nwfsc.fisheye@noaa.gov")),
            # tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'nwfsc.fisheye@noaa.gov'),
            br()),
          
          p(
            "Erin Steiner", br(),
            "Economist", br(),
            "206-860-3202", br(),
            "Northwest Fisheries Science Center", br(),br(),
            #                    "nwfsc.fisheye@noaa.gov",
            
            "Amanda Phillips", br(),
            "Contractor-ECS Federal, Inc.", br(),
            "In support of NMFS", br(),
            "Northwest Fisheries Science Center", br(),
            #                    "nwfsc.fisheye@noaa.gov", tags$br(),
            br(),
            hr(),
            
            # "You can send comments and questions directly to us by clicking",
            #                 tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'contact us'), 'or by copying our email address',
            #                 tags$em('nwfsc.fisheye@noaa.gov'), 'and using your favorite email program.',
            br(),
            br()
          ))
    ) #end withTags
  })
  
  output$BlogText <- renderUI({
    withTags(
      div(style = "margin-top: 15px;margin-bottom:0; width: 80%; ",
          h3("Bulletin Board"),
          p("On this page we will provide information on updates to the application."),
          hr()
      )
    ) #end withTags
  }) #        br(),
  ####################################
  
  #       ,
  ####################################
  #Blog
  ####################################
  output$BlogUpdates <- renderUI({ 
    withTags(
      div(style = "margin-top:0; padding-top:0;background-color:#F8F8E9;",
          h3("Updates"),
          # Welcome to the whiting application
          div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
               # step 1: update date
               HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Dec</span><br />
            <span class='day' style='font-size:16px;'>20</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2019</span>")
          ),
          # step 2: update title
          p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Welcome to the Whiting purchase and production app</span>")), 
          # step 3: update message
          p(br(),"Updates, improvements, and revisions to the application will be posted here."),
          hr(),
          br(),
          
          hr()#, 
          
      )
    )
  })
  
  ####################################
})

