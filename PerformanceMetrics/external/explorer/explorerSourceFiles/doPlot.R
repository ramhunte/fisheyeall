doPlot <- function(dat, x, y) {
  if (PermitPlot()) {
    #removeNAs
    #dat <- subset(dat, is.na(dat$VALUE) == FALSE)
    ##Prepping data for plotting by converting to more "plot friendly" values
    dat4plot <- dat %>%
      mutate(VARIANCE = case_when(
        unit == '' ~ VARIANCE,
        unit == 'thousands' ~ VARIANCE/1e3,
        unit == 'millions' ~ VARIANCE/1e6,
        unit == 'billions' ~ VARIANCE/1e9,
        T ~ -999),
        q25 = case_when(
          unit == '' ~ q25,
          unit == 'thousands' ~ q25/1e3,
          unit == 'millions' ~ q25/1e6,
          unit == 'billions' ~ q25/1e9,
          T ~ -999),
        q75 = case_when(
          unit == '' ~ q75,
          unit == 'thousands' ~ q75/1e3,
          unit == 'millions' ~ q75/1e6,
          unit == 'billions' ~ q75/1e9,
          T ~ -999),
        VALUE = case_when(
          unit == '' ~ VALUE,
          unit == 'thousands' ~ VALUE/1e3,
          unit == 'millions' ~ VALUE/1e6,
          unit == 'billions' ~ VALUE/1e9,
          T ~ -999))

    dat4plot$sort2 <- if (!input$LayoutSelect) {
      reorder(dat4plot$VARIABLE, dat4plot$sort)
    } else {
        reorder(dat4plot$ylab, dat4plot$sort)
      }

    dat4plot$upper <-
      if (input$Ind_sel == "Economic") {
        if (input$AVE_MED == 'A') {
          dat4plot$VALUE + dat4plot$VARIANCE
        } else if (input$AVE_MED == 'T') {
          dat4plot$VALUE
        } else {
          dat4plot$q75
        }} else if (input$Ind_sel == 'Cost') {
          if (input$AVE_MED_COSTS == 'A') {
            dat4plot$VALUE + dat4plot$VARIANCE
          } else if (input$AVE_MED_COSTS == 'T') {
            dat4plot$VALUE
          } else {
            dat4plot$q75
          }
        } else if (input$Ind_sel == 'Other') {
          if (input$otherStats == 'Mean') {
            dat4plot$VALUE + dat4plot$VARIANCE
          } else if (input$otherStats == 'Total') {
            dat4plot$VALUE
          } else {
            dat4plot$q75
          }
        } else if (input$Ind_sel == 'Labor') {
          if(input$crewStats == 'Mean') {
            dat4plot$VALUE + dat4plot$VARIANCE
          } else if (input$crewStats == 'Total') {
            dat4plot$VALUE
          } else {
            dat4plot$q75
          }
        } else if (input$Ind_sel == 'Vessel characteristics' || input$Ind_sel == 'Processor characteristics') {
          if (input$demStats == 'Mean') {
            dat4plot$VALUE + dat4plot$VARIANCE
          } else if (input$demStats == 'Total') {
            dat4plot$VALUE
          } else {
            dat4plot$q75
          }
        } else if (input$Ind_sel == 'Impacts') {
    dat4plot$VALUE
  }
    
    
    dat4plot$lower <-
      if (input$Ind_sel == "Economic") {
        if (input$AVE_MED == 'A') {
          dat4plot$VALUE - dat4plot$VARIANCE
        } else if (input$AVE_MED == 'T') {
          dat4plot$VALUE
        } else  {
          dat4plot$q25
        }
      } else if (input$Ind_sel == 'Cost') {
        if (input$AVE_MED_COSTS == 'A') {
          dat4plot$VALUE - dat4plot$VARIANCE
        } else if (input$AVE_MED_COSTS == 'T') {
          dat4plot$VALUE
        } else {
          dat4plot$q25
        }
      } else if (input$Ind_sel == 'Other') {
        if (input$otherStats == 'Mean') {
          dat4plot$VALUE - dat4plot$VARIANCE
        } else if (input$otherStats == 'Total') {
          dat4plot$VALUE
        } else {
          dat4plot$q25
        }
      } else if (input$Ind_sel == 'Labor') {
        if (input$crewStats == 'Mean') {
          dat4plot$VALUE - dat4plot$VARIANCE
        } else if (input$crewStats == 'Total') {
          dat4plot$VALUE
        } else {
          dat4plot$q25
        }
      } else if (input$Ind_sel == 'Vessel characteristics' || input$Ind_sel == 'Processor characteristics') {
        if (input$demStats == 'Mean') {
          dat4plot$VALUE - dat4plot$VARIANCE
        } else if (input$demStats == 'Total') {
          dat4plot$VALUE
        } else  {
          dat4plot$q25
        }
      } else if (input$Ind_sel == 'Impacts') {
    dat4plot$VALUE
  }
    
    upper <- function() {
      if (input$Ind_sel == "Economic") {
          if (input$AVE_MED == 'A') {
            max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
          } else if (input$AVE_MED == 'M') {
            max(dat4plot$q75, na.rm = T)
          } else  {
            max(dat4plot$VALUE, na.rm = T)
          } 
      } else if (input$Ind_sel == 'Cost') {
          if (input$AVE_MED_COSTS == 'A') {
            max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
          } else if (input$AVE_MED_COSTS == 'M') {
            max(dat4plot$q75, na.rm = T)
          } else  {
            max(dat4plot$VALUE, na.rm = T)
          }
      } else if (input$Ind_sel == 'Other') {
          if (input$otherStats == 'Mean') {
            max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
          } else if (input$otherStats == 'Median') {
            max(dat4plot$q75, na.rm = T)
          } else {
            max(dat4plot$VALUE, na.rm = T)
          }
      } else if (input$Ind_sel == 'Labor') {
          if (input$crewStats == 'Mean') {
            max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
          } else if (input$crewStats == 'Median') {
            max(dat4plot$q75, na.rm = T)
          } else {
            max(dat4plot$VALUE, na.rm = T)
        }
      } else if (input$Ind_sel == 'Vessel characteristics' || input$Ind_sel == 'Processor characteristics') {
          if (input$demStats == 'Mean') {
            max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
          } else if (input$demStats == 'Median') {
            max(dat4plot$q75, na.rm = T)
          } else {
            max(dat4plot$VALUE, na.rm = T) 
          }
      }
      ##All options are total onnly##
      else if (input$Ind_sel == 'Impacts') {
        max(dat4plot$VALUE, na.rm = T)
      }
      }
    
    yaxislabel <- function () {
      if (input$LayoutSelect) {
        paste(dat4plot$STAT,
          "(see plot title for units)")
      } else {
        dat4plot$ylab
      }
    }
    
    yr <- function() {
      return(unique(as.numeric(dat4plot$YEAR)))
    }
    print(yr())
    groupVar <- "whitingv"
    
    
    # set colors for whiting/non-whiting/all lines ####
    allcolourThirds <- 
      c(
        # set colors for whiting/non-whiting/all vessels
        'Non-whiting vessels' = "#d7191c",
        'Whiting vessels' = "#2b83ba",
        'All vessels' = "#000000",
        # set colors for whiting/non-whiting/all processors
        'Non-whiting processors' = "#d7191c",
        'Whiting processors' = "#2b83ba",
        'All processors' = "#000000",
        # set black for mothership vessels
        'Mothership vessels' = "#000000",
        # set black for catcher-procesor vessels
        'Catcher-processor vessels' = "#000000")
    
    colourThirds <- allcolourThirds[unique(dat4plot$whitingv)]
    
    
    # Plot header construction ####
    # title
    plot.title <- function() {
      if (input$Sect_sel == "CV") {
        return("West Coast Catcher Vessels")
      } else if (input$Sect_sel == "M") {
        return("West Coast Mothership Vessels")
      } else if (input$Sect_sel == "CP") {
        return("West Coast Catcher-Processor Vessels")
      } else if (input$Sect_sel == "FR") {
        return("West Coast Shorebased Processors")
      }
    }
    
    
    gv <- function () {
      if (input$Sect_sel == 'CV') {
        if (!input$LayoutSelect) {
          if (input$CategorySelect != 'Fisheries') {
            sprintf(paste(
              "Vessels in",
              input$inSelect))
          } else {
            sprintf(paste(
              ""
            ))
          }} else if (input$CategorySelect == 'Homeport' | input$CategorySelect == 'State') {
            sprintf(paste(
              input$VariableSelect, 
              "vessels in",
              input$inSelect
            ))
          }
        else if (input$CategorySelect == 'Vessel length class') {
          sprintf(paste(
            "Vessels in",
            input$inSelect,
            "for",
            input$VariableSelect
          ))
        }
        else {
          sprintf(paste(
            "Vessels in",
            input$VariableSelect
          ))
        }
      }
      else {
        sprintf(paste(
          ""
        ))
      }
    }
    
    
    main <- function() {
      bquote(atop(.(plot.title()), .(gv())))
    }       
    ##gv <- function() {
    
    
    
    
    # confidentiality messages ####
    supp_obs <- function() {
      "\n
  \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. \nPlease see the About tab for more information."
    }
    

    # x-axis label ####
    # xlab is actually "notes"
    xlab <- function() {
      
      if(sum(dat4plot$conf, na.rm = T) == 0 & sum(dat4plot$flag, na.rm = T) == 0) {
        ""
      } else {
        paste(supp_obs())
      }
    }
        
    # Scaling factor for text size ####
    scale_text <- function() {
      if (input$Ind_sel != "Economic") {
        #     if (min(dat4plot$YEAR)<2009) {
        #      return(1.2)
        #     }  else {
        return(1.1)
        #  }
      } else {
        return(1.2)
        #   } else {
        #     return(1.2)
        #  }
      }
    }
    
    # scaling factor for geom text size ####
    scale_geom_text <- function() {
      if (any(dat4plot$VALUE > 0, na.rm = T)) {
        return(max(dat4plot$VALUE, na.rm = T))
      } else {
        return(0)
      }
    }
    
    # format data for graph and add to graph ####
    # special data for seasonality plot
    #  print(paste0(seasonality, 1))
    if(input$Ind_sel == 'Other' & !input$LayoutSelect) {
      # and seasonality is selected
      if(input$otherSelect =="Seasonality") {
        ssn <- mutate(dat4plot, 
          VALUE = as.Date(VALUE, origin = "2014-01-01", format = "%Y-%m-%d"),
          sort2 = reorder(VARIABLE, sort))
        g <- ggplot(ssn, aes(x = .data[[x]], y = .data[[y]], group = .data[[groupVar]]), environment =
            environment()) 
        g <- g + geom_line(aes(colour = .data[[groupVar]]), size = .75) +
          geom_point(aes(colour = .data[[groupVar]]), size = 4)
        # otherwise normal plot:
      } else {
        dat4plot <- dat4plot[order(dat4plot$sort), ]
        dat4plot$bystategrp <- paste0(dat4plot$AGID, dat4plot$whitingv)

        g <-
          # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
          ggplot(dat4plot, aes(x = .data[[x]], y = .data[[y]], group = .data[[groupVar]]), environment =
              environment())
      }
    } else {
      #dat4plot <- dat4plot[order(dat4plot$sort), ]
      g <-
        # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
        ggplot(dat4plot, aes(x = .data[[x]], y = .data[[y]], group = .data[[groupVar]]), environment =
            environment()) 
    }
    

    
    # add 'data variability' band ####
    if (!exists('ssn')) {
      g <-
        g + geom_ribbon(aes(
          ymax = upper,
          ymin = lower,
          fill = whitingv
        ), alpha = .25)#show.legend = FALSE,
    } else {
      g <- g
    }
    #----- define facet -----#####
    if (!input$LayoutSelect) {
      g <- g + facet_wrap(~ sort2, ncol = 2)
    } else {
      g <- g + facet_wrap(~ sort2, scales = 'free_y', ncol = 2)
    }
    
    
    #----- Define grey shading and Non-CS/CS labels ------####
    # choose label text size ####
    labeltext <- ifelse(input$tabs == 'Panel1', 7, 5)
    
    # geom_rect (define the grey boxes for pre-catch shares) ####
    geom_rect_fun <- function(ymin_val = -Inf, ymax_val = Inf) {
      geom_vline(
          xintercept = table(yr() <= 2010)[[2]] + .5,
          linewidth = 1.1,
          color = "darkgray"
        )
    }
    
    # geom_text function ####
    
    geom_text_fun <- function(x_val, y_val, label_val, vjust_val = .5) {
      
      geom_text(
        aes(
          x = x_val,
          y = y_val,
          vjust = vjust_val,
          label = label_val,
          family = "serif",
          fontface = "italic"
        ),
        hjust = 0,
        color = "grey20",
        size = labeltext / scale_text()
      )
      
    }

    # set rect and text for plots with both CS and non-CS years ####
    # otherwise no rect or text
    # the original code for the geom_text* are commented out at the bottom of the doc
    # if there are years shown before and after implementation of catch shares
    if (length(yr()) > 1 & min(yr()) < 2011 & max(yr()) > 2010) {
      # if the "Group by vessels" display is chosen
      if (!input$LayoutSelect) {
        # if seasonality is clicked
        if(input$Ind_sel == 'Other') {
          # and seasonality is selected
          if(input$otherSelect =="Seasonality") {
            g <- g + geom_rect_fun(
              ymin_val = structure(-Inf, class = "Date"),
              ymax_val = structure(Inf, class = "Date"))

            # for all other variables
          } else {
            g <- g + geom_rect_fun()

          } 
        } else {
          g <- g + geom_rect_fun()
    
        } 
      } else { # Compare by metrics
        g <- g + geom_rect_fun()
       
        
      } } else {
        # end of rect/text for cs/non-cs, no CS box required for plots with only one "kind" of year
        g <- g
      }

    # set colors for the three lines (whiting vessels, non-whiting vessels, all vessels) ####
    g <-
      g + scale_fill_manual(values = colourThirds) + scale_colour_manual(values = colourThirds)
    
    # add x axis line ####
    g <- g + geom_hline(yintercept = 0)
    
    #---- define labels (not sure what this does------####
    if (input$tabs == 'Panel1') {
      g <- g + labs(y = yaxislabel(),
        x = xlab(),
        title = main())
    } else {
      g <- g + labs(y = yaxislabel(),
        x = '',
        title = main())
    }
    
        # add lines and points to the plot ####
    if (input$Ind_sel == 'Other') {
      if (input$otherSelect == 'Share of landings by state') {
        g <-
          g + geom_line(aes(colour = .data[[groupVar]], group = 'bystategrp'), linewidth = 1.5) +
          geom_point(aes(colour = .data[[groupVar]], shape = 'AGID', group = 'bystategrp'),
            size = 4)
      } else {
        g <- g + geom_line(aes(colour = .data[[groupVar]]), linewidth = .75) +
          geom_point(aes(colour = .data[[groupVar]]), size = 4)
      }} else {
        g <- g + geom_line(aes(colour = .data[[groupVar]]), linewidth = .75) +
          geom_point(aes(colour = .data[[groupVar]]), size = 4)
      }
    
    if (input$LayoutSelect) {
      if (input$tabs == 'Panel1') {
        strptextsize <- 12
      } else {
        strptextsize <- 12
      } } else {
        strptextsize <- 18
      }

    # Define and add THEME to g####
    g <- g + theme(
      plot.title = element_text(
        vjust = 1,
        hjust = .5,
        size = rel(1.5),
        colour = "grey25",
        family = "sans",
        face = "bold"
      ),
      #
      # plot.title = element_text(, vjust = 1),
      panel.background = element_rect(fill = "white"),
      #      panel.spacing = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(
        family = "sans",
        size = strptextsize,
        color = "grey25",
        vjust = 1
      ),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(
        size = rel(1.1),
        hjust = 0,
        face = "italic",
        vjust = -1,
        colour = "grey25"
      ),
      axis.title.y = element_text(
        size = rel(1.2),
        vjust = 2,
        colour = "grey25"
      ),
      axis.line.x = element_line(
        linewidth = 2,
        colour = "black",
        linetype = "solid"
      ),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(
        family = "sans",
        color = "grey25",
        face = "bold",
        size = 12
      ),
      legend.title = element_blank()

    )

    print(g)
    
  } else
    plot(
      0,
      0,
      type = "n",
      axes = F,
      xlab = "",
      ylab = ""
    )

}