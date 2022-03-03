tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         tags$div(
           tags$h3("Fisheries Economics Explorer (Fisheye)"),
           tags$h4("About"),
           tags$p("Welcome to Fisheries Economics Explorer (Fisheye)—an interactive tool for exploring economic data 
              from West Coast commercial fisheries. Data used in FISHEyE comes from the", 
              tags$a("https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm",
              "Economic Data Collection (EDC) Program"), 
             "and the",
              tags$a(href = "http://pacfin.psmfc.org/", "Pacific Fisheries Information Network (PacFIN).")),
           tags$p("As part of the catch share program, participants of the fishery are required to complete",
             tags$a(href = "https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm",
             "EDC survey forms"), 
             "as stated in",
             tags$a(href = "http://www.ecfr.gov/cgi-bin/text-idx?SID=06f0c396e52e564ce22a048aa910f49f&node=50:13.0.1.1.1.4.1.5&rgn=div8",
             "regulation 50 CFS 660.114."),
             "Data collection began in 2009, two years prior to implementing the catch share program. 
             All vessels that participate in the catch share program must report data for all fisheries 
             they participate in, including non-catch share fisheries."),
           tags$h4("West Coast Groundfish Trawl Catch Share Fishery"),
           tags$p("The fishery consists of cooperatives for the at-sea mothership and catcher-processor fleets, and an
           individual fishing quota (IFQ) program for the shorebased trawl fleet (catcher vessels, and first receivers 
           and shorebased processors). The catch share program was implemented in 2011 with several goals, including:",
             tags$ul("Provide for a viable, profitable, and efficient groundfish fishery;"),
             tags$ul("Increase operational flexibility;"),
             tags$ul("Minimize adverse effects on fishing communities and other fisheries to the extent practical;"),
             tags$ul("Promote measurable economic and employment benefits through the seafood catching, processing, 
             distribution, and support sectors of the industry."),
             "Please refer to the links in the",
             tags$a("./Resources.html", "Resources"),
             "section for more detailed information about the fishery."),
           tags$h4(em("A note about confidentiality")),
           tags$p("Data confidentiality requirements do not allow us to show individual observations. Therefore, we aggregate or 
           summarize the data to protect individual confidentiality. Data queries that would display confidential data are 
           not plotted or made available to download. In these cases, the number of observations will be shown in the data 
           table, but the value will be shown as NA."),
           tags$p("Where possible, data are reported for all vessels combined, whiting vessels, and non-whiting vessels. If 
           there are fewer than three vessels in either the whiting or non-whiting category we will only report the 
           All vessels category. More information on data confidentiality requirements can be found in the",
           tags$a(href = "http://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operations_Report_2014.pdf",
           "EDC Administration and Operations Report.")),
           tags$h4(em("Disclaimer")),
           tags$p("The data used in this application are periodically updated and subject to change. Data updates are recorded in the Fisheye blog."),
           tags$h4(em("Acknowledgements")),
           tags$p("There are numerous individuals to thank for their contributions in developing Fisheye. We thank the Northwest 
             Fisheries Science Center (NWFSC) economists and application developers, Scientific Data Management staff, and 
             Information Technology staff. We thank PacFIN for providing landings information. NMFS Office of Science and 
             Technology and NMFS Office of Sustainable Fisheries provided support. Numerous individuals reviewed the 
             application and we thank them for their helpful comments and suggestions. Finally and very importantly, we 
             thank the members of the West Coast fishing industry who have supplied information to the EDC Program.")
)
)