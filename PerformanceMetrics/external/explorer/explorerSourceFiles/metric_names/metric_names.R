# This is a list of the hyperlinks that are used in the metric selector sidebar components in ex.io.sidebar1.R ####
# IMPACTS METRICS #####
impacts <- data.frame(
  metric_value = c(
    'Income impacts',
    'Employment impacts'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#impacts' target = '_blank'>Income impacts</a>",
    "<a href = 'pmtechmemo.html#impacts'target = '_blank'>Employment impacts</a>"
  )
)

# CHARACTERISTIC METRICS ####
dem_cv <- data.frame(
  metric_value = c(
    'Number of vessels',
    "Vessel length", 
    "Vessel replacement value",
    "Vessel market value",
    "Vessel horsepower",
    "Vessel fuel capacity",
    "Number of fisheries", 
    "Proportion of ex-vessel revenue from CS fishery", 
    "Revenue diversification"
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#descnumber' target = '_blank'>Number of vessels</a>",
    "<a href = 'pmtechmemo.html#descattributes' target = '_blank'>Vessel length</a>",
    "<a href = 'pmtechmemo.html#descvalue' target = '_blank'>Vessel replacement value</a>",
    "<a href = 'pmtechmemo.html#descvalue' target = '_blank'>Vessel market value</a>",
    "<a href = 'pmtechmemo.html#descattributes' target = '_blank'>Vessel horsepower</a>",
    "<a href = 'pmtechmemo.html#descattributes' target = '_blank'>Vessel fuel capacity</a>",
    "<a href = 'pmtechmemo.html#descfisheries' target = '_blank'>Number of fisheries</a>",
    "<a href = 'pmtechmemo.html#desccsrevenue' target = '_blank'>Proportion of ex-vessel revenue from catch share fishery</a>",
    "<a href = 'pmtechmemo.html#descdiversity' target = '_blank'>Revenue diversification</a>"
    )
)

dem_cpms <- data.frame(
  metric_value = c(
    'Number of vessels',
    "Vessel length",
    'Vessel replacement value',
    'Vessel market value',
    'Vessel horsepower',
    'Vessel fuel capacity',
    "Proportion of landings from CS fishery"
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#descnumber' target = '_blank'>Number of vessels</a>",
    "<a href = 'pmtechmemo.html#descattributes' target = '_blank'>Vessel length</a>",
    "<a href = 'pmtechmemo.html#descvalue' target = '_blank'>Vessel replacement value</a>",
    "<a href = 'pmtechmemo.html#descvalue' target = '_blank'>Vessel market value</a>",
    "<a href = 'pmtechmemo.html#descattributes' target = '_blank'>Vessel horsepower</a>",
    "<a href = 'pmtechmemo.html#descattributes' target = '_blank'>Vessel fuel capacity</a>",
    "<a href = 'pmtechmemo.html#desccsrevenue' target = '_blank'>Proportion of landings from catch share fishery</a>"
    )
)

dem_fr <- data.frame(
  metric_value = c(
    'Number of processors',
    "Number of species processed",
    "Number of species sold",
    "Revenue diversification",
    "Proportion of production value from West Coast groundfish",
    'Number of processors who fillet non-whiting groundfish'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#descnumber' target = '_blank'>Number of processors</a>",
    "<a href = 'pmtechmemo.html#descprocessed' target = '_blank'>Number of species processed</a>",
    "<a href = 'pmtechmemo.html#descsold' target = '_blank'>Number of species sold</a>",
    "<a href = 'pmtechmemo.html#descdiversity' target = '_blank'>Revenue diversification</a>",
    "<a href = 'pmtechmemo.html#descproduction' target = '_blank'>Proportion of production value from West Coast groundfish</a>",
    "<a href = 'pmtechmemo.html#descfillet' target = '_blank'>Number of processors who fillet non-whiting groundfish</a>"
    )
)

# ECONOMIC METRICS ####
econ <- data.frame(
  metric_value = c(
    'Revenue', 
    'Variable costs', 
    'Fixed costs', 
    'Variable cost net revenue', 
    'Total cost net revenue'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#econrevenue' target = '_blank'>Revenue</a>",
    "<a href = 'pmtechmemo.html#econcosts' target = '_blank'>Variable costs</a>",
    "<a href = 'pmtechmemo.html#econcosts' target = '_blank'>Fixed costs</a>",
    "<a href = 'pmtechmemo.html#econvcnr' target = '_blank'>Variable cost net revenue</a>",
    "<a href = 'pmtechmemo.html#econtcnr' target = '_blank'>Total cost net revenue</a>")
)

econ_fr <- data.frame(
  metric_value = c(
    'Revenue', 
    'Seafood sales revenue', 
    'Offload revenue', 
    'Custom processing and other revenue',
    'Variable costs', 
    'Fixed costs', 
    'Variable cost net revenue', 
    'Total cost net revenue'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#econrevenue' target = '_blank'>Revenue</a>",
    "<a href = 'pmtechmemo.html#econrevenue' target = '_blank'>Seafood sales revenue</a>",
    "<a href = 'pmtechmemo.html#econrevenue' target = '_blank'>Offload revenue</a>",
    "<a href = 'pmtechmemo.html#econrevenue' target = '_blank'>Custom processing and other revenue</a>",
    "<a href = 'pmtechmemo.html#econcosts' target = '_blank'>Variable costs</a>",
    "<a href = 'pmtechmemo.html#econcosts' target = '_blank'>Fixed costs</a>",
    "<a href = 'pmtechmemo.html#econvcnr' target = '_blank'>Variable cost net revenue</a>",
    "<a href = 'pmtechmemo.html#econtcnr' target = '_blank'>Total cost net revenue</a>"
    )
)

# LABOR METRICS ####
labor_cv_crew <- data.frame(
  metric_value = c(
    "Number of crew", 
    "Number of crew-days",
    'Crew payments',
    "Crew wage per year",
    "Crew wage per day",
    "Crew wage per dollar revenue",
    "Revenue per crew-day"
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#labornumber' target = '_blank'>Number of crew</a>",
    "<a href = 'pmtechmemo.html#labornumber' target = '_blank'>Number of crew-days</a>",
    "<a href = 'pmtechmemo.html#laborpayments' target = '_blank'>Crew payments</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Crew wage per year</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Crew wage per day</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Crew wage per dollar revenue</a>",
    "<a href = 'pmtechmemo.html#laborproductivity' target = '_blank'>Revenue per crew-day</a>"
    )
)
labor_cv_captain <- data.frame(
  metric_value = c(
    'Captain wage per year',
    'Captain wage per day',
    'Captain wage per dollar revenue'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Captain wage per year</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Captain wage per day</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Captain wage per dollar revenue</a>"
    )
)

labor_fr_prod <- data.frame(
  metric_value = c(
    'Average monthly number of production employees',
    'Maximum monthly number of production employees',
    'Production employee payments',
    'Hourly compensation per production employee',
    'Processing wage per value-added'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#labornumberfr' target = '_blank'>Average monthly number of production employees</a>",
    "<a href = 'pmtechmemo.html#labornumberfr' target = '_blank'>Maximum monthly number of production employees</a>",
    "<a href = 'pmtechmemo.html#laborpayments' target = '_blank'>Production employee payments</a>",
    "<a href = 'pmtechmemo.html#laborratefr' target = '_blank'>Hourly compensation per production employee</a>",
    "<a href = 'pmtechmemo.html#laborvalueadded' target = '_blank'>Processing wage per value-added</a>"
    )
)
labor_fr_nonprod <- data.frame(
  metric_value = c(
    'Number of non-production employees',
    'Non-production employee payments',
    'Annual compensation per non-production employee'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#labornumberfr' target = '_blank'>Number of non-production employees</a>",
    "<a href = 'pmtechmemo.html#laborpayments' target = '_blank'>Non-production employee payments</a>",
    "<a href = 'pmtechmemo.html#laborratefr' target = '_blank'>Annual compensation per non-production employees</a>"
    )
)

labor_cpms_proc <- data.frame(
  metric_value = c(
    'Number of processing crew',
    'Number of processing crew-days',
    'Processing crew payments',
    'Processing crew wage per year',
    'Processing crew wage per day',
    'Processing crew wage per dollar revenue'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#labornumber' target = '_blank'>Number of processing crew</a>",
    "<a href = 'pmtechmemo.html#labornumber' target = '_blank'>Number of processing crew-days</a>",
    "<a href = 'pmtechmemo.html#laborpayments' target = '_blank'>Processing crew payments</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Processing crew wage per year</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Processing crew wage per day</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Processing crew wage per dollar revenue</a>"
    )
)
labor_cpms_nonproc <- data.frame(
  metric_value = c(
    'Number of non-processing crew',
    'Number of non-processing crew-days',
    'Non-processing crew payments',
    'Non-processing crew wage per year',
    'Non-processing crew wage per day',
    'Non-processing crew wage per dollar revenue'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#labornumber' target = '_blank'>Number of non-processing crew</a>",
    "<a href = 'pmtechmemo.html#labornumber' target = '_blank'>Number of non-processing crew-days</a>",
    "<a href = 'pmtechmemo.html#laborpayments' target = '_blank'>Non-processing crew payments</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Non-processing crew wage per year</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Non-processing crew wage per day</a>",
    "<a href = 'pmtechmemo.html#laborrate' target = '_blank'>Non-processing crew wage per dollar revenue</a>"
    )
)

# OTHER METRICS ####
other_cv <- data.frame(
  metric_value = c(
    "Days at sea",
    'Trips',
    "Landed weight",
    "Fuel use per day", 
    "Annual fuel use",
    "Speed while fishing",
    "Gini coefficient", 
    "Share of landings by state",
    "Seasonality"
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days at sea</a>",
    "<a href = 'pmtechmemo.html#othertrips' target = '_blank'>Trips</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Landed weight</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Fuel use per day</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Annual fuel use</a>",
    "<a href = 'pmtechmemo.html#otherspeed' target = '_blank'>Speed while fishing</a>",
    "<a href = 'pmtechmemo.html#othergini' target = '_blank'>Gini coefficient</a>",
    "<a href = 'pmtechmemo.html#othersharelandings' target = '_blank'>Share of landings by state</a>",
    "<a href = 'pmtechmemo.html#otherseasonality' target = '_blank'>Seasonality</a>")
)
other_cv_layout <- data.frame(
  metric_value = c(
    "Days at sea", 
    'Trips',
    "Landed weight",
    "Fuel use per day", 
    "Annual fuel use",
    "Speed while fishing"
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days at sea</a>",
    "<a href = 'pmtechmemo.html#othertrips' target = '_blank'>Trips</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Landed weight</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Fuel use per day</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Annual fuel use</a>",
    "<a href = 'pmtechmemo.html#otherspeed' target = '_blank'>Speed while fishing</a>"
    )
)

other_fr <- data.frame(
  metric_value = c(
    "Gini coefficient",
    'Percentage of purchases from non-vessel sources',
    'Percentage of production processed'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#othergini' target = '_blank'>Gini coefficient</a>",
    "<a href = 'pmtechmemo.html#othernonvessel' target = '_blank'>Percentage of purchases from non-vessel sources</a>",
    "<a href = 'pmtechmemo.html#otherproductionprocessed' target = '_blank'>Percentage of production processed</a>"
    )
  )

other_ms <- data.frame(
  metric_value = c(
    'Days fishing, processing, and steaming on the WC',
    'Days offloading on the WC',
    'Purchase weight (West Coast)',
    'Fuel use per day',
    'Annual fuel use',
    "Gini coefficient",
    "Seasonality",
    'Days fishing, processing, and steaming in AK',
    'Days steaming between the WC and AK', 
    'Purchase weight (Alaska)'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days fishing, processing, and steaming on the WC</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days offloading on the WC</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Purchase weight (West Coast)</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Fuel use per day</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Annual fuel use</a>",
    "<a href = 'pmtechmemo.html#othergini' target = '_blank'>Gini coefficient</a>",
    "<a href = 'pmtechmemo.html#otherseasonality' target = '_blank'>Seasonality</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days fishing, processing, and steaming in AK</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days steaming between the WC and AK</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Purchase weight (Alaska)</a>"
    )
)
other_ms_layout <- data.frame(
  metric_value = c(
    'Days fishing, processing, and steaming on the WC',
    'Days offloading on the WC',
    'Purchase weight (West Coast)',
    'Fuel use per day',
    'Annual fuel use',
    'Gini coefficient',
    'Days fishing, processing, and steaming in AK',
    'Days steaming between the WC and AK', 
    'Purchase weight (Alaska)'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days fishing, processing, and steaming on the WC</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days offloading on the WC</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Purchase weight (West Coast)</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Fuel use per day</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Annual fuel use</a>",
    "<a href = 'pmtechmemo.html#othergini' target = '_blank'>Gini coefficient</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days fishing, processing, and steaming in AK</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days steaming between the WC and AK</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Purchase weight (Alaska)</a>"
    )
)

other_cp <- data.frame(
  metric_value = c(
    'Days fishing, processing, and steaming on the WC',
    'Days offloading on the WC',
    'Catch weight (West Coast)',
    'Fuel use per day',
    'Annual fuel use',
    "Gini coefficient",
    "Seasonality",
    'Days fishing, processing, and steaming in AK',
    'Days steaming between the WC and AK',
    'Catch weight (Alaska)'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days fishing, processing, and steaming on the WC</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days offloading on the WC</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Catch weight (West Coast)</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Fuel use per day</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Annual fuel use</a>",
    "<a href = 'pmtechmemo.html#othergini' target = '_blank'>Gini coefficient</a>",
    "<a href = 'pmtechmemo.html#otherseasonality' target = '_blank'>Seasonality</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days fishing, processing, and steaming in AK</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days steaming between the WC and AK</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Catch weight (Alaska)</a>"
    )
)
other_cp_layout <- data.frame(
  metric_value = c(
    'Days fishing, processing, and steaming on the WC',
    'Days offloading on the WC',
    'Catch weight (West Coast)',
    'Fuel use per day',
    'Annual fuel use',
    'Gini coefficient',
    'Days fishing, processing, and steaming in AK',
    'Days steaming between the WC and AK',
    'Catch weight (Alaska)'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days fishing, processing, and steaming on the WC</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days offloading on the WC</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Catch weight (West Coast)</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Fuel use per day</a>",
    "<a href = 'pmtechmemo.html#otherfuel' target = '_blank'>Annual fuel use</a>",
    "<a href = 'pmtechmemo.html#othergini' target = '_blank'>Gini coefficient</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days fishing, processing, and steaming in AK</a>",
    "<a href = 'pmtechmemo.html#otherdays' target = '_blank'>Days steaming between the WC and AK</a>",
    "<a href = 'pmtechmemo.html#otherweight' target = '_blank'>Catch weight (Alaska)</a>"
    )
)

# COSTS METRICS ####
costs_cv <- data.frame(
  metric_value = c(
    'All fixed costs',
    'Fishing gear',
    'On-board equipment',
    'Other fixed costs',
    'All variable costs',
    'Buyback fees',
    'Labor',
    'Cost recovery fees',
    'Fuel',
    'Observers/EM', 
    'Other variable costs'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#fcosts' target = '_blank'>All fixed costs</a>",
    "<a href = 'pmtechmemo.html#costsfishinggear' target = '_blank'>Fishing gear</a>",
    "<a href = 'pmtechmemo.html#costsonboardequipment' target = '_blank'>On-board equipment</a>",
    "<a href = 'pmtechmemo.html#costsotherfixed' target = '_blank'>Other fixed costs</a>",
    "<a href = 'pmtechmemo.html#vcosts' target = '_blank'>All variable costs</a>",
    "<a href = 'pmtechmemo.html#costsbuyback' target = '_blank'>Buyback fees</a>",
    "<a href = 'pmtechmemo.html#costslabor' target = '_blank'>Labor</a>",
    "<a href = 'pmtechmemo.html#costscr' target = '_blank'>Cost recovery fees</a>",
    "<a href = 'pmtechmemo.html#costsfuel' target = '_blank'>Fuel</a>",
    "<a href = 'pmtechmemo.html#costsobservers' target = '_blank'>Observers/EM</a>",
    "<a href = 'pmtechmemo.html#costsothervariable' target = '_blank'>Other variable costs</a>"
    )
  )

costs_fr <- data.frame(
  metric_value = c(
    'All fixed costs',
    'Buildings',
    'Equipment',
    'Other fixed costs',
    'All variable costs',
    'Fish purchases',
    'Additives',
    'Production supplies',
    'Freight & trucking',
    'Labor',
    'Shoreside monitoring',
    'Taxes',
    'Offloading',
    'Off-site freezing & storage',
    'Packing materials',
    'Electricity',
    'Gas',
    'Waste & Byproduct Disposal',
    'Water',
    'Other variable costs'
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#fcosts' target = '_blank'>All fixed costs</a>",
    "<a href = 'pmtechmemo.html#costsfixedfr' target = '_blank'>Buildings</a>",
    "<a href = 'pmtechmemo.html#costsfixedfr' target = '_blank'>Equipment</a>",
    "<a href = 'pmtechmemo.html#costsotherfixed' target = '_blank'>Other fixed costs</a>",
    "<a href = 'pmtechmemo.html#vcosts' target = '_blank'>All variable costs</a>",
    "<a href = 'pmtechmemo.html#costsfishpurchases' target = '_blank'>Fish purchases</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Additives</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Production supplies</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Freight & trucking</a>",
    "<a href = 'pmtechmemo.html#costslabor' target = '_blank'>Labor</a>",
    "<a href = 'pmtechmemo.html#costsmonitoring' target = '_blank'>Shoreside monitoring</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Taxes</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Offloading</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Off-site freezing & storage</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Packing materials</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Electricity</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Gas</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Waste & Byproduct Disposal</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Water</a>",
    "<a href = 'pmtechmemo.html#costsotherfr' target = '_blank'>Other variable costs</a>"
    )
)

costs_ms <- data.frame(
  metric_value = c(
    "All fixed costs",
    "Fishing gear",
    "On-board equipment",
    "Processing equipment",
    'Other fixed costs',
    "All variable costs",
    "Fish purchases",
    "Fuel",
    "Labor",
    "Observers",
    "Other variable costs"
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#fcosts' target = '_blank'>All fixed costs</a>",
    "<a href = 'pmtechmemo.html#costsfishinggear' target = '_blank'>Fishing gear</a>",
    "<a href = 'pmtechmemo.html#costsonboardequipment' target = '_blank'>On-board equipment</a>",
    "<a href = 'pmtechmemo.html#costsprocessingequipment' target = '_blank'>Processing equipment</a>",
    "<a href = 'pmtechmemo.html#costsotherfixed' target = '_blank'>Other fixed costs</a>",
    "<a href = 'pmtechmemo.html#vcosts' target = '_blank'>All variable costs</a>",
    "<a href = 'pmtechmemo.html#costsfishpurchases' target = '_blank'>Fish purchases</a>",
    "<a href = 'pmtechmemo.html#costsfuel' target = '_blank'>Fuel</a>",
    "<a href = 'pmtechmemo.html#costslabor' target = '_blank'>Labor</a>",
    "<a href = 'pmtechmemo.html#costsobservers' target = '_blank'>Observers</a>",
    "<a href = 'pmtechmemo.html#costsothervariable' target = '_blank'>Other variable costs</a>"
    )
)

costs_cp <- data.frame(
  metric_value = c(
    "All fixed costs",
    "Fishing gear",
    "On-board equipment",
    "Processing equipment",
    'Other fixed costs',
    "All variable costs",
    'Cost recovery fees', 
    "Fuel",
    "Labor",
    "Observers",
    "Other variable costs"
  ),
  metric_link = c(
    "<a href = 'pmtechmemo.html#fcosts' target = '_blank'>All fixed costs</a>",
    "<a href = 'pmtechmemo.html#costsfishinggear' target = '_blank'>Fishing gear</a>",
    "<a href = 'pmtechmemo.html#costsonboardequipment' target = '_blank'>On-board equipment</a>",
    "<a href = 'pmtechmemo.html#costsprocessingequipment' target = '_blank'>Processing equipment</a>",
    "<a href = 'pmtechmemo.html#costsotherfixed' target = '_blank'>Other fixed costs</a>",
    "<a href = 'pmtechmemo.html#vcosts' target = '_blank'>All variable costs</a>",
    "<a href = 'pmtechmemo.html#costscr' target = '_blank'>Cost recovery fees</a>",
    "<a href = 'pmtechmemo.html#costsfuel' target = '_blank'>Fuel</a>",
    "<a href = 'pmtechmemo.html#costslabor' target = '_blank'>Labor</a>",
    "<a href = 'pmtechmemo.html#costsobservers' target = '_blank'>Observers</a>",
    "<a href = 'pmtechmemo.html#costsothervariable' target = '_blank'>Other variable costs</a>"
    )
)
