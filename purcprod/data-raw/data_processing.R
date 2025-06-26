#NOTE: this script does not need to be run again unless there are changes to the raw data in the future


# this script processes the raw data stored in the data-raw file
# it is used for development purposes only. Data is processed and saved in the
# fisheyedataprep/dataprep_Purcprod repo as "purcprod_data.RData" it is then
# moved to this repo in the data-raw folder where it is loaded in this script.
# After loaded, it is saved using use_data() at the bottom of this script telling
# the app to use this data internally in the app. This does not need to be run
# again unless the "purcprod_data.RData" is changed or the color pallette/line types
# are changed below. It just needs to be run during development unless data changes are made


# loading in all data from "fisheyedataprep/dataprep_Purcprod" repo
# this contains all the data we need to run the app including:

# - overview (overview page data)
# - met_prac, met_reg, met_size (metric tab data)
# - prod_prac, prod_reg, prod_size (product type tab)
# - specs_prod, specs_reg, specs_size (species data)
# - gdp_defl (GDP deflator value data)
# - coverage (EDC coverage rate data)
# - clean_purcprod (All purchase production app data)

load("data-raw/purcprod_data.RData")



########################### Plot aesthetics #################################

# color pallete
pal <- c(
  light_text = "#0085CA",
  dark_text = "#003087",
  value1 = "#005E5E",
  value2 = "#C2D9E3",
  value3 = "#5EB6D9",
  value4 = "#90DFE3",
  bg_plot = "#E9F3F6"
)


# line colors
line_col <- c(
  # species colors
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
  "Other shellfish" = '#595478',
  "Other species" = '#C0B3B6',

  # state colors
  "California" = '#001B70',
  "Washington and Oregon" = '#C1052F',

  # processor size colors
  "Small/Medium" = '#005B70',
  "Large" = '#648C1C',

  # product type colors
  "All product types" = "#8E6C8A",
  "Fresh" = "#208AAE",
  "Frozen" = "#FF9F1C",
  "Other" = "#9E2B25",
  "Surimi" = "#1E2B25",
  "Unprocessed" = "#607744"
)

# line type
line_ty <- c(
  # states
  "California" = 'solid',
  "Washington and Oregon" = 'solid',

  # processor size
  "Small/Medium" = 'solid',
  "Large" = 'solid',

  # species
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

  # other species
  "Other species production" = 'dashed',
  "Crab" = 'dashed',
  "Shrimp" = 'dashed',
  "Salmon" = 'dashed',
  "Tuna" = 'dashed',
  "Coastal pelagics" = 'dashed',
  "Other shellfish" = 'dashed',
  "Other species" = 'dashed',

  # product type
  "All product types" = "solid",
  "Fresh" = "solid",
  "Frozen" = "solid",
  "Other" = "solid",
  "Unprocessed" = "solid",
  "Surimi" = "solid"
)


####################### writing to R/sysdata.rda file (internal data) ###########################

# this function writes the desired data frames that are used in the app into the 'data' folder
usethis::use_data(

  ########### GDP deflator vals
  gdp_defl,
  ########### for "Summary" tab on the Explore the Data page
  met_prac,
  met_reg,
  met_size,
  ###########  for "By Product Type" tab on the Explore the Data page
  prod_prac,
  prod_reg,
  prod_size,
  ###########  for "By Species" tab on the Explore the Data page
  specs_prod,
  specs_reg,
  specs_size,

  ###########  for "Overview" page
  overview,
  coverage,
  order,

  ###########  plot aesthetics
  pal,
  line_ty,
  line_col,
  ###########
  overwrite = TRUE,
  internal = TRUE # this parameter makes it so that the user of the app does not have access to the data itself. Just usign these data to make the plots. Not so the user can wrangle it themselves
)
