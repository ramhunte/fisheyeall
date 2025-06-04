## this script processes the raw data stored in the data-raw file
# it is used for development purposes only. I take the raw data (raw_purcprod), clean it (clean_purcprod)
# and then subset it into smaller data frames for individual tabs and their associated plots
# I intentionally do not save raw_purcprod, clean_purcprod, nor proddf to the 'data' folder
# as they are not called in the app itself. just used in this script for cleaning

#NOTE: this script does not need to be run again unless there are changes to the raw data in the future

########################### Reading  Raw data #################################

# purchase production data
clean_purcprod <- readRDS("data-raw/mini_purcprod_targ.RDS")

# EDC coverage data
coverage <- readRDS("data-raw/coverage.rds")

####################### Cleaning "Summary" tab data ###########################

# this section subsets the data to be used for the "Summary" tab on the "Explore the Data" page

# subsetting data for the "Production Activities" bottom tab under "Summary"
sumdf_prac <- clean_purcprod |>
  dplyr::filter(tab == 'Summary', cs == "")

# subsetting data for the "Region" bottom tab under "Summary"
sumdf_reg <- clean_purcprod |>
  dplyr::filter(
    tab == 'Summary',
    # cs != "",
    variable %in% c("California", "Washington and Oregon")
  )

# subsetting data for the "Processor size/type" bottom tab under "Summary"
sumdf_size <- clean_purcprod |>
  dplyr::filter(
    tab == 'Summary',
    # cs != "",
    variable %in% c("Small", "Medium", "Large", "Non-processor")
  )


####################### Cleaning "By Product Type" tab data ###########################

# this section subsets the data to be used for the "By Product Type" tab on the "Explore the Data" page

# cleaning data for the "By Product Type" tab
# proddf <- dplyr::filter(clean_purcprod, tab == 'Product') |> # production tab data
  # tidyr::separate(
  #   metric,
  #   into = c("type", "metric"),
  #   sep = " (?=\\()",
  #   extra = "merge"
  # ) |>
  # dplyr::mutate(metric = substr(metric, 2, nchar(metric) - 1)) |>
  # dplyr::filter(!is.na(value))

# subsetting data for the "Production Activities" bottom tab under "By Product Type"
proddf_prac <- clean_purcprod |>
  dplyr::filter(tab == 'Product',
                cs == "",
                !is.na(value))

# subsetting data for the "Region" bottom tab under "By Product Type"
proddf_reg <- clean_purcprod |>
  dplyr::filter(
    tab == 'Product',
    cs != "",
    variable %in% c("California", "Washington and Oregon")
  )

# subsetting data for the "Processor size/type" bottom tab under "By Product Type"
proddf_size <- clean_purcprod |>
  dplyr::filter(
    tab == 'Product',
    variable %in% c("Small", "Medium", "Large", "Non-processor"),
    !is.na(value)
  )


####################### Cleaning "Species" tab data ###########################

# this data set is going to be similar to the By Product Type tab but flipped. However,
# some data labeled a "thousands" and some as "millions" will be put on the same plot
# here (unlike in the previous data frames), so I am restructuring raw data so it is
# all shown as numbers in the millions and not also thousands.

specsdf <- proddf_prac %>%

  # original data filter already classified labels as millions, billions,
  # dplyr::mutate(
  #   unit = dplyr::case_when(
  #     metric == "Production weight" ~ "millions",
  #     metric == "Production value" ~ "millions",
  #     metric == "Production price (per lb)" ~ ""
  #   ),
  #   value = dplyr::case_when(
  #     metric == "Production weight" ~ value / 1e6,
  #     metric == "Production value" ~ value / 1e6,
  #     metric == "Production price (per lb)" ~ value
  #   )
  # ) |>
  rbind(
    clean_purcprod |>
      dplyr::filter(tab == "Overview")
  )


# making sub df's for filters
specsdf_protype <- specsdf |>
  dplyr::filter(tab == "Product")

specsdf_reg <- specsdf |>
  dplyr::filter(
    tab == "Overview" & type %in% c("California", "Washington and Oregon")
  )

specsdf_size <- specsdf |>
  dplyr::filter(tab == "Overview" & type %in% c("Small/Medium", "Large"))


####################### Cleaning "Overview" page data ###########################

overviewdf <- clean_purcprod |>
  dplyr::filter(
    tab == "Overview"
  ) |>
  # adding in "All" option from summary data frame
  rbind(
    sumdf_prac |>
      # dplyr::select(-type) |>
      dplyr::filter(
        metric %in% c("Production value", "Production weight")
      ) |>
      dplyr::mutate(type = "All", tab = "Overview")
  ) |>
  dplyr::filter(
    metric %in% c("Production value", "Production weight")
  ) |>

  # View data in terms of 2023 deflator value, so shoe 2023 equivalence
  dplyr::mutate(
    value = dplyr::case_when(
      metric == "Production value" ~ value / defl,
      TRUE ~ value
    )
  ) |>
  dplyr::select(-defl)


order <- overviewdf %>%
  filter(type == "All",
         metric == "Production value") %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE
  )) %>%
  arrange(mean) %>%
  pull(variable)


# Apply the ordering to the overviewdf variable column as a factor
overviewdf <- overviewdf %>%
  mutate(variable = factor(variable, levels = order))



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
  "Small" = '#001B70',
  "Medium" = '#C1052F',
  "Small/Medium" = '#005B70',
  "Large" = '#648C1C',
  "Non-processor" = '#D89B2C',

  # product type colors
  "Canned" = "#287271",
  "Fillet" = "#9E2B25",
  "Fresh" = "#208AAE",
  "Frozen" = "#FF9F1C",
  "Headed-and-gutted" = "#8E6C8A",
  "Other" = "#B1B695",
  "Unprocessed" = "#607744",
  "Smoked" = "#D77A61"
)

# line type
line_ty <- c(
  # states
  "California" = 'solid',
  "Washington and Oregon" = 'solid',

  # processor size
  "Small" = 'solid',
  "Medium" = 'solid',
  "Small/Medium" = 'solid',
  "Large" = 'solid',
  "Non-processor" = 'solid',

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
  "Canned" = "solid",
  "Fillet" = "solid",
  "Fresh" = "solid",
  "Frozen" = "solid",
  "Headed-and-gutted" = "solid",
  "Other" = "solid",
  "Unprocessed" = "solid",
  "Smoked" = "solid"
)


####################### writing to R/sysdata.rda file (internal data) ###########################

# this function writes the desired data frames that are used in the app into the 'data' folder
usethis::use_data(
  ########### Deflator data
  gdp_defl,
  ########### for "Summary" tab on the Explore the Data page
  sumdf_prac,
  sumdf_reg,
  sumdf_size,
  ###########  for "By Product Type" tab on the Explore the Data page
  proddf_prac,
  proddf_reg,
  proddf_size,
  ###########  for "By Species" tab on the Explore the Data page
  specsdf,
  specsdf_protype,
  specsdf_reg,
  specsdf_size,

  ###########  for "Overview" page
  overviewdf,
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
