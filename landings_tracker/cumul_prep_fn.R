# This function takes consecutive date-fishery level "suppressed flags" and puts them into sets
# For example, if there original data were:
# Jan - suppr
# Feb - ok
# Mar - suppr
# Apr - suppr
# May - ok
# Jun - ok

# then it creates a new grouping variable:
# Jan - suppr
# Feb - ok
# Mar-Apr - ??
# May - ok
# Jun - ok

# And then with the new grouping variable, it checks again to see if the multiple dates are combined, that we can stop suppressing the data

cumul_prep_fn <- function(split_dataframe, name) {
  
  # create the container for the cumulative confidentiality loop
cumulative_container <- vector(mode = 'list', length = length(split_dataframe))

# all this step does it label groups of dates to then apply new confidentiality rules to
number_of_sets <- length(split_dataframe)


pb <- progress_bar$new(total = number_of_sets,
  format = paste0("  ", name, " [:bar] :current/:total sets (:percent)"),
  clear = FALSE, width= 80)

for(s in 1:number_of_sets) {
  pb$tick()
  #print(paste0(s, ": ", names(conf_cumsum_split)[s]))
  int_outer <- split_dataframe[[s]]
  
  # if it's an empty item then skip
  if(nrow(int_outer) == 0) {
    print(paste0('no obs in ', names(split_dataframe)[s]))
    
    # if there's only one row, then skip 
  } else if(nrow(int_outer) == 1) {
    #print(paste0("\nthere's only one row:", s))
    int_outer$csflag <- 'f1'
    cumulative_container[[s]] = int_outer
    
    } else {

      # create csflag column
      int_outer$csflag <- NA
      
      # set NA index counter to 1 for each "block"
      counter = 1
      
      # set the first cumsum flag
      int_outer$csflag[1] <- ifelse(int_outer$final[1] == 'suppress', paste0('f', counter), paste0('clear', int_outer$LANDING_MONTH[1]))
      
      for(r in 2:nrow(int_outer)) {

        if(int_outer$final[r] == 'ok') {
          # if not suppressed then mark clear
          int_outer$csflag[r] <- paste0('clear', int_outer$LANDING_MONTH[r])
          
        } else if(int_outer$final[r] == 'suppress' & int_outer$final[r-1] != 'suppress') {
          # if suppressed and the previous one was not suppressed then give it a new counter
          counter <- counter + 1
          int_outer$csflag[r] <- paste0('f', counter)
          
        } else if(int_outer$final[r] == 'suppress' & int_outer$final[r-1] == 'suppress') {
          
          # if suppressed and previous one was also suppressed then give it the same counter
          int_outer$csflag[r] <- paste0('f', counter)
          
        } else {
          
          print(paste0('Something went wrong with s = ', s, ' and r = ', r))
          
        }
      }
      cumulative_container[[s]] = int_outer
    }
  }

cumulative_container_bind <- bind_rows(cumulative_container)

return(cumulative_container_bind)

}