# landings_tracker_2020

# Getting the app started requires running two data files first:

    - datapull.R - this is query provided by Camille Kohler at PacFIN
    - dataprep.R - pepares the data for loading into the app

# Notes about error messages:
# As of 5/14/2020 there are 6 types of error messages. 
There are custom messages for each of the four filters.
In addition there is a message when cumulative = Y and metric is not exvessel revenue or weight,
and a message for when statistic = mean/median and metric is number of vessels or buyers.
These messages override the filter messages.
