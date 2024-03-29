# FUNCTION DEFINITIONS
# To be used in the rest of the workflow for this project.
library(dplyr)
library(lubridate)
library(data.table)
library(spatsoc)
library(purrr)
#devtools::install_github("kaijagahm/vultureUtils")
library(vultureUtils)

# This function definition comes from the Vulture-Conveyor-Belt project. I've removed all the other functions since this is the only one we need.

# 4. rotate_data_table ------------------------------------------------------------
# Note: this no longer actually takes a data_table, just keeping the name for consistency with previous.
# Unlike the previous function, this one requires that you separate the dates and times into separate columns beforehand.
rotate_data_table <- function(dataset, shiftMax, idCol = "indiv", dateCol = "date", timeCol = "time"){
  indivList <- dataset %>%
    group_by(.data[[idCol]]) %>%
    group_split(.keep = T)
  joined <- vector(mode = "list", length = length(indivList))
  for(indiv in 1:length(indivList)){
    x <- indivList[[indiv]]
    shift <- sample(-(shiftMax):shiftMax, size = 1)
    #cat(shift, "\n")
    # get all unique days that show up
    days <- sort(unique(x[[dateCol]]))
    
    # get min and max dates to shift around (the "poles" of the conveyor)
    selfMinDate <- min(days, na.rm = T)
    selfMaxDate <- max(days, na.rm = T)
    
    # create a total sequence of dates to select from
    daysFilled <- seq(lubridate::ymd(selfMinDate), lubridate::ymd(selfMaxDate), by = "day")
    # converting to numbers so we can use %%--which dates are the ones we started with?
    vec <- which(daysFilled %in% days)
    shiftedvec <- vec + shift # shift
    new <- (shiftedvec - min(vec)) %% (max(vec)-min(vec)+1)+1 # new dates as numbers
    shiftedDates <- daysFilled[new] # select those dates from the possibilities
    
    # Make a data frame to hold the old and new dates
    daysDF <- bind_cols({{dateCol}} := days, 
                        "newDate" = shiftedDates,
                        shift = shift)
    nw <- left_join(x, daysDF, by = dateCol)
    
    if(!is.null(timeCol)){
      nw$newdatetime <- lubridate::ymd_hms(paste(nw$newDate, nw[[timeCol]]))
    }
    joined[[indiv]] <- nw
  }
  out <- purrr::list_rbind(joined)
  return(out)
}
