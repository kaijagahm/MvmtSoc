# Steps to perform a wrap-around randomization
# 1. Extract an explicit "time" column from your timestamps. Usually I use substr() or a similar function. Make sure you also have an explicit "date" column--i.e. you will need to have the full timestamp separated into separate date and time columns. 
# 2. Define number of repetitions `n` (e.g. 100) and a shiftMax (the maximum number of days in each direction that the dataset can be shifted; minimum 1)
# 3. Using a for loop or a map() statement, apply rotate_data_table() `n` times to your dataset. Save the repetitions.
# 4. For each of the repetitions, create a social network using the get_*_edges() functions in vultureUtils.

# rotate_data_table function
# Requires that you separate the dates and times into separate columns beforehand.
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

# Example code ------------------------------------------------------------

# rotate data
#message("rotating")
rotated <- rotate_data_table(dataset = dataset, shiftMax = shiftMax, 
                             idCol = "Nili_id", dateCol = "dateOnly",
                             timeCol = "time") %>% 
  mutate(timestamp = newdatetime) %>%
  sf::st_as_sf(., coords = c("location_long", "location_lat"), remove = F, crs = "WGS84")
verts <- unique(rotated$Nili_id)

tojoin_roosts <- rotated %>% st_drop_geometry() %>% 
  select(Nili_id, "roost_date" = dateOnly, newDate) %>% distinct()
roosts_temp <- rst %>%
  left_join(tojoin_roosts, by = c("Nili_id", "roost_date")) %>%
  mutate(roost_date = newDate)
vertsr <- unique(roosts_temp$Nili_id)

# calculate sri for rotated data
#message("calculating sri")
fl <- vultureUtils::getFlightEdges(rotated, roostPolygons = roostPolygons,
                                   distThreshold = 1000, idCol = "Nili_id",
                                   return = "sri") %>% 
  mutate(sri = replace_na(sri, 0)) %>%
  filter(sri != 0)
fe <- vultureUtils::getFeedingEdges(rotated, roostPolygons = roostPolygons,
                                    distThreshold = 50, idCol = "Nili_id",
                                    return = "sri") %>%
  mutate(sri = replace_na(sri, 0)) %>%
  filter(sri != 0)
ro <- vultureUtils::getRoostEdges(roosts_temp, mode = "distance",
                                  distThreshold = 100, latCol = "location_lat",
                                  longCol = "location_long", idCol = "Nili_id",
                                  dateCol = "roost_date", return = "sri") %>%
  mutate(sri = replace_na(sri, 0)) %>%
  filter(sri != 0)

# make graph from sri
#message("making graph")
g_fl <- igraph::graph_from_data_frame(d = fl %>% mutate(weight = sri), 
                                      directed = FALSE, vertices = verts)
g_fe <- igraph::graph_from_data_frame(d = fe %>% mutate(weight = sri), 
                                      directed = FALSE, vertices = verts)
g_ro <- igraph::graph_from_data_frame(d = ro %>% mutate(weight = sri), 
                                      directed = FALSE, vertices = vertsr)


# calculate graph metrics
#message("calculating metrics")
graph_metrics <- data.frame(degree = degree(g_fl),
                            strength = strength(g_fl),
                            Nili_id = names(degree(g_fl)),
                            situ = "flight") %>%
  bind_rows(data.frame(degree = degree(g_fe),
                       strength = strength(g_fe),
                       Nili_id = names(degree(g_fe)),
                       situ = "feeding")) %>%
  bind_rows(data.frame(degree = degree(g_ro),
                       strength = strength(g_ro),
                       Nili_id = names(degree(g_ro)),
                       situ = "roosting")) %>%
  mutate(rep = iter)
row.names(graph_metrics) <- NULL

# save graph metrics to spot in list
return(graph_metrics)
