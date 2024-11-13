library(vultureUtils)
library(sf)
library(targets)
library(tidyverse)
library(here)
load(here("data/data_cut_fall20.Rda"))
dat <- data_cut_fall20[[1]]
length(dat)
rp <- sf::st_read(here("data/roosts50_kde95_cutOffRegion.kml"))

# target individuals for minimal reprex: cale, cork, hatfield, dingle
day2 <- dat[[2]] %>% filter(Nili_id %in% c("cale", "cork", "hatfield", "dingle"))

# Example day -------------------------------------------------------------
getEdges(day2, roostPolygons = rp, roostBuffer = 50, consecThreshold = 2, distThreshold = 50, speedThreshUpper = 5, speedThreshLower = NULL, timeThreshold = "10 minutes", idCol = "Nili_id", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "sri", getLocs = F, speedCol = "ground_speed", timestampCol = "timestamp")

getEdges <- function(dataset, roostPolygons = NULL, roostBuffer, consecThreshold, distThreshold, speedThreshUpper, speedThreshLower, timeThreshold = "10 minutes", idCol = "Nili_id", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "edges", getLocs = FALSE, speedCol = "ground_speed",
                     timestampCol = "timestamp"){

  # Apply timegroups --------------------------------------------------------
  #XXX begin part================================
  # Convert the timestamp column to POSIXct.
  dataset <- dataset %>%
    dplyr::mutate({{timestampCol}} := as.POSIXct(.data[[timestampCol]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
  
  # Convert to a data table for spatsoc.
  data.table::setDT(dataset)
  
  # Group the points into timegroups using spatsoc::group_times.
  dataset <- spatsoc::group_times(dataset, datetime = timestampCol, threshold = timeThreshold)
  timegroupData <- dataset %>%
    dplyr::select(tidyselect::all_of(timestampCol), timegroup) %>% # save information about when each timegroup starts and ends.
    dplyr::group_by(timegroup) %>%
    dplyr::summarize(minTimestamp = min(.data[[timestampCol]], na.rm = T),
                     maxTimestamp = max(.data[[timestampCol]], na.rm = T))
  dataset <- sf::st_as_sf(dataset, coords = c("location_long", "location_lat"), crs ="WGS84", remove = F) # XXX this is fragile for now but whatever
  
  # # Retain timestamps for each point, with timegroup information appending. This will be joined back at the end, to fix #43 and make individual points traceable.
  # timestamps <- dataset[,c(timestampCol, idCol, "timegroup")] # XXX get rid of this for now bc we don't need it
  #XXX end part==================================
  
  # Message about getLocs and sri
  if(getLocs & return == "sri"){
    warning("Cannot return interaction locations when return = 'sri'. If you want interaction locations, use return = 'edges' or return = 'both'.")
  }
  # 
  # # Get all unique individuals before applying any filtering
  # if(includeAllVertices){
  #   uniqueIndivs <- unique(dataset[[idCol]])
  # }
  
  ## FILTER THE POINTS
  # If roost polygons were provided, use them to filter out data
  if(!is.null(roostPolygons)){
    # Buffer the roost polygons
    
    if(!is.null(roostBuffer)){
      roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)
    }
    # Exclude any points that fall within a (buffered) roost polygon
    removedRoosts <- dataset[lengths(sf::st_intersects(dataset, roostPolygons)) == 0,]
  }else{
    message("No roost polygons provided; points will not be filtered by spatial intersection.")
    removedRoosts <- dataset
  }
  
  # Restrict based on daylight
  if(daytimeOnly){
    times <- suncalc::getSunlightTimes(date = unique(lubridate::date(removedRoosts$timestamp)), lat = 31.434306, lon = 34.991889,
                                       keep = c("sunrise", "sunset")) %>%
      dplyr::select(date, sunrise, sunset) # XXX the coordinates I'm using here are from the centroid of Israel calculated here: https://rona.sh/centroid. This is just a placeholder until we decide on a more accurate way of doing this.
    removedRoosts <- removedRoosts %>%
      # remove leftover sunrise/sunset cols just in case
      {if("sunrise" %in% names(.)) dplyr::select(., -sunrise) else .}%>%
      {if("sunset" %in% names(.)) dplyr::select(., -sunset) else .}%>%
      dplyr::left_join(times, by = c("dateOnly" = "date")) %>%
      dplyr::mutate(daytime = dplyr::case_when(timestamp > .data[["sunrise"]] &
                                                 timestamp < .data[["sunset"]] ~ T,
                                               TRUE ~ F))
    
    # Filter out nighttimes
    nNightPoints <- nrow(removedRoosts[removedRoosts$daytime == F,])
    dayonly <- removedRoosts %>%
      dplyr::filter(daytime == T)
    nDayPoints <- nrow(dayonly)
    if(quiet == F){
      cat(paste0("Removed ", nNightPoints, " nighttime points, leaving ",
                 nDayPoints, " points.\n"))
    }
  }
  # This is the last thing to do so this part gets only used to calculate interactions
  # Restrict interactions based on ground speed
  filteredData <- filterLocs(df = dayonly,
                                           speedThreshUpper = speedThreshUpper,
                                           speedThreshLower = speedThreshLower, speedCol = speedCol)
  
  # If there are no rows left after filtering, create an empty data frame with the appropriate format.
  if(nrow(filteredData) == 0){
    # DUMMY EDGELIST, NO SRI TO COMPUTE
    out <- data.frame(timegroup = as.integer(),
                      ID1 = as.character(),
                      ID2 = as.character(),
                      distance = as.numeric(),
                      minTimestamp = as.POSIXct(character()),
                      maxTimestamp = as.POSIXct(character()))
    warning("After filtering, the dataset had 0 rows.")
  }
  
  # Make a new dataset to use as the SRI denominator: all data with only the daylight filter applied, not speed.
  denominator <- dayonly
  
  ## GET EDGELIST (optionally compute SRI)
  if(nrow(filteredData) != 0){
    ## Do we need to compute SRI?
    if(return == "edges"){ # if SRI is not needed, we can save time by not computing it.
      if(quiet){
        ### EDGES ONLY, QUIET
        out <- suppressMessages(suppressWarnings(spaceTimeGroups(dataset = filteredData,
                                                                               sriDenominatorDataset = denominator, # XXX added this
                                                                               distThreshold = distThreshold,
                                                                               consecThreshold = consecThreshold,
                                                                               timeThreshold = timeThreshold,
                                                                               sri = FALSE,
                                                                               idCol = idCol,
                                                                               timegroupData = timegroupData)))
      }else{
        ### EDGES ONLY, WARNINGS
        # compute edges without suppressing warnings
        out <- spaceTimeGroups(dataset = filteredData,
                                             sriDenominatorDataset = denominator, # XXX added this
                                             distThreshold = distThreshold,
                                             consecThreshold = consecThreshold,
                                             timeThreshold = timeThreshold,
                                             sri = FALSE,
                                             idCol = idCol,
                               timegroupData = timegroupData)
      }
      
    }else if(return %in% c("sri", "both")){ # otherwise we need to compute SRI.
      if(quiet){
        ### EDGES AND SRI, QUIET
        # suppress warnings while computing edges and SRI, returning a list of edges+sri
        out <- suppressMessages(suppressWarnings(spaceTimeGroups(dataset = filteredData,
                                                                               sriDenominatorDataset = denominator, # XXX added this                  
                                                                               distThreshold = distThreshold,
                                                                               consecThreshold = consecThreshold,
                                                                               timeThreshold = timeThreshold,
                                                                               sri = TRUE,
                                                                               idCol = idCol,
                                                                 timegroupData = timegroupData)))
        if(return == "sri"){
          out <- out["sri"]
        }
      }else{
        ### EDGES AND SRI, WARNINGS
        # compute edges and SRI without suppressing warnings, returning a list of edges+sri
        out <- spaceTimeGroups(dataset = filteredData,
                                             sriDenominatorDataset = denominator, # XXX added this
                                             distThreshold = distThreshold,
                                             consecThreshold = consecThreshold,
                                             timeThreshold = timeThreshold,
                                             sri = TRUE,
                                             idCol = idCol,
                               timegroupData = timegroupData)
        if(return == "sri"){
          out <- out["sri"]
        }
      }
    }
  }
  
  locsColNames <- c("latID1", "longID1", "latID2", "longID2", "interactionLat", "interactionLong")
  if(!getLocs & return %in% c("edges", "both")){
    if(!is.list(out)){
      out <- out %>%
        dplyr::select(-any_of(locsColNames))
    }else{
      if("edges" %in% names(out)){
        out$edges <- out$edges %>%
          dplyr::select(-any_of(locsColNames))
      }else{
        out <- out %>%
          dplyr::select(-any_of(locsColNames))
      }
    }
  }
  
  ## APPEND VERTICES
  if(includeAllVertices){
    toReturn <- append(out, list(as.character(uniqueIndivs)))
  }else{
    toReturn <- out
  }
  
  # If the list only has one object, unlist it one level down.
  if(length(toReturn) == 1){
    toReturn <- toReturn[[1]]
  }
  
  ## RETURN LIST
  return(toReturn)
}


spaceTimeGroups <- function(dataset, sriDenominatorDataset, distThreshold, consecThreshold = 2, crsToSet = "WGS84", crsToTransform = 32636, timestampCol = "timestamp", timeThreshold = "10 minutes", idCol = "Nili_id", latCol = "location_lat", longCol = "location_long", returnDist = TRUE, fillNA = FALSE, sri = T, timegroupData){
  # XXX added timegroupdata
  
  # Set up an sf object for use.
  if("sf" %in% class(dataset)){ # If dataset is an sf object...
    if(is.na(sf::st_crs(dataset))){ # only fill in crs if it is missing
      message(paste0("`dataset` is already an sf object but has no CRS. Setting CRS to ", crsToSet, "."))
      dataset <- sf::st_set_crs(dataset, crsToSet)
    }
  }else if(is.data.frame(dataset)){ # otherwise, if feedingSites is a data frame...
    # make sure it contains the lat and long cols
    checkmate::assertChoice(latCol, names(dataset))
    checkmate::assertChoice(longCol, names(dataset))
    
    if(nrow(dataset) == 0){
      stop("Dataset passed to vultureUtils::spaceTimeGroups has 0 rows. Cannot proceed with grouping.")
    }
    
    # convert to an sf object
    dataset <- dataset %>%
      sf::st_as_sf(coords = c(.data[[longCol]], .data[[latCol]]), remove = FALSE) %>%
      sf::st_set_crs(crsToSet) # assign the CRS
    
  }else{ # otherwise, throw an error.
    stop("`dataset` must be a data frame or an sf object.")
  }
  
  # Save lat and long coords, in case we need them later. Then, convert to UTM.
  dataset <- dataset %>%
    sf::st_transform(crsToTransform)
  dataset$utmE <- unlist(purrr::map(dataset$geometry, 1))
  dataset$utmN <- unlist(purrr::map(dataset$geometry, 2))
  dataset <- sf::st_drop_geometry(dataset) # spatsoc won't work if this is still an sf object. # fixed spelling error
  
  # Generate edge lists by timegroup
  edges <- spatsoc::edge_dist(DT = dataset, threshold = distThreshold, id = idCol,
                              coords = c("utmE", "utmN"), timegroup = "timegroup",
                              returnDist = returnDist, fillNA = T)
  
  # Remove self and duplicate edges
  edges <- edges %>%
    dplyr::filter(as.character(.data$ID1) < as.character(.data$ID2))
  
  # Now create a list where the edge only stays if it occurred in at least `consecThreshold` consecutive time steps.
  edgesFiltered <- consecEdges(edgeList = edges, consecThreshold = consecThreshold) %>%
    dplyr::ungroup()
  
  # Join to the timegroup data
  edgesFiltered <- edgesFiltered %>%
    dplyr::left_join(timegroupData, by = "timegroup")
  
  # Compute interaction locations
  ## get locations of each individual at each time group
  locs <- dataset %>%
    tibble::as_tibble() %>%
    dplyr::select(tidyselect::all_of(c(idCol, "timegroup", latCol, longCol))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(across(tidyselect::all_of(c(latCol, longCol)), as.numeric))
  
  # In case there is more than one point per individual per timegroup, get the mean.
  meanLocs <- locs %>%
    dplyr::group_by(across(all_of(c(idCol, "timegroup")))) %>%
    dplyr::summarize(mnLat = mean(.data[[latCol]], na.rm = T),
                     mnLong = mean(.data[[longCol]], na.rm = T))
  
  ef <- edgesFiltered %>%
    dplyr::left_join(meanLocs, by = c("ID1" = idCol, "timegroup")) %>%
    dplyr::rename("latID1" = mnLat, "longID1" = mnLong) %>%
    dplyr::left_join(meanLocs, by = c("ID2" = idCol, "timegroup")) %>%
    dplyr::rename("latID2" = mnLat, "longID2" = mnLong) %>%
    dplyr::mutate(interactionLat = (latID1 + latID2)/2,
                  interactionLong = (longID1 + longID2)/2)
  
  if(!(nrow(ef) == nrow(edgesFiltered))){
    stop("wrong number of rows") # XXX need a better way of preventing and handling this error.
  }
  edgesFiltered <- ef
  
  if(sri){
    if(nrow(edgesFiltered) > 1){ # XXX why is this >1 instead of >0? -KG 2024-11-12
      dfSRI <- calcSRI(dataset = sriDenominatorDataset,
                       edges = edgesFiltered, idCol = idCol)
    }else{
      dfSRI <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("ID1", "ID2", "sri"))
    }
    outList <- list("edges" = edgesFiltered, "sri" = dfSRI)
  }else{
    outList <- list("edges" = edgesFiltered)
  }
  # XXX need a step here where I join `timestamps` to `edgesFiltered`, in order to address #43. But for this to work, I have to decide what to do about the problem with some individuals showing up twice within the same 10-minute window.
  # Should I average their position during the window? Or should I pick just the first fix? Or should I compute the distance twice and if either of them is close enough to another individual, we consider it an edge? Very important to figure this out.
  return(outList)
}


calcSRI <- function(dataset, edges, idCol = "Nili_id", timegroupCol = "timegroup"){
  # setup for time warning
  cat("\nComputing SRI... this may take a while if your dataset is large.\n")
  start <- Sys.time()
  
  # arg checks
  checkmate::assertSubset(timegroupCol, names(dataset))
  checkmate::assertSubset(idCol, names(dataset))
  checkmate::assertDataFrame(dataset)
  checkmate::assertDataFrame(edges)
  
  edges <- dplyr::as_tibble(edges)
  
  ## get individuals per timegroup as a list
  # Info about timegroups and individuals, for SRI calculation
  timegroupsList <- dataset %>%
    dplyr::select(tidyselect::all_of(c(timegroupCol, idCol))) %>%
    dplyr::mutate({{idCol}} := as.character(.data[[idCol]])) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data[[timegroupCol]]) %>%
    dplyr::group_split() %>%
    purrr::map(~.x[[idCol]])
  
  ## get unique set of timegroups
  timegroups <- unique(dataset[[timegroupCol]])
  
  ## get all unique pairs of individuals
  inds <- as.character(unique(dataset[[idCol]]))
  allPairs <- expand.grid(ID1 = as.character(inds), ID2 = as.character(inds), stringsAsFactors = F) %>%
    dplyr::filter(ID1 < ID2)
  
  # wide data
  datasetWide <- dataset %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tidyselect::all_of(c(timegroupCol, idCol))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(val = TRUE) %>%
    tidyr::pivot_wider(id_cols = tidyselect::all_of(timegroupCol), names_from = tidyselect::all_of(idCol),
                       values_from = "val", values_fill = FALSE)
  
  ## get SRI information
  dfSRI <- purrr::pmap_dfr(allPairs, ~{
    a <- .x
    b <- .y
    colA <- datasetWide[,a]
    colB <- datasetWide[,b]
    nBoth <- sum(colA & colB)
    x <- nrow(unique(edges[edges$ID1 %in% c(a, b) & edges$ID2 %in% c(a, b), timegroupCol]))
    yab <- nBoth - x
    sri <- x/(x+yab)
    if(is.infinite(sri)){
      sri <- 0
    }
    dfRow <- data.frame("ID1" = a, "ID2" = b, "sri" = sri)
    return(dfRow)
  })
  
  # complete the time message
  end <- Sys.time()
  duration <- difftime(end, start, units = "secs")
  cat(paste0("SRI computation completed in ", duration, " seconds."))
  return(dfSRI)
}
