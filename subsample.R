# Marta's subsampling code
library(svMisc)

subsample <- function(df, idCol = "Nili_id", timestampCol = "timestamp", mins = 10, quiet = F){
  allgv <- df[order(df[[idCol]], df[[timestampCol]]),]
  allgv$burst <- 1:nrow(allgv)
  
  temp.dif <- difftime(allgv[[timestampCol]][2:nrow(allgv)], allgv[[timestampCol]][(1:(nrow(allgv)-1))], units = "mins")
  allgv$dif <- c(1, temp.dif)
  allgv$dif <- dplyr::if_else(duplicated(as.character(allgv[[idCol]])) == FALSE, 1, allgv$dif)
  
  # Create a new data frame in which to store the subsampled data
  subsampled <- allgv[1,]
  n <- nrow(allgv)
  for(i in 1:(nrow(allgv)-1)){
    if(allgv$burst[i] <= tail(subsampled$burst, n = 1)){next}
    svMisc::progress(i, n) # this isn't perfect but it'll do for now.
    if(allgv$dif[i] > mins){
      subsampled <- rbind(subsampled, allgv[i,])
    }else{
      ix <- length(which(cumsum(allgv$dif[i:nrow(allgv)]) <= mins))
      subsampled <- rbind(subsampled, allgv[i+ix,])
      if(sum(allgv$dif[i:nrow(allgv)]) <= mins) {break}
    }
  }
  return(subsampled)
}

subsample_tidy <- function(df, idCol = "Nili_id", timestampCol = "timestamp", mins = 10, quiet = F){
  gvs <- df %>%
    arrange(idCol, timestampCol) %>%
    dplyr::group_by(.data[[idCol]]) %>%
    mutate(burst = 1:n(),
           temp.dif = as.numeric(difftime(.data[[timestampCol]], lag(.data[[timestampCol]])), units = "mins")) %>%
    group_split()
  
  
  temp.dif <- difftime(allgv[[timestampCol]][2:nrow(allgv)], allgv[[timestampCol]][(1:(nrow(allgv)-1))], units = "mins")
  allgv$dif <- c(1, temp.dif)
  allgv$dif <- dplyr::if_else(duplicated(as.character(allgv[[idCol]])) == FALSE, 1, allgv$dif)
  
  # Create a new data frame in which to store the subsampled data
  subsampled <- allgv[1,]
  n <- nrow(allgv)
  for(i in 1:(nrow(allgv)-1)){
    if(allgv$burst[i] <= tail(subsampled$burst, n = 1)){next}
    svMisc::progress(i, n) # this isn't perfect but it'll do for now.
    if(allgv$dif[i] > mins){
      subsampled <- rbind(subsampled, allgv[i,])
    }else{
      ix <- length(which(cumsum(allgv$dif[i:nrow(allgv)]) <= mins))
      subsampled <- rbind(subsampled, allgv[i+ix,])
      if(sum(allgv$dif[i:nrow(allgv)]) <= mins) {break}
    }
  }
  return(subsampled)
}