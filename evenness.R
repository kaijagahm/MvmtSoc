# Evenness function--written by Kaija Gahm in May 2023

evenness <- function(graph){
  checkmate::assert_class(graph, "igraph")
  am <- igraph::as_adjacency_matrix(graph, attr = "weight", names = T, sparse = F)
  el <- am %>% # create an edge list that includes duplicates
    as.data.frame() %>%
    mutate(ID1 = row.names(.)) %>%
    pivot_longer(cols = -ID1, names_to = "ID2", values_to = "weight") %>%
    filter(weight > 0) # have to remove the zeroes in order for the degree calculation to work properly
  el <- el %>%
    group_by(ID1) %>%
    mutate(prop = weight/sum(weight, na.rm = T))
  el_s <- el %>%
    group_by(ID1) %>%
    summarize(n = n(),
              H = -1*sum(prop*log(prop), na.rm = T),
              evenness = H/log(n))
  e <- el_s$evenness
  names(e) <- el_s$ID1
  
  # Add back any vertices that are missing (because they're isolated)
  isolated <- names(V(graph))[!(names(V(graph)) %in% names(e))]
  toAdd <- rep(NA, length(isolated))
  names(toAdd) <- isolated
  e <- c(e, toAdd)
  if(!(all(names(e) %in% names(V(graph))))){
    stop("There are some names in the output vector that don't appear in the graph. Huh?")
  }
  
  if(!(all(names(V(graph)) %in% names(e)))){
    stop("The output vector is missing some individuals from the graph")
  }
  
  # Now we have to switch the order to match the order of vertices
  reordered <- e[names(V(graph))]
  if(!(all(names(reordered) == names(V(graph))))){
    stop("Error in name ordering")
  }else{
    return(reordered) # CRUCIALLY IMPORTANT, KAIJA!!! Ugh....
  }
}
