is_london_palette <- function(x){
  if (class(x) == "london_palette") {
    state <- TRUE
  } else { state <- FALSE}
  return(state)
}

is_london_palette_list <- function(x){
  if (class(x) == "london_palette_list") {
    state <- TRUE
  } else { state <- FALSE}
  return(state)
}
