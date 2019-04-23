source("Scripts/00_functions_viz.R")


lu = function(x){
  length(unique(x))
}


replace_NAs_with_latest_value = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)            # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) ))  # diffing the indices + length yields how often 
}                               # they need to be repeated

# copied from: https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value

sigmoid = function(x, ix, s){ 1/(1+exp(-(x-ix)*s))}
