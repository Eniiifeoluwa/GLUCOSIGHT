splitter<- function(x, y, set.seed = None, ratio){
  split = as.integer(ratio * length(x))
  indices = permutations(length(x))
  splitter_indices = indices[:split]
  tester = indices[, split::]

  
  
}
library(gtools)