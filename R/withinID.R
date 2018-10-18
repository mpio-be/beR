# ==========================================================================
# functions applied on the same individual
# ==========================================================================

#' bouts counter
#' @export
bCounter = function(x){
  n  = length(x)
  y  = x[-1] != x[-n]
  i  = c( which(y | is.na(y)), n) 
  lengths = diff(c(0L, i))
  bout_length = rep(lengths, lengths)
  ids = 1:length(lengths)
  
  rep(ids, lengths)

 }

