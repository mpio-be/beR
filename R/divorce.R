
#' divorced pairs
#' @export
#' @examples
#' data(divorcees)
#' d = data.table(divorcees)
#' o = pairing_status(d)





pairing_status <- function(d) {
  o = data.table(d)
  o[, .id := .I]

  i = 4
  
  x = rep( list(o[i, .(maleID, femaleID, season)]), nrow(o)-1) %>% rbindlist
  setnames(x, paste0( 'f_', names(x) ))

  x = cbind(x,  o[ setdiff(.id, i) , .(maleID, femaleID, season) ]  )
  x[, status := as.character(NA)]



  # assign status
  x[f_maleID == maleID & f_femaleID == femaleID & season < f_season, status  := 'faithful']

  
  p[f_maleID == maleID & f_femaleID != femaleID, status  := 'divorced_male']








}

