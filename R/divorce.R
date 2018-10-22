
#' divorced pairs
#' @export
#' @examples
#' data(divorcees)
#' d = divorcees
#' o = pairing_status(d)




pairing_status <- function(d) {
  o = data.table(d)

  #IN WORK!

  m = merge(o, o, by = 'maleID', suffixes = c('', '_f'))

  # faithful
  m[season > season_f & femaleID == femaleID_f, male_status := 're-united']
  
  #divorces and widows
  x = m[season > season_f & femaleID != femaleID_f]
  # is femaleID_f (former) still alive in season?
  x = merge(x, o[, .(femaleID, season)], by.x = 'femaleID_f', by.y = 'femaleID', 
      suffixes = c('', '_y'))
  
  x[season > season_y, male_status := 'divorced_male']
  x[season != season_y, male_status := 'widowed_male']


  rbind(m[!is.na(male_status), .(maleID,season, male_status)], 
        x[!is.na(male_status), .(maleID,season, male_status)])







}

