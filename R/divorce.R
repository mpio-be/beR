

#' Divorced pairs
#'
#' @param d data.frame containing maleID, femaleID and season
#'
#' @return a data.table similar to d but with two additional columns: male_status and female_status
#' @export
#'
#' @examples
#' data(divorcees)
#' d = divorcees
#' x = pairing_status(divorcees)
#' x
 
pairing_status <- function(d) {
  o = data.table(d)


  # FAITHFUL pairs
    rpairs = merge(o, o, by = c('maleID', 'femaleID'), suffixes = c('', '_f'), allow.cartesian = TRUE)
    rpairs = rpairs[season > season_f  , .(maleID, femaleID, season) ] %>% unique
    rpairs[, ':='  (male_status = 're-united',  female_status = 're-united') ]


  #MALES divorcees and widows
    m = merge(o, o, by = 'maleID', suffixes = c('', '_f'), allow.cartesian = TRUE)
    x = m[season > season_f & femaleID != femaleID_f]

    # is femaleID_f (former) still alive in season?
    z = merge( x[, .(maleID, femaleID_f, season)], o[, .(femaleID, season)], by.x = c('femaleID_f', 'season'), by.y = c('femaleID', 'season') )
    z[, male_status := 'divorced']
    dw = merge(x, z[, .(maleID, season, male_status)] , by = c('maleID', 'season') , all.x = TRUE,  allow.cartesian = TRUE)
    dw[is.na(male_status), male_status := 'widowed']
    dwmales = dw[, .(maleID, season,male_status)]%>% unique



  #FEMALES divorcees and widows
    m = merge(o, o, by = 'femaleID', suffixes = c('', '_f'), allow.cartesian = TRUE)
    x = m[season > season_f & maleID != maleID_f]

    # is maleID_f (former) still alive in season?
    z = merge( x[, .(femaleID, maleID_f, season)], o[, .(maleID, season)], by.x = c('maleID_f', 'season'), by.y = c('maleID', 'season') )
    z[, female_status := 'divorced']
    dw = merge(x, z[, .(femaleID, season, female_status)] , by = c('femaleID', 'season') , all.x = TRUE,  allow.cartesian = TRUE )
    dw[is.na(female_status), female_status := 'widowed']
    dwfemales = dw[, .(femaleID, season,female_status)]%>% unique


  # all
    O = merge(o, rpairs,  by = c('maleID', 'femaleID', 'season'), all.x = TRUE )

    O = merge(O, dwmales,  by = c('maleID', 'season'), all.x = TRUE , suffixes = c('', '_temp'))
    O[!is.na(male_status_temp) & is.na(male_status) , male_status := male_status_temp][, male_status_temp := NULL]

    O = merge(O, dwfemales,  by = c('femaleID', 'season'), all.x = TRUE , suffixes = c('', '_temp'))
    O[!is.na(female_status_temp) & is.na(female_status), female_status := female_status_temp][, female_status_temp := NULL]


    O = O[, .(maleID, femaleID, season, male_status, female_status)]
    setorder(O, season, maleID, femaleID)
    O

  }
