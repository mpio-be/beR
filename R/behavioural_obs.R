#' @title Make combo
#' @name make_combo
#' @param x  a data.table
#' @param UL character vector, Upper Left
#' @param LL character vector, Lower Left
#' @param UR character vector, Upper Right
#' @param LR character vector,Lower Right
#' @param only_use character vector  UL, LL, UR or LR. if not missing will use this one only
#' @export
#' @import glue
#' @examples 
#' require(beR)
#' x = fread(
#' "UL; LL; UR; LR
#' M ; DG ; R,DG,R
#' M ; DG ; Y,R,DB
#' M ; DG ; DB,DG,DG
#' M ; DG ; Y,DB,R
#' M ; DG ; DB,R,O
#' M ; DG ; Y,DG,DG
#' M ; DG ; R,Y,O    
#' M ; DG ; NA    
#' "
#', sep = ";" )
#' make_combo(x)
#' setnames(x, "LR", "LoR")
#' make_combo(x, LR = 'LoR')
#' make_combo(x, short = "UR")
#'
make_combo <- function(x, UL="UL", LL="LL", UR="UR", LR="LR", short) {

  if( missing(short)) {

    cols <- c(UL, LL, UR, LR)
    cc <- x[, ..cols]
    setnames(cc, c("UL", "LL", "UR", "LR"))

    o = cc[, .(COMBO = glue_data(.SD, "{UL}/{LL}|{UR}/{LR}", .na = "~"))]$COMBO

    }

  if(! missing(short) ) {
    stopifnot(short %in% c("UL", "LL", "UR", "LR"))

    o =  glue("{x[, ..short][[1]]}", .na = "~")

    }

  o
}
