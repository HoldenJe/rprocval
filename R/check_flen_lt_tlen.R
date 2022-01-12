#' check_flen_lt_tlen - Checks FLEN less than TLEN
#' @description Query checks for instances where TLEN is less then FLEN
#' @return dataframe Table of records that fail the test is returned
#' @export
#'

check_flen_lt_tlen <- function(){
  sqldf::sqldf('SELECT PRJ_CD,
               SAM,
               EFF,
               SPC,
               GRP,
               FISH,
               TLEN,
               FLEN
        FROM FN125
        WHERE (TLEN<FLEN)
')
}
