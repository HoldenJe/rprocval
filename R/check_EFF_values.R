#' check_EFF_values - Checks for unusual/non-standard mesh sizes
#' @description Most of the gillnets use common sizes. This function checks for sizes not commonly used.
#' @return
#' @export
#'

check_EFF_values <- function(fn = FN125){
   fn$EFF_check<-ifelse(fn$EFF %in% c('1','2', '3', '032', '038', '051', '064',
                             '076', '089', '102', '114', '127', '140', '153'), T, F)
   fn
}
