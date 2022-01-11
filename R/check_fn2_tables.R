#' check_fn2_tables
#' @description This function checks whether all the template tables exist in memory.
#' @return message Provides usethis messages verifying whether a table exists in memory
#' @export
#'

check_fn2_tables <- function(){
  FNtables <- c('FN011', 'FN012', 'FN013', 'FN014', 'FN022', 'FN026', 'FN028',
                'FN121', 'FN122', 'FN123', 'FN124', 'FN125', 'FN126','FN127')
  chatty_exists <- function(fntab){
    if(exists(fntab)){
      usethis::ui_done(paste(fntab, " exists."))
      return(TRUE)
    } else {
      usethis::ui_oops(paste(fntab, " was not found."))
      return(FALSE)
    }
  }
  passfail <- invisible(lapply(FNtables, chatty_exists))
  if(all(unlist(passfail)) == FALSE) {usethis::ui_oops("Not all template tables exist.")}
}
