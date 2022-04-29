#' Check FN121 Columns
#'
#' @param FN121 FN121 table that adheres to the FN2 data model
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' fn121_column_check(FN121)
#' }
#'

fn121_column_check <- function(FN121) {
  req_names <-
    c("PRJ_CD",
      "SAM",
      "EFFDT0",
      "EFFDT1",
      "EFFTM0",
      "EFFTM1",
      "SIDEP",
      "SITEM",
      "EFFDUR",
      "EFFST")
  if (all(req_names %in% names(FN121))) {
    usethis::ui_done("FN121 has required fields")
  }

  if (!("PRJ_CD" %in% names(FN121))) {
    usethis::ui_oops("PRJ_CD is required in FN121")
  }
  if (!("SAM" %in% names(FN121))) {
    usethis::ui_oops("SAM is required in FN121")
  }
  if (!("EFFDT0" %in% names(FN121))) {
    usethis::ui_oops("EFFDT0 is required in FN121")
  }
  if (!("EFFDT1" %in% names(FN121))) {
    usethis::ui_oops("EFFDT1 is required in FN121")
  }
  if (!("EFFTM0" %in% names(FN121))) {
    usethis::ui_oops("EFFTM0 is required in FN121")
  }
  if (!("EFFTM1" %in% names(FN121))) {
    usethis::ui_oops("EFFTM1 is required in FN121")
  }
  if (!("SIDEP" %in% names(FN121))) {
    usethis::ui_oops("SIDEP is required in FN121")
  }
  if (!("SITEM" %in% names(FN121))) {
    usethis::ui_oops("SITEM is required in FN121")
  }
  if (!("EFFDUR" %in% names(FN121))) {
    usethis::ui_oops("EFFDUR is required in FN121")
  }
  if (!("EFFST" %in% names(FN121))) {
    usethis::ui_oops("EFFST is required in FN121")
  }
}
