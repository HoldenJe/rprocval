#' Check FN121 Columns
#'
#' @param FN121
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' fn121_column_check(FN121)
#' }
fn121_column_check <- function(FN121) {
  req_names <-
    c("PRJ_CD",
      "SAM",
      "EFFDT0",
      "EFFDT1",
      "EFFTM0",
      "EFFTM1",
      "SIDEP",
      "EFFDUR")
  if (all(req_names %in% names(FN121))) {
    usethis::ui_done("FN121 has required fields")
  }

  if (!("PRJ_CD" %in% names(FN121))) {
    usethis::ui_oops("PRJ_CD is required")
  }
  if (!("SAM" %in% names(FN121))) {
    usethis::ui_oops("SAM is required")
  }
  if (!("EFFDT0" %in% names(FN121))) {
    usethis::ui_oops("EFFDT0 is required")
  }
  if (!("EFFDT1" %in% names(FN121))) {
    usethis::ui_oops("EFFDT1 is required")
  }
  if (!("EFFTM0" %in% names(FN121))) {
    usethis::ui_oops("EFFTM0 is required")
  }
  if (!("EFFTM1" %in% names(FN121))) {
    usethis::ui_oops("EFFTM1 is required")
  }
  if (!("SIDEP" %in% names(FN121))) {
    usethis::ui_oops("SIDEP is required")
  }
  if (!("EFFDUR" %in% names(FN121))) {
    usethis::ui_oops("EFFDUR is required")
  }
  if (!("EFFST" %in% names(FN121))) {
    usethis::ui_oops("EFFST is required")
  }
}
