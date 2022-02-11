#' Check FN123 Columns
#'
#' @param FN123 FN123 table that adheres to the FN2 data model
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' fn123_column_check(FN123)
#' }
#'

fn123_column_check <- function(FN123) {
  req_names <-
    c("PRJ_CD",
      "SAM",
      "EFF",
      "SPC",
      "CATCNT",
      "CATWT",
      "BIOCNT"
      )
  if (all(req_names %in% names(FN123))) {
    usethis::ui_done("FN123 has required fields")
  }

  if (!("PRJ_CD" %in% names(FN123))) {
    usethis::ui_oops("PRJ_CD is required in FN123")
  }
  if (!("SAM" %in% names(FN123))) {
    usethis::ui_oops("SAM is required in FN123")
  }
  if (!("EFF" %in% names(FN123))) {
    usethis::ui_oops("EFF is required in FN123")
  }
  if (!("SPC" %in% names(FN123))) {
    usethis::ui_oops("SPC is required in FN123")
  }
  if (!("CATCNT" %in% names(FN123))) {
    usethis::ui_oops("CATCNT is required in FN123")
  }
  if (!("CATWT" %in% names(FN123))) {
    usethis::ui_oops("CATWT is required in FN123")
  }
  if (!("BIOCNT" %in% names(FN123))) {
    usethis::ui_oops("BIOCNT is required in FN123")
  }
}
