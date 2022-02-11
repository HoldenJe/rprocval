
#' FN123 Data Check
#'
#' @param FN123 FN123 table with PRJ_CD, SAM, EFF, SPC, CATCNT, CATWT, BIOCNT
#' @param FN125 FN125 table with PRJ_CD, SAM, EFF, SPC, FISH, RWT
#'
#' @return list of named tables with records that should be checked.
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' ErrorTables <- fn123_error_queries(FN123, FN125)
#' lapply(ErrorTables, head)
#' }

fn123_error_queries <- function(FN123, FN125){
  require(dplyr)
  FN125_summary <- FN125 %>%
    group_by(PRJ_CD, SAM, EFF, SPC) %>%
    summarize(BIOCNT125 = n(), CATWT125 = sum(RWT)/1000)

  # Merge columns with FN123
  FN123 <- left_join(FN123, FN125_summary, by =c('PRJ_CD', 'SAM', 'EFF', 'SPC'))

  # Do error Checks
  ## CATCNT not null/na
  has_na_catcnt <- FN123 %>% filter(is.na(CATCNT))
  n_has_na_catcnt <- nrow(has_na_catcnt)
  if(n_has_na_catcnt >=1) {
    usethis::ui_oops(paste0("FN123 has ", n_has_na_catcnt, " missing CATCNTs"))
  } else {usethis::ui_done("No missing CATCNTs")}

  ## BIOCNT not na
  has_na_biocnt <- FN123 %>% filter(is.na(BIOCNT))
  n_has_na_biocnt <- nrow(has_na_biocnt)
  if(n_has_na_biocnt >=1) {
    usethis::ui_oops(paste0("FN123 has ", n_has_na_biocnt, " missing BIOCNTs"))
  } else {usethis::ui_done("No missing BIOCNTs")}

  # Missing CATWT
  has_na_catwt <- FN123 %>% filter(is.na(CATWT))
  n_has_na_catwt <- nrow(has_na_catwt)
  if(n_has_na_catwt >=1) {
    usethis::ui_oops(paste0("FN123 has ", n_has_na_catwt, " missing CATWTs"))
  } else {usethis::ui_done("No missing CATWTs")}

  # Check for FN123 records with missing childs
  missing_fn125_child <- anti_join(FN123, FN125,
                                   by = c("PRJ_CD", "SAM", "SPC", "EFF")) %>%
    filter(BIOCNT >=1)
  n_missing_fn125_child <- nrow(missing_fn125_child)
  if(n_missing_fn125_child >1) {
    usethis::ui_oops(paste0("FN123 has ", n_missing_fn125_child,
                            " records missing FN125 childs"))
  } else {usethis::ui_done("No missing FN125 child records")}

  # check for orphaned FN125 - checks errors with key fields, not N fish
  orphaned_fn125 <- anti_join(FN125, FN123, by =c("PRJ_CD", "SAM", "SPC", "EFF"))
  n_orphaned_fn125 <- nrow(orphaned_fn125)
  if(n_orphaned_fn125 >=1) {
    usethis::ui_oops(paste0("FN125 has ", n_orphaned_fn125,
                            " orphaned FN125 records"))
  } else {usethis::ui_done("No missing FN125 child records")}

  # BIOCNT must equal BIOCNT125
  # This is akin to orphaned FN125 as FN125
  # but some issues may be data entry errors on BIOCNT
  bio125_bio_notequal <- FN123 %>% filter(BIOCNT != BIOCNT125)
  n_bio125_bio_notequal <- nrow(bio125_bio_notequal)
  if(n_bio125_bio_notequal >=1) {
    usethis::ui_oops(paste0("FN123 has ", n_bio125_bio_notequal,
                            " records where BIOCNT not equal N FN125"))
  } else {usethis::ui_done("FN123 BIOCNTs all match FN125 counts")}

  # CATCNT must be greater than BIOCNT
  biocnt_gt_catcnt <- FN123 %>% filter(CATCNT < BIOCNT)
  n_biocnt_gt_catcnt <- nrow(biocnt_gt_catcnt)
  if(n_biocnt_gt_catcnt >=1) {
    usethis::ui_oops(paste0("FN123 has ", n_biocnt_gt_catcnt,
                            " records where BIOCNT > CATCNT"))
  } else {usethis::ui_done("No FN123 records where BIOCNT > CATCNT")}


  # BIOCNT125 can't be greater than CATCNT
  biocnt125_gt_catcnt <- FN123 %>% filter(CATCNT < BIOCNT125)
  n_biocnt125_gt_catcnt <- nrow(biocnt125_gt_catcnt)
  if(n_biocnt125_gt_catcnt >=1) {
    usethis::ui_oops(paste0("FN123 has ", n_biocnt125_gt_catcnt,
                            " records where FN125 records > CATCNT"))
  } else {usethis::ui_done("No FN123 records where N FN125 records > CATCNT")}

  # This isn't necessarily an error but a good table to review
  # FN123 %>% filter(CATCNT > BIOCNT)

  # Check for strange values
  # check mean weight (CATWT/CATCNT) is not huge
  # similar to the FN125 max length check the fail criteria is species dependent
  # rather than a fixed value this could be generated from the FN125 and would
  # then be species, mesh, lake, project specific
  spc_max_weight <- 10 # 10kg is reasonable for walleye
  # this will generally indicate when CATWT is in g not kg
  large_weight <- FN123 %>% filter(CATWT/CATCNT > spc_max_weight)
  n_large_weight <- nrow(large_weight)
  if(n_large_weight >=1) {
    usethis::ui_oops(paste0("FN123 has ", n_large_weight,
                            " records where the mean weight of fish is very high."))
    usethis::ui_info("It's possible that CATWT is in grams not Kg")
  } else {usethis::ui_done("No FN123 records where average fish weight is very high.")}
  # general fix is CATWT/1000 then re-run

  # CATWT doesn't have to equal CATWT125
  # (generically - think programs with subsampling)
  # check that mean fish size in FN123 not significantly different than FN125
  avg_fn123_wt_diff_fn125 <- FN123 %>%
    mutate(mn_wt_123 = CATWT/CATCNT, mn_wt_125 = CATWT125/BIOCNT125) %>%
    mutate(mn_wt_logratio = abs(log10(mn_wt_123)-log10(mn_wt_125))) %>%
    filter(mn_wt_logratio > 0.2)
  n_log_ratio_wt <- nrow(avg_fn123_wt_diff_fn125)
  if(n_log_ratio_wt >=1) {
    usethis::ui_oops(paste0("FN123 has ", n_log_ratio_wt,
                            " records where the mean weight of fish differs from FN125 weights."))
  } else {usethis::ui_done("No FN123 records where average fish weight differs from FN125.")}

  # this is BIG number of errors... but..
  # this is currently returning errors that are mostly likely CATCNT, BIOCNT errors
  # will be more informative when earlier errors are cleaned

  error_tables <- list(NA_CATCNT = has_na_catcnt,
                       NA_BIOCNT = has_na_biocnt,
                       NA_CATWT = has_na_catwt,
                       Missing_FN125 = missing_fn125_child,
                       Orphaned_FN125 = orphaned_fn125,
                       BIOCNT_differs_FN125 = bio125_bio_notequal,
                       BIOCNT_gt_CATCNT = biocnt_gt_catcnt,
                       FN125_gt_CATCNT = biocnt125_gt_catcnt,
                       Very_Large_Avg_RWT = large_weight,
                       Avg_RWT_differs_in_FN123 = avg_fn123_wt_diff_fn125)
}
