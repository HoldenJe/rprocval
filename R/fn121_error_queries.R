#' FN121 error checks
#'
#' @param FN121 FN121 table
#' @param FN123 FN123 table
#'
#' @return Usethis messages and a list of dataframes that fail tests
#' @export
#'
#' @examples
#' #' \dontrun{
#' fn121_errors <- fn121_error_queries(FN121, FN123)
#' }

fn121_error_queries <- function(FN121, FN123){
  require(dplyr)

  # check for duplicate key fields ---------
  keyfield <- FN121 %>% group_by(PRJ_CD, SAM) %>%
    summarize(N = n()) %>%
    filter(N>1)
  if(nrow(keyfield) > 0){
    usethis::ui_oops(paste0(nrow(keyfield), " key field violations exist"))
  } else {usethis::ui_done("No duplicate key fields found.")}
  #

  # checking missing dates --------
  missing_datetime <- FN121 %>%
    filter(is.na(EFFDT0) | is.na(EFFDT1) | is.na(EFFTM0) | is.na(EFFTM1))
  if(nrow(missing_datetime) > 0){
    usethis::ui_oops(paste0(nrow(missing_datetime), " are missing a date or time."))
  } else {usethis::ui_done("All records have dates and times.")}

  # Check project code year and FN121 years match ----------
  prjcd_setdate <- FN121 %>%
    mutate(prj_yr = as.numeric(substr(PRJ_CD, 7,8))) %>%
    mutate(PRJ_YEAR = ifelse(prj_yr < 40, prj_yr + 2000, prj_yr + 1900)) %>%
    filter(PRJ_YEAR != lubridate::year(EFFDT0) | PRJ_YEAR != lubridate::year(EFFDT1))
  if(nrow(prjcd_setdate) > 0){
    usethis::ui_oops(paste0(nrow(prjcd_setdate), " records have a set or lift date that differs from project code year."))
  } else {usethis::ui_done("All records occur in the same year as indicated by the project code.")}

   # check for parents with no childs ----------
  # it's possible these are empty nets
  emptynet <- anti_join(FN121, FN123, by=c("PRJ_CD", "SAM"))
  if(nrow(emptynet) > 0){
    usethis::ui_oops(paste0(nrow(emptynet), " FN121 records have no child records in FN123. Verify these are empty nets."))
  } else {usethis::ui_done("All FN121 records have FN123 child records")}

  # check for childs with no parents ----------
  # this really should use the FN122
  orphaned_childs <- anti_join(FN123, FN121, by=c("PRJ_CD", "SAM"))
  if(nrow(orphaned_childs) > 0){
    usethis::ui_oops(paste0(nrow(orphaned_childs), " FN123 records are missing FN121 parents."))
  } else {usethis::ui_done("All FN123 records have FN121 parent records")}

  ## Check EFFDUR ----------
  large_effdur <- FN121 %>% filter(EFFDUR > 30)
  if(nrow(large_effdur) > 0){
    usethis::ui_oops(paste0(nrow(large_effdur), " records have EFFDUR > 30 hours"))
  } else {usethis::ui_done("No EFFDUR values are not excessively large.")}

  ## Check EFFDUR vs Calculated
  FN121 <- FN121 %>%
    mutate(SET = lubridate::ymd_hm(paste(EFFDT0, EFFTM0, sep= " ")),
           LIFT = lubridate::ymd_hm(paste(EFFDT1, EFFTM1, sep= " ")),
           CalcEFFDUR = as.numeric(difftime(LIFT, SET), units = "hours"),
           EFF_difference = abs(EFFDUR-abs(CalcEFFDUR)))

  neg_effdur <- FN121 %>% filter(CalcEFFDUR < 0)

  if(nrow(neg_effdur) > 0){
    usethis::ui_oops(paste0(nrow(neg_effdur), " records have a calculated effort duration < 0."))
  } else {usethis::ui_done("Calculated effort durations are all positive.")}

  ## Check report EFFDUR vs Calculated
  effdur_calceffdur <- FN121 %>%
    filter(EFF_difference > 1)

  if(nrow(effdur_calceffdur) > 0){
    usethis::ui_oops(paste0(nrow(effdur_calceffdur), " records have a calculated effort duration that differs significantly from EFFDUR"))
  } else {usethis::ui_done("Calculated effort durations are all comparable to EFFDUR")}

  # Check for invalid codes in fields ------
  ## EFFST ---------
  effst_error <- FN121 %>% filter(EFFST != 1)
  if(nrow(effst_error) > 0){
    usethis::ui_oops(paste0(nrow(effst_error), " records have values for EFFST that require review"))
  } else {usethis::ui_done("EFFST codes all valid.")}

  ## SITEM ----------
  sitem <- FN121 %>% filter(SITEM >40 | SITEM <= 0)
  if(nrow(sitem) > 0){
    usethis::ui_oops(paste0(nrow(effst_error), " records have invalid values for SITEM"))
  } else {usethis::ui_done("SITEM values are within expected range.")}

  # create error table ----
  error_tables <- list(
    keyfield = keyfield,
    missing_datetime = missing_datetime,
    prjcd_setdate = prjcd_setdate,
    emptynet = emptynet,
    orphaned_FN123 = orphaned_childs,
    large_effdur = large_effdur,
    neg_effdur = neg_effdur,
    effdur_calceffdur = effdur_calceffdur,
    effst_error = effst_error,
    sitem = sitem
  )
}


#' FN121 error checks based on GL Template 5
#'
#' @param FN121 FN121 table
#' @param FN123 FN123 table
#'
#' @return Usethis messages and a list of dataframes that fail tests
#' @export
#'
#' @examples
#' #' \dontrun{
#' fn121_errors <- fn121_error_queries_t5(FN121, FN123)
#' }

fn121_error_queries_t5 <- function(FN121, FN123){
  require(dplyr)

  # check for duplicate key fields ---------
  keyfield <- FN121 %>% group_by(PRJ_CD, SAM) %>%
    summarize(N = n()) %>%
    filter(N>1)
  if(nrow(keyfield) > 0){
    usethis::ui_oops(paste0(nrow(keyfield), " key field violations exist"))
  } else {usethis::ui_done("No duplicate key fields found.")}
  #

  # checking missing dates --------
  missing_datetime <- FN121 %>%
    filter(is.na(EFFDT0) | is.na(EFFDT1) | is.na(EFFTM0) | is.na(EFFTM1))
  if(nrow(missing_datetime) > 0){
    usethis::ui_oops(paste0(nrow(missing_datetime), " are missing a date or time."))
  } else {usethis::ui_done("All records have dates and times.")}

  # Check project code year and FN121 years match ----------
  prjcd_setdate <- FN121 %>%
    mutate(prj_yr = as.numeric(substr(PRJ_CD, 7,8))) %>%
    mutate(PRJ_YEAR = ifelse(prj_yr < 40, prj_yr + 2000, prj_yr + 1900)) %>%
    filter(PRJ_YEAR != lubridate::year(EFFDT0) | PRJ_YEAR != lubridate::year(EFFDT1))
  if(nrow(prjcd_setdate) > 0){
    usethis::ui_oops(paste0(nrow(prjcd_setdate), " records have a set or lift date that differs from project code year."))
  } else {usethis::ui_done("All records occur in the same year as indicated by the project code.")}

  # check for parents with no childs ----------
  # it's possible these are empty nets
  emptynet <- anti_join(FN121, FN123, by=c("PRJ_CD", "SAM"))
  if(nrow(emptynet) > 0){
    usethis::ui_oops(paste0(nrow(emptynet), " FN121 records have no child records in FN123. Verify these are empty nets."))
  } else {usethis::ui_done("All FN121 records have FN123 child records")}

  # check for childs with no parents ----------
  # this really should use the FN122
  orphaned_childs <- anti_join(FN123, FN121, by=c("PRJ_CD", "SAM"))
  if(nrow(orphaned_childs) > 0){
    usethis::ui_oops(paste0(nrow(orphaned_childs), " FN123 records are missing FN121 parents."))
  } else {usethis::ui_done("All FN123 records have FN121 parent records")}

  ## Check EFFDUR ----------
  large_effdur <- FN121 %>% filter(EFFDUR > 30)
  if(nrow(large_effdur) > 0){
    usethis::ui_oops(paste0(nrow(large_effdur), " records have EFFDUR > 30 hours"))
  } else {usethis::ui_done("No EFFDUR values are not excessively large.")}

  ## Check EFFDUR vs Calculated
  FN121 <- FN121 %>%
    mutate(SET = lubridate::ymd_hms(paste(EFFDT0, format(EFFTM0, format = "%H:%M:%S"), sep= " ")),
           LIFT = lubridate::ymd_hms(paste(EFFDT1, format(EFFTM1, format = "%H:%M:%S"), sep= " ")),
           CalcEFFDUR = as.numeric(difftime(LIFT, SET), units = "hours"),
           EFF_difference = abs(EFFDUR-abs(CalcEFFDUR)))

  neg_effdur <- FN121 %>% filter(CalcEFFDUR < 0)

  if(nrow(neg_effdur) > 0){
    usethis::ui_oops(paste0(nrow(neg_effdur), " records have a calculated effort duration < 0."))
  } else {usethis::ui_done("Calculated effort durations are all positive.")}

  ## Check report EFFDUR vs Calculated
  effdur_calceffdur <- FN121 %>%
    filter(EFF_difference > 1)

  if(nrow(effdur_calceffdur) > 0){
    usethis::ui_oops(paste0(nrow(effdur_calceffdur), " records have a calculated effort duration that differs significantly from EFFDUR"))
  } else {usethis::ui_done("Calculated effort durations are all comparable to EFFDUR")}

  # Check for invalid codes in fields ------
  ## EFFST ---------
  effst_error <- FN121 %>% filter(EFFST != 1)
  if(nrow(effst_error) > 0){
    usethis::ui_oops(paste0(nrow(effst_error), " records have values for EFFST that require review"))
  } else {usethis::ui_done("EFFST codes all valid.")}

  ## SITEM ----------
  sitem <- FN121 %>% filter(SITEM0 >40 | SITEM0 <= 0)
  if(nrow(sitem) > 0){
    usethis::ui_oops(paste0(nrow(effst_error), " records have invalid values for SITEM0"))
  } else {usethis::ui_done("SITEM0 values are within expected range.")}

  # create error table ----
  error_tables <- list(
    keyfield = keyfield,
    missing_datetime = missing_datetime,
    prjcd_setdate = prjcd_setdate,
    emptynet = emptynet,
    orphaned_FN123 = orphaned_childs,
    large_effdur = large_effdur,
    neg_effdur = neg_effdur,
    effdur_calceffdur = effdur_calceffdur,
    effst_error = effst_error,
    sitem = sitem
  )
}
