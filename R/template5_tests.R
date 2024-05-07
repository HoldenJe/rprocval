# template 5 functions

#' Basic database structural tests
#'
#' @param FN012
#' @param FN022
#' @param FN026
#' @param FN026_Subspace
#' @param FN028
#' @param FN121
#' @param FN123
#' @param FN124
#' @param FN125
#'
#' @return returns usethis messages
#' @export
#' @examples
#' \dontrun{
#' fn_template5_basic_checks(FN012, FN022, FN026_Subspace, FN028, FN121, FN123, FN124, FN125)
#' }
#'
fn_template5_basic_checks <- function(
                            FN012 = FN012,
                            FN022 = FN022,
                            FN026_Subspace = FN026_Subspace,
                            FN028 = FN028,
                            FN121 = FN121,
                            FN123 = FN123,
                            FN124 = FN124,
                            FN125 = FN125) {
  if(all(FN121$SSN %in% FN022$SSN)){
    usethis::ui_done("All FN121 SSN values exist in FN022")
  } else {usethis::ui_oops("FN121 has SSN values that do not exist in FN022")}

  if(all(FN121$SUBSPACE %in% FN026_Subspace$SUBSPACE)){
    usethis::ui_done("All FN121 SUBSPACE values exist in FN026_Subspace")
  } else {usethis::ui_oops("FN121 has SUBSPACE values that do not exist in FN026_Subspace")}

  if(all(FN123$SPC %in% FN012$SPC)){
    usethis::ui_done("All FN123 SPC values exist in FN022")
  } else {usethis::ui_oops("FN123 has SPC values that do not exist in FN022")}

  if(all(FN124$SPC %in% FN012$SPC)){
    usethis::ui_done("All FN124 SPC values exist in FN022")
  } else {usethis::ui_oops("FN124 has SPC values that do not exist in FN022")}

  if(all(FN125$SPC %in% FN012$SPC)){
    usethis::ui_done("All FN125 SPC values exist in FN022")
  } else {usethis::ui_oops("FN125 has SPC values that do not exist in FN022")}

  # add more tests
}



#' Check FN125 data against FN012 table
#'
#' @param FN125 FN125 table from Template 5
#' @param FN012 FN012 table from Template 5
#' @param makeplot option to produce plot. Default = TRUE
#'
#' @return FN125 table with Fulton K, ranges from FN012 and categorization
#' @export
#'
#' @examples
#' \dontrun{
#' FN125 <- fn125_fulton(FN125, FN012, makeplot = T)
#' FN125 <- fn125_fulton(FN125, FN012, makeplot = F)
#' }

fn125_fulton <- function(FN125, FN012, makeplot = TRUE){
  FN125 <- left_join(FN125, FN012, by = c("PRJ_CD", "SPC", "GRP")) %>%
    mutate(FultonK = RWT/(TLEN^3)*100000) %>%
    mutate(FultonCheck = case_when(
      FultonK > K_MAX_ERROR | FultonK < K_MIN_ERROR ~"Exceeds_Max",
      FultonK > K_MAX_WARN | FultonK < K_MIN_WARN ~"Exceeds_Warning",
      .default = "Pass"
    ))

  if(makeplot) {
    p <- ggplot(FN125, aes(TLEN, RWT, color = FultonCheck)) +
    geom_point() +
    facet_wrap(~SPC, scales = "free") +
    ggtitle("Fulton's K")

    print(p)
    }
  return(FN125)
}



#' Check FN125 data against FN012 min/max lengths
#'
#' @param FN125 FN125 table from Template 5
#' @param FN012 FN012 table from Template 5
#' @param makeplot option to produce plot. Default = TRUE
#'
#' @return FN125 table with length checks from FN012
#' @export
#'
#' @examples
#' \dontrun{
#' FN125 <- fn125_minmax_TLEN(FN125, FN012)
#' FN125 <- fn125_minmax_TLEN(FN125, FN012, makeplot = F)
#' }
fn125_minmax_TLEN <- function(FN125, FN012, makeplot = TRUE){
  FN125 <- left_join(FN125, FN012, by = c("PRJ_CD", "SPC", "GRP")) %>%
    mutate(LengthCheck = case_when(
      TLEN > TLEN_MAX ~"Exceeds_Max",
      TLEN < TLEN_MIN ~"Below_Min",
      .default = "Pass"
    ))

  if(makeplot) {
    p <- ggplot(FN125, aes(TLEN, fill = LengthCheck)) +
      geom_histogram(binwidth = 5) +
      facet_wrap(~SPC, scales = "free_y") +
      ggtitle("FN125 Checks")

    print(p)
  }
  return(FN125)
}



#' Checks FN124 data against FN012 min/max lengths
#'
#' @param FN124 FN124 table from Template 5
#' @param FN012 FN012 table from Template 5
#' @param makeplot option to produce plot. Default = TRUE
#'
#' @return FN124 table with length checks from FN012
#' @export
#'
#' @examples
fn124_minmax_TLEN <- function(FN124, FN012, makeplot = TRUE){

  FN124 <- left_join(FN124, FN012, by = c("PRJ_CD", "SPC", "GRP")) %>%
    mutate(LengthCheck = case_when(
      SIZ > TLEN_MAX ~"Exceeds_Max",
      SIZ < TLEN_MIN ~"Below_Min",
      .default = "Pass"
    ))

  if(makeplot) {
    FN124 <- FN124 %>% group_by(PRJ_CD, SPC, SIZ, LengthCheck) %>%
      summarize(SIZCNT = sum(SIZCNT))
    p <- ggplot(FN124, aes(SIZ, SIZCNT, fill = LengthCheck)) +
      geom_bar(stat = "identity") +
      facet_wrap(~SPC, scales = "free_y") +
      ggtitle("FN124 check")

    print(p)
  }
  return(FN124)
}


