# template 5 functions

template5_tests <- function(FN012 = FN012,
                            FN022 = FN022,
                            FN026 = FN026,
                            FN026_Subspace = FN026_Subspace,
                            FN028 = FN028,
                            FN121 = FN121,
                            FN123 = FN123,
                            FN124 = FN124,
                            FN125 = FN125) {

}



#' Check FN125 data against FN012 table
#'
#' @param FN125 FN125 table from Template 5
#' @param FN012 FN012 table from Temaplate 5
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
#' @param FN125
#' @param FN012
#' @param makeplot
#'
#' @return
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




