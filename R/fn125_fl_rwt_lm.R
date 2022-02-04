
#' fl_rwt_lm - Compares FL:RWT using nlm relationship
#'
#' @param FN125
#' @param makeplot
#' @param fail_criteria
#'
#' @return
#' @export
#'

fn125_fl_rwt_lm <- function(FN125, makeplot = F, fail_criteria = 0.3) {
  if(length(unique(FN125$PRJ_CD)) > 1){usethis::ui_warn("Multiple projects included.")}
  if(length(unique(FN125$SPC)) > 1){
    usethis::ui_warn("Multiple species included.")
    # create custom different plot title
  }
  if(sum(!(is.na(FN125$RWT))) < 10 | sum(!(is.na(FN125$FLEN))) < 10){
    usethis::ui_oops(paste0("Fewer than 10 records exists for SPC == ", unique(FN125$SPC)))
    return(FN125)
  } else {
    FLEN.RWT <- nls(RWT~alpha*FLEN^(beta), data=FN125, start=list(alpha=0.000005, beta=3))
    FN125 <- FN125 |>
      mutate(PRED_RWT = coefficients(FLEN.RWT)[1]*FLEN^coefficients(FLEN.RWT)[2]) |>
      mutate(LogRatioRWT = abs(log10(PRED_RWT)-log10(RWT))) |>
      mutate(qid3_error = ifelse(LogRatioRWT > fail_criteria, T, F))

    if(makeplot){
      require(ggplot2)
      print(ggplot(FN125, aes(FLEN, RWT, color = qid3_error)) +
            geom_point() +
            ggtitle(paste(unique(FN125$PRJ_CD), unique(FN125$SPC), sep = ":")))
    }

    if(sum(FN125$qid3_error, na.rm = T) == 0){
      done_message <- paste0("No instances of irregular FLEN~RWT values found for SPC == ", unique(FN125$SPC))
      usethis::ui_done(done_message)
    }

    if(sum(FN125$qid3_error, na.rm = T) > 0){
      n_flags <- sum(FN125$qid3_error, na.rm = T)
      done_message <- paste0(n_flags,
                             " instances of irregular FLEN~RWT values found for SPC == ",
                             unique(FN125$SPC),
                             "[`FN125 %>% filter(qid3_error)`]")
      usethis::ui_oops(done_message)
    }

    if(is.na(sum(FN125$qid3_error))){usethis::ui_warn("NA values in FLEN or RWT")}

    FN125
  }
}
