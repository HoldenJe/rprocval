
#' fl_tl_lm - Compares FL:TL using lm relationship
#'
#' @param FN125
#' @param makeplot
#' @param fail_criteria
#'
#' @return
#' @export
#'

fn125_fl_tl_lm <- function(FN125,  makeplot = F, fail_criteria = 0.1) {
  if(length(unique(FN125$PRJ_CD)) > 1){usethis::ui_warn("Multiple projects included.")}
  if(length(unique(FN125$SPC)) > 1){
    usethis::ui_warn("Multiple species included.")
    # create custom different plot title
  }
  if(sum(!(is.na(FN125$TLEN))) < 10 | sum(!(is.na(FN125$FLEN))) < 10){
    usethis::ui_oops(paste0("Fewer than 10 records exists for SPC == ", unique(FN125$SPC)))
    return(FN125)
  } else {
    require(dplyr)
    mod <- lm(TLEN~FLEN, FN125)
    FN125 <- FN125  %>%
      mutate(PRED_FLEN = (TLEN - mod$coef[1])/mod$coef[2]) %>%
      mutate(PRED_TLEN = FLEN*mod$coef[2] + mod$coef[1])  %>%
      mutate(LogRatioTLEN = abs(log10(TLEN)-log10(PRED_TLEN)))  %>%
      mutate(qid2_error = ifelse(LogRatioTLEN > fail_criteria, T, F))
    if(makeplot){
      require(ggplot2)
      print(ggplot(FN125, aes(FLEN, TLEN, color = qid2_error)) +
            geom_point() +
            ggtitle(paste(unique(FN125$PRJ_CD), unique(FN125$SPC), sep = ":")))
    }
    if(sum(FN125$qid2_error, na.rm = T) == 0){
      done_message <- paste0("No instances of irregular FL~TL values found for SPC == ", unique(FN125$SPC))
      usethis::ui_done(done_message)
    }

    if(sum(FN125$qid2_error, na.rm = T) > 0){
      n_flags <- sum(FN125$qid2_error, na.rm = T)
      done_message <- paste0(n_flags,
                             " instances of irregular FLEN~RWT values found for SPC == ",
                             unique(FN125$SPC),
                             "[`FN125 %>% filter(qid2_error)`]")
      usethis::ui_oops(done_message)
    }

    if(is.na(sum(FN125$qid2_error))){usethis::ui_warn("NA values in FLEN or TLEN")}
    FN125
  }
}
