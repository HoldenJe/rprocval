
#' vonb_check_flen - Fits VBL growth function to FLEN and AGE
#'
#' @param FN125
#' @param makeplot
#' @param fail_criteria
#'
#' @return
#' @export
#'

fn125_vonb_check_flen <- function(FN125, makeplot = FALSE, fail_criteria = 0.2){
  # This section will be useful when function updated to accept FLEN or TLEN as fishlength arg
  # if(!(fishlength %in% names(FN125))) {
  #   usethis::ui_stop(paste(fishlength, " is not a valid field."))
  #   }
  if(length(unique(FN125$PRJ_CD)) > 1){usethis::ui_warn("Multiple projects included.")}
  if(length(unique(FN125$SPC)) > 1){
    usethis::ui_warn("Multiple species included.")
    # create custom different plot title
  }
  if(sum(!(is.na(FN125$AGE))) < 10 | sum(!(is.na(FN125$FLEN))) < 10){
    usethis::ui_oops(paste0("Fewer than 10 records exists for SPC == ", unique(FN125$SPC)))
    return(FN125)
  } else {
    datain <- FN125
    FN125 <- FN125[!(is.na(FN125$AGE)) & !(is.na(FN125$FLEN)), ]

    von.bert <- (nls(FLEN~Linf*(1-exp(-k*(AGE-t0))), data=FN125,
                     start=list(Linf=500, k=.15, t0 = -1)))
    l <- coef(von.bert)[1]
    m <- coef(von.bert)[2]
    n <- coef(von.bert)[3]

    FN125$PRED_FLEN <-  predict(von.bert)
    FN125$LogRatioAge <- abs((log10(FN125$FLEN))-(log10(FN125$PRED_FLEN)))
    FN125$qid5_error <- ifelse(FN125$LogRatioAge > fail_criteria, T, F)

    if(makeplot) {
      require(ggplot2)
      age_fit <- data.frame(AGE = 0:max(FN125$AGE))
      age_fit$FLEN <- predict(von.bert, age_fit)
      vbplot <- ggplot() +
        geom_point(data = FN125, aes(AGE, FLEN, color = qid5_error)) +
        geom_line(data = age_fit, aes(AGE, FLEN), lty = 2) +
        ggtitle(paste(unique(FN125$PRJ_CD), unique(FN125$SPC), sep = ": "))
      print(vbplot)
    }

    if(sum(FN125$qid5_error, na.rm = T) == 0){
      done_message <- paste0("No instances of irregular FLEN~AGE values found for SPC == ", unique(FN125$SPC))
      usethis::ui_done(done_message)
    }
    if(sum(FN125$qid5_error, na.rm = T) > 0){
      n_flags <- sum(FN125$qid5_error, na.rm = T)
      done_message <- paste0(n_flags, " instances of irregular FLEN~AGE values found for SPC == ", unique(FN125$SPC))
      usethis::ui_oops(done_message)
      usethis::ui_info("Use `FN125 %>% filter(qid5_error)` to view flagged records")
    }


    FN125 <- suppressMessages(left_join(datain, FN125)) # this is generally not advised but I think suitable here
    FN125
  }
}
