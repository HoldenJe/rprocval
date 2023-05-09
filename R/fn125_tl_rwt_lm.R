#' tl_rwt_lm - Compares FL:RWT using nlm relationship
#'
#' @param FN125
#' @param makeplot
#' @param fail_criteria
#'
#' @return
#' @export
#'

fn125_tl_rwt_lm <-
  function(FN125,
           makeplot = F,
           fail_criteria = 0.3) {
    if (length(unique(FN125$PRJ_CD)) > 1) {
      usethis::ui_warn("Multiple projects included.")
    }
    if (length(unique(FN125$SPC)) > 1) {
      usethis::ui_warn(
        "Multiple species included. Function will still run but may produce unexpected results."
      )
    }
    # create variables RWT and TLEN
    nTLEN <- sum(!(is.na(FN125$TLEN)))
    nRWT <- sum(!(is.na(FN125$RWT)))
    if (nRWT < 10 | nTLEN < 10) {
      usethis::ui_oops(paste0("Fewer than 10 records exists for SPC == ", unique(FN125$SPC)))
      return(FN125)
    }
    if (nRWT >= 10 & nTLEN >=10) {
      # test that model will converge
      testfit <- NULL # set dummy variable to NULL
      try(testfit <-
            nls(
              RWT ~ alpha * TLEN ^ (beta),
              data = FN125,
              start = list(alpha = 0.000005, beta = 3)
            ), silent = T)

      if(is.null(testfit)){
        if(makeplot){
          require(ggplot2)
          print(ggplot(FN125, aes(TLEN, RWT)) +
                  geom_point() +
                  ggtitle(paste(
                    unique(FN125$PRJ_CD), unique(FN125$SPC), sep = ":"
                  ), subtitle = "nls did not converge"))}
        usethis::ui_oops(paste0("nls model could not converge for SPC == ", unique(FN125$SPC)))
        return(FN125)
        }

      if (!is.null(testfit)) {
        TLEN.RWT <-
          nls(
            RWT ~ alpha * TLEN ^ (beta),
            data = FN125,
            start = list(alpha = 0.000005, beta = 3)
          )
        FN125 <- FN125 |>
          mutate(PRED_TLENRWT = coefficients(TLEN.RWT)[1] * TLEN ^ coefficients(TLEN.RWT)[2]) |>
          mutate(LogRatioTLENRWT = abs(log10(PRED_TLENRWT) - log10(RWT))) |>
          mutate(qid4_error = ifelse(LogRatioTLENRWT > fail_criteria, T, F))

        if (makeplot) {
          require(ggplot2)
          print(ggplot(FN125, aes(TLEN, RWT, color = qid4_error)) +
                  geom_point() +
                  ggtitle(paste(
                    unique(FN125$PRJ_CD), unique(FN125$SPC), sep = ":"
                  )))
        }

        if (sum(FN125$qid4_error, na.rm = T) == 0) {
          done_message <-
            paste0("No instances of irregular TLEN~RWT values found for SPC == ",
                   unique(FN125$SPC))
          usethis::ui_done(done_message)
        }

        if (sum(FN125$qid4_error, na.rm = T) > 0) {
          n_flags <- sum(FN125$qid4_error, na.rm = T)
          done_message <- paste0(
            n_flags,
            " instances of irregular TLEN~RWT values found for SPC == ",
            unique(FN125$SPC),
            "[`FN125 %>% filter(qid4_error)`]"
          )
          usethis::ui_oops(done_message)
        }
        if (is.na(sum(FN125$qid4_error))) {
          usethis::ui_warn("NA values in TLEN or RWT")
        }

        FN125
      }
    }
  }
