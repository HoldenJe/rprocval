#' fl_gt_tl - Identify where FL > TL
#'
#' @param FN125
#' @param makeplot
#'
#' @return
#' @export
#'

fn125_fl_gt_tl <- function(FN125, makeplot = F){
  if(length(unique(FN125$PRJ_CD)) > 1){usethis::ui_warn("Multiple projects included.")}
  if(length(unique(FN125$SPC)) > 1){
      usethis::ui_warn("Multiple species included. This is likely to cause issues with messages and plot titles.")
      # create custom different plot title
    }

  FN125$qid1_error <- ifelse(FN125$FLEN >= FN125$TLEN, T, F)

  if(makeplot){
    if(sum(!is.na(FN125$TLEN)) < 1 | sum(!is.na(FN125$FLEN)) < 1){
      usethis::ui_oops(paste0("No FLEN TLEN pairs exist for SPC == ", unique(FN125$SPC)))
      return(FN125)
    } else {
      require(ggplot2)
      print(ggplot(FN125, aes(FLEN, TLEN, color = qid1_error)) +
            geom_point()  +
            ggtitle(paste(unique(FN125$PRJ_CD), unique(FN125$SPC), sep=":"))
      )}
  }
  if(sum(FN125$qid1_error, na.rm = T) == 0){
      usethis::ui_done(
        paste0("No instances of FLEN > TLEN found for SPC == ",
               unique(FN125$SPC)))
    }

  if(sum(FN125$qid1_error, na.rm = T) > 0){
      n_flags <- sum(FN125$qid1_error, na.rm = T)
      done_message <- paste0(n_flags,
                             " instances FLEN > TLEN found for SPC == ",
                             unique(FN125$SPC),
                             "[`FN125 %>% filter(qid1_error)`]")
      usethis::ui_oops(done_message)
    }

  FN125
}
