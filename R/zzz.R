.onAttach <- function(...) {
  fn2message1 <- "Notice: functions in this package are designed to use FN2 variable names"
  fn2message2 <- "Data not using this model will need to be formatted to fit the FN2 naming convention"
  fn2message <- paste(fn2message1, fn2message2, sep="\n")
  packageStartupMessage(fn2message)
}
