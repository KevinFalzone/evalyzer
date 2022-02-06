#' @title Stop the execution and show the missing argument.
#'
#' @param arg Argument name.
#'
#' @noRd
stop_if_missing <- function(arg) {
  stop(
    paste0("`", arg, "`", " argument is missing.")
  )
}

#' @title Stop the execution and show the expected type value.
#'
#' @param arg Argument name.
#' @param expected_type Expected type.
#'
#' @noRd
stop_if_bad_type <- function(arg, expected_type) {
  stop(
    paste0(expected_type, " is expected for", "`", arg, "`", ".")
  )
}

#' @title Stop the execution and show the expected length.
#'
#' @param arg Argument name.
#' @param len Expected length.
#'
#' @noRd
stop_gt_length <- function(arg, len) {
  stop(
    paste0("`", arg, "`", " vector length is greater than ", len, ".")
  )
}
