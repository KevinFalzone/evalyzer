#' @title Authenticate to the Evalyzer platform.
#'
#' @description Authenticate to the Evalyzer platform.
#'
#' @param username User name.
#' @param password User password.
#' @return A rvest session.
#'
#' @importFrom rvest html_form
#' @importFrom rvest html_form_set
#' @importFrom rvest session
#' @importFrom rvest session_submit
#'
#' @export
#'
#' @examples
#' session <- auth("my-username", "my-password")
auth <- function(username, password) {

  # Check user inputs
  if (missing(username)) {
    stop_if_missing("username")
  }

  if (missing(password)) {
    stop_if_missing("password")
  }

  if (!(is.character(username))) {
    stop_if_bad_type("username", "character string")
  }

  if (!(is.character(password))) {
    stop_if_bad_type("password", "character string")
  }

  if (length(username) > 1) {
    stop_gt_length("username", "1")
  }

  if (length(password) > 1) {
    stop_gt_length("password", "1")
  }

  session <- rvest::session("https://app.evalyzer.com/evalyzer/users/login")

  form <- rvest::html_form(session)[[1]]

  filled_form <-
    rvest::html_form_set(form,
      `data[User][username]` = username,
      `data[User][password]` = password
    )

  submited_form <-
    rvest::session_submit(
      session,
      filled_form
    )
}
