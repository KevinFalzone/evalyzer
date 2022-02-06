#' @title Get an overview of all users
#'
#' @description Get some data about each user:
#'     * the user id,
#'     * the test session date and time,
#'     * the task performances (task duration and task status),
#'     * the total duration of the task(s).
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param id The id of a project. The id lies in url like
#'   \url{https://app.evalyzer.com/evalyzer/analyzes/participants/<id>}
#'
#' @return A `data.frame` with the user ids, the dates and times, the task
#'         performances (task duration and task status), the total duration of
#'         the task(s).
#'
#' @importFrom rvest is.session
#'
#' @export
#'
#' @examples
#' session <- auth("my-username", "my-password")
#' user_overview <- get_overview(session, "project-id")
get_overview <- function(session, id) {
  if (missing(session)) {
    stop_if_missing("session")
  }

  if (missing(id)) {
    stop_if_missing("id")
  }

  if (!(rvest::is.session(session))) {
    stop_if_bad_type("rvest session", "session")
  }

  webpage <- retrieve_user_list_webpage(session, id)

  webpage |>
    extract_user_table() |>
    clean_user_table()
}
