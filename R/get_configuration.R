#' @title Get configuration of each user
#'
#' @description Get configuration of each user for each task.
#' Configuration information includes:
#'   * the screen resolution information,
#'   * the browser information,
#'   * the operating system information.
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param id The id of a project. The id lies in url like
#'   \url{https://app.evalyzer.com/evalyzer/analyzes/participants/<id>}
#'
#' @return A `data.frame` with the screen resolution, browser, operating system
#'         information.
#'
#' @importFrom rvest is.session
#'
#' @export
#'
#' @examples
#' session <- auth("my-username", "my-password")
#' user_configuration <- get_configuration(session, "project-id")
get_configuration <- function(session, id) {
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

  user_profile_paths <- extract_user_profile_links(webpage)

  user_task_paths <- lapply(user_profile_paths, function(path) {
    webpage <- retrieve_user_profile_webpage(session, path)

    user_id <- extract_user_id_from_url(path)

    user_task_paths <- extract_user_task_links(webpage)

    data.frame(
      user_id = user_id,
      user_task_paths = user_task_paths
    )
  })

  user_task_paths <- do.call("rbind", user_task_paths)

  user_data <- mapply(
    function(user_id, user_task_path) {
      webpage <- retrieve_user_task_webpage(session, user_task_path)

      user_id <- user_id

      datetime <- extract_datetime(webpage)

      task_number <- extract_task_number(webpage)

      screen_resolution_information <-
        extract_screen_resolution_information(webpage)

      browser_information <- extract_browser_information(webpage)

      operating_system_information <-
        extract_operating_system_information(webpage)

      data.frame(
        user_id = user_id,
        datetime = datetime,
        task_number = task_number,
        screen_resolution = screen_resolution_information,
        browser = browser_information,
        operating_system = operating_system_information
      )
    },
    user_task_paths$user_id,
    user_task_paths$user_task_paths,
    SIMPLIFY = FALSE
  )

  user_data <- do.call("rbind", user_data)

  user_data[order(user_data$user_id, user_data$task_number), ]
}

