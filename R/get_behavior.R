#' @title Get behaviors of all users
#'
#' @description Get behaviors of all users for each task.
#' The behaviors mainly include:
#'   - clicks,
#'   - scrolls,
#'   - page inputs
#'   - consulted pages.
#'
#' @param session A rvest session. The rvest session is created by the
#' @param id The id of a project. The id lies in url like
#'        \code{\link{auth}} function.
#'
#' @return A `data.frame` with user timestamps, clicks, scrolls, page inputs,
#'         consulted pages.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tidyselect everything
#'
#' @export
#'
#' @examples
#' session <- auth("my-username", "my-password")
#' user_behavior <- get_behavior(session, "project-id")
get_behavior <- function(session, id) {
  if (missing(session)) {
    stop_if_missing("session")
  }

  if (missing(id)) {
    stop_if_missing("id")
  }

  if (!(is.session(session))) {
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

      behaviors <- webpage |>
        extract_behaviors() |>
        clean_behaviors()

      behaviors |>
        mutate(
          user_id = user_id,
          datetime = datetime,
          task_number = task_number,
          .before = everything()
        )
    },
    user_task_paths$user_id,
    user_task_paths$user_task_paths,
    SIMPLIFY = FALSE
  )

  user_data <- do.call("bind_rows", user_data)

  filter(user_data, !(type == "Scroll" & axe_x == "0" & axe_y == "0"))
}
