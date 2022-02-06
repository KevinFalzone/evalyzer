#' @title Get performance information of all users
#'
#' @description Get performance information of all users for each task.
#' Information include :
#'   * task duration
#'   * task status
#'   * task duration (ms)
#'   * number of clicks.
#'   * number of scrolls,
#'   * number of page inputs
#'   * number of consulted pages,
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param id The id of a project. The id lies in url like
#'   \url{https://app.evalyzer.com/evalyzer/analyzes/participants/<id>}
#'
#' @return A `data.frame`
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom tidyselect everything
#'
#' @export
#'
#' @examples
#' session <- auth("my-username", "my-password")
#' user_performance <- get_performance(session, "project-id")
get_performance <- function(session, id) {
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

      task_number <- extract_task_number(webpage)

      task_status <- webpage |>
        extract_task_status()

      behaviors <- webpage |>
        extract_behaviors()

      behaviors |>
        mutate(
          user_id = user_id,
          task_status = task_status,
          task_number = task_number,
          .before = everything()
        )
    },
    user_task_paths$user_id,
    user_task_paths$user_task_paths,
    SIMPLIFY = FALSE
  )

  user_data <- do.call("bind_rows", user_data)

  user_data <- filter(user_data, !(type == "Scroll" & axe_x == "0" & axe_y == "0"))

  counted_behaviors <- user_data |>
    select(user_id, task_number, type) |>
    group_by(user_id, task_number, type) |>
    summarize(n = n()) |>
    pivot_wider(
      names_from = c(type, task_number),
      names_glue = "task_{task_number}_nbr_{type}",
      values_from = n
    ) |>
    mutate(
      across(everything(), ~replace_na(.x, 0))
    ) |>
    rename_with(tolower)

  print(counted_behaviors)


  task_status <- user_data |>
    select(user_id, task_number, task_status) |>
    distinct() |>
    pivot_wider(
      names_from = task_number,
      names_glue = "task_{task_number}_status",
      values_from = task_status
    )

print(task_status)

  task_time <- user_data |>
    select(user_id, task_number, timestamp) |>
    group_by(user_id, task_number) |>
    summarize(task_time = max(as.integer(timestamp))) |>
    pivot_wider(
      names_from = task_number,
      names_glue = "task_{task_number}_time_ms",
      values_from = task_time
    )
print(task_time)

  inner_join(task_status, task_time, by = "user_id") |>
    inner_join(counted_behaviors, by = "user_id") |>
    as.data.frame()
}
