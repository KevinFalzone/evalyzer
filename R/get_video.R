#' Get the videos of each user for each task.
#'
#' Get the videos of all participants for each task.
#'
#' @importFrom dplyr bind_rows
#' @importFrom rvest html_attr
#' @importFrom rvest html_children
#' @importFrom rvest html_elements
#' @importFrom rvest html_text
#' @importFrom rvest session_jump_to
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace

#' @param session The session is available by the \code{\link{auth}} function.
#'   Please make sure you have used this function beforehand.
#' @param id The id of a project. The id lies in url like
#'   \url{https://app.evalyzer.com/evalyzer/analyzes/participation/<id>}
#' @param path The path to store videos.
#' @return The user videos.

#' @export

#' @examples
#' session <- auth("my-username", "my-password")
#' get_video(session, "1234", "pathtostorefiles/")
get_video <- function(session, id, path) {
  if (missing(session)) {
    stop_if_missing("session")
  }

  if (missing(id)) {
    stop_if_missing("id")
  }

  if (!(is.session(session))) {
    stop_if_bad_type("rvest session", "session")
  }

  if (!(dir.exists(path))) {
    stop("The path where the videos should be stored does not exist.")
  }

  webpage <- retrieve_user_list_webpage(session, id)

  user_profile_links <- webpage |>
    extract_user_profile_links()

  for (x in user_profile_links) {
    url <- session_jump_to(session, paste0("https://app.evalyzer.com", x))

    webpage <- read_html(url)

    user_task_links <- webpage |>
      extract_user_task_links()


    for (i in user_task_links) {
      user_id <- extract_user_id_from_url(x)

      url <- session_jump_to(session, i)
      webpage <- read_html(url)

      task_number <- webpage |>
        extract_task_number()

      video_url <- webpage |>
        extract_video_url()


      url <- session_jump_to(session, video_url)

      writeBin(
        url$response$content,
        paste0(
          path,
          "id-", id,
          "-user-", user_id, 
          "-task-", task_number, 
          ".mp4"
        )
      )
    }
  }
}
