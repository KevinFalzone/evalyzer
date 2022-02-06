#' @title Retrieve the user list webpage
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param id The id of a project. The id lies in url like
#'   \url{https://app.evalyzer.com/evalyzer/analyzes/participation/<id>}
#'
#' @return The HTML webpage.
#'
#' @importFrom rvest read_html
#' @importFrom rvest session_jump_to
#'
#' @noRd
retrieve_user_list_webpage <- function(session, id) {
  url <-
    rvest::session_jump_to(
      session,
      paste0("https://app.evalyzer.com/evalyzer/analyzes/participants/", id)
    )

  rvest::read_html(url)
}

#' @title Retrieve user profile webpage
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param path The path of the profile webpage.
#'
#' @return The HTML webpage.
#'
#' @importFrom rvest read_html
#' @importFrom rvest session_jump_to
#'
#' @noRd
retrieve_user_profile_webpage <- function(session, path) {
  url <-
    rvest::session_jump_to(
      session,
      paste0("https://app.evalyzer.com", path)
  )

  rvest::read_html(url)
}

#' @title Retrieve the user's task webpages
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param path The path of the user's task webpage.
#'
#' @return The HTML webpage.
#'
#' @importFrom rvest read_html
#' @importFrom rvest session_jump_to
#'
#' @noRd
retrieve_user_task_webpage <- function(session, path) {
  url <-
    rvest::session_jump_to(
    session,
    paste0("https://app.evalyzer.com", path)
  )

  rvest::read_html(url)
}


#' @title Retrieve the pre-test question webpage
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param question_id The id of the question.
#' @param user_id The user id.
#'
#' @return The HTML webpage.
#'
#' @importFrom rvest read_html
#' @importFrom rvest session_jump_to
#'
#' @noRd
retrieve_pre_test_question_webpage <- function(session, question_id, user_id) {
  url <-
    rvest::session_jump_to(
    session,
    paste0("https://app.evalyzer.com/analyzes/getquestionanswers/",
           question_id,
           "/",
           user_id)
  )

  rvest::read_html(url)
}

#' @title Retrieve the post-task question webpage
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param question_id The id of the question.
#' @param user_id The user id.
#'
#' @return The HTML webpage.
#'
#' @importFrom rvest read_html
#' @importFrom rvest session_jump_to
#'
#' @noRd
retrieve_post_task_question_webpage <- function(session, question_id, user_id) {
  url <-
    rvest::session_jump_to(
    session,
    paste0("https://app.evalyzer.com/analyzes/getquestionanswerstask/",
           question_id,
           "/",
           user_id)
  )

  rvest::read_html(url)
}

#' @title Retrieve the post-task question webpage
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param question_id The id of the question.
#' @param user_id The user id.
#'
#' @return The HTML webpage.
#'
#' @importFrom rvest read_html
#' @importFrom rvest session_jump_to
#'
#' @noRd
retrieve_post_test_question_webpage <- function(session, question_id, user_id) {
  url <-
    rvest::session_jump_to(
    session,
    paste0("https://app.evalyzer.com/analyzes/getquestionanswers/",
           question_id,
           "/",
           user_id)
  )

  rvest::read_html(url)
}
