#' Get the answers of all users
#'
#' @description Get the pre-test, post-task and post-test answers of all users.
#'
#' @param session A rvest session. The rvest session is created by the
#'        \code{\link{auth}} function.
#' @param id The id of a project. The id lies in url like
#'   \url{https://app.evalyzer.com/evalyzer/analyzes/participants/<id>}
#'
#' @return A `data.frame`  with the pre-test, post-task and post-test answers.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
#' @examples
#' session <- auth("my-username", "my-password")
#' participant_answers <- get_answer(session, "1234")
get_answer <- function(session, id) {
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
    user_id <- extract_user_id_from_url(path)

    webpage <- retrieve_user_profile_webpage(session, path)

    user_task_paths <- extract_user_task_links(webpage)

    data.frame(
      user_id = user_id,
      user_task_paths = user_task_paths
    )
  })


  user_task_paths <- do.call("rbind", user_task_paths)


  user_question_information <- mapply(
    function(user_id, user_task_path) {
      webpage <- retrieve_user_task_webpage(session, user_task_path)

      pre_test_question_information <-
        extract_pre_test_question_information(webpage)

      post_task_question_information <-
        extract_post_task_question_information(webpage)

      post_test_question_information <-
        extract_post_test_question_information(webpage)

      question_information <-
        do.call(
          "bind_rows",
          list(
            pre_test_question_information,
            post_task_question_information,
            post_test_question_information
          )
        )

      cbind(user_id, question_information)

  },
  user_task_paths$user_id,
  user_task_paths$user_task_paths,
  SIMPLIFY = FALSE
  )

  user_question_information <- bind_rows(user_question_information)
  user_question_information <- unique(user_question_information)
  user_question_information <-
    arrange(
      user_question_information,
      factor(
        question_stage , 
        levels = c("pre-test", "post-task", "post-test")
      )
    )

  answers <-
    mapply(
      function(user_id, 
               question_stage, 
               question_id, 
               question_type) {

        if (question_stage == "pre-test") {

          webpage <- 
            retrieve_pre_test_question_webpage(
              session,
              question_id,
              user_id
          )


          answer <- 
            switch(
              question_type,
              "ouverte" = extract_opened_question(webpage),
              "reponseunique" = extract_single_choice_question(webpage),
              "reponsesmultiples" = extract_multiple_choice_question(webpage),
              "echelle" = extract_likert_scale_question(webpage),
              "classement" = extract_ranking_question(webpage),
              "grille" = extract_grid_question(webpage),
              "sus" = extract_sus_question(webpage),
              "nps" = extract_nps_question(webpage),
              NA)

          return(
            data.frame(
              user_id = user_id,
              question_id = question_id,
              answer = answer
            )
          )

        }

        else if (question_stage == "post-task") {
          webpage <- 
            retrieve_post_task_question_webpage(
              session, 
              question_id, 
              user_id
            )

          answer <- 
            switch(
              question_type,
              "ouverte" = extract_opened_question(webpage),
              "reponseunique" = extract_single_choice_question(webpage),
              "reponsesmultiples" = extract_multiple_choice_question(webpage),
              "echelle" = extract_likert_scale_question(webpage),
              "classement" = extract_ranking_question(webpage),
              "grille" = extract_grid_question(webpage),
              "sus" = extract_sus_question(webpage),
              "nps" = extract_nps_question(webpage),
              NA)

          return(
            data.frame(
              user_id = user_id,
              question_id = question_id,
              answer = answer
            )
          )
        }

        else if (question_stage == "post-test") {
          webpage <- 
            retrieve_post_test_question_webpage(
              session, 
              question_id,
              user_id
          )

          answer <- 
            switch(
              question_type,
              "ouverte" = extract_opened_question(webpage),
              "reponseunique" = extract_single_choice_question(webpage),
              "reponsesmultiples" = extract_multiple_choice_question(webpage),
              "echelle" = extract_likert_scale_question(webpage),
              "classement" = extract_ranking_question(webpage),
              "grille" = extract_grid_question(webpage),
              "sus" = extract_sus_question(webpage),
              "nps" = extract_nps_question(webpage),
              NA)

          return(
            data.frame(
              user_id = user_id,
              question_id = question_id,
              answer = answer
            )
          )
        }
      }, 
      user_question_information$user_id, 
      user_question_information$question_stage,
      user_question_information$`data-questionid`, 
      user_question_information$`data-type`,
      SIMPLIFY = FALSE
    )

  answers <- bind_rows(answers)

  pivot_wider(
    answers,
    names_from = question_id, 
    names_glue = "question_{question_id}",
    values_from = answer
  )

}
