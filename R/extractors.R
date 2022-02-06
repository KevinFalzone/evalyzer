#' @title Extract a table of user information
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A `data.frame` of user information is returned.
#'
#' @importFrom rvest html_element
#' @importFrom rvest html_table
#'
#' @noRd
extract_user_table <- function(webpage) {
  message("The table of user information is being extracted...")

  webpage |>
    rvest::html_element("table[id='participantstable']") |>
    rvest::html_table()
}

#' @title Extract links of user profiles
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A vector of user profile links is returned.
#'
#' @importFrom rvest html_attr
#' @importFrom rvest html_elements
#' @importFrom stringr str_subset
#'
#' @noRd
extract_user_profile_links <- function(webpage) {
  message("The URL of each user profile is being extracted...")

  webpage |>
    html_elements("a") |>
    html_attr("href") |>
    str_subset("fiche") |>
    print()
}

#' @title Extract user id from url
#'
#' @param url The URL in which you extract the user id.
#'
#' @return A character string of the user id is returned.
#'
#' @importFrom stringr str_extract
#'
#' @noRd
extract_user_id_from_url <- function(url) {
  message("The user id from url is being extracted...")

  url |>
    str_extract("[[:digit:]]+$") |>
    as.integer() |>
    print()
}

#' @title Extract user task links of user profiles
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A vector of user task links is returned.
#'
#' @importFrom rvest html_attr
#' @importFrom rvest html_elements
#' @importFrom stringr str_extract
#' @importFrom stringr str_subset
#'
#' @noRd
extract_user_task_links <- function(webpage) {
  message("The URL of each user's tasks is being extracted...")

  webpage |>
    html_elements("a") |>
    html_attr("href") |>
    str_subset("fiche") |>
    str_extract("/evalyzer/analyzes/fiche/[0-9/]*") |>
    unique() |>
    unlist() |>
    print()
}

#' @title Extract datetime
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A character string of the datetime is returned.
#'
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#'
#' @noRd
extract_datetime <- function(webpage) {
  message("The datetime is being extracted...")

  webpage |>
    html_text() |>
    str_extract("[[:digit:]]*/[[:digit:]]*/[[:digit:]]* [[:digit:]]*:[[:digit:]]*") |>
    as.character() |>
    print()
}

#' @title Extract task number
#'
#' @param webpage A specific HTML webpage.
#'
#' @return task number is returned.
#'
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#'
#' @noRd
extract_task_number <- function(webpage) {
  message("The task number is being extracted...")

  webpage |>
    html_text() |>
    str_extract("Tâche [[:digit:]]+ :") |>
    str_extract("[[:digit:]]+") |>
    as.integer() |>
    print()
}

#' @title Extract screen resolution information
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A character string of the screen resolution information is returned.
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#'
#' @noRd
extract_screen_resolution_information <- function(webpage) {
  message("The screen resolution information is being extracted...")

  webpage |>
    html_text() |>
    str_extract("[[:digit:]]+x[[:digit:]]+") |>
    as.character() |>
    print()
}

#' @title Extract browser information
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A character string of the browser information is returned.
#'
#' @importFrom rvest html_attr
#' @importFrom rvest html_elements
#' @importFrom stringr str_extract
#' @importFrom stringr str_subset
#'
#' @noRd
extract_browser_information <- function(webpage) {
  message("The browser information is being extracted...")

  webpage |>
    html_elements("i") |>
    html_attr("class") |>
    str_subset("firefox|chrome|edge") |>
    str_extract("firefox|chrome|edge") |>
    as.character() |>
    print()
}

#' @title Extract operating system information
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A character string of the operating system information is returned.
#'
#' @importFrom rvest html_attr
#' @importFrom rvest html_elements
#' @importFrom stringr str_extract
#' @importFrom stringr str_subset
#'
#' @noRd
extract_operating_system_information <- function(webpage) {
  message("The operating system information is being extracted...")

  webpage |>
    html_elements("i") |>
    html_attr("class") |>
    str_subset("windows|apple|linux") |>
    str_extract("windows|apple|linux") |>
    as.character() |>
    print()
}

#' @title Extract behaviors
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A `data.frame` of user behaviors is returned.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom stringr fixed
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#'
#' @noRd
extract_behaviors <- function(webpage) {
  message("The user behaviors are being extracted...")

  webpage |>
    html_text() |>
    str_extract("//<![^>]*") |>
    str_remove(fixed("//<![CDATA[\nwindow.app = {\"jsonevents\":\"{\\\"events\\\":")) |>
    str_remove(fixed("}\"};\n//]]")) |>
    str_remove_all("\\\\") |>
    fromJSON() |>
    data.frame()
}

#' @title Extract task status
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A character string of task status is returned.
#'
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#'
#' @noRd
extract_task_status <- function(webpage) {
  message("The task status is being extracted...")

  webpage |>
    html_text() |>
    str_extract(
      "Vraie réussite|Fausse réussite|Vrai échec|Faux échec|Statut invalide"
    ) |>
    str_replace("Vraie réussite", "true success") |>
    str_replace("Fausse réussite", "false success") |>
    str_replace("Vrai échec", "true failure") |>
    str_replace("Faux échec", "false failure") |>
    str_replace("Statut invalide", "invalid status") |>
    print()
}

#' @title Extract question information (pre-test questions)
#'
#' @param webpage A specific HTML webpage.
#'
#' @return a table with `data-questionid` `data-type` columns
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom rvest html_attrs
#' @importFrom rvest html_elements
#'
#' @noRd
extract_pre_test_question_information <- function(webpage) {
  message("The question information (pre-test questions) is being extracted...")

  webpage |>
    html_elements("div#home") |>
    html_elements("div.tab-pane") |>
    html_attrs() |>
    bind_rows() |>
    select("data-questionid", "data-type") |>
    mutate(question_stage = "pre-test") |>
    print()
}

#' @title Extract question information (post-task questions)
#'
#' @param webpage A specific HTML webpage.
#'
#' @return a table with `data-questionid` `data-type` columns
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom rvest html_attrs
#' @importFrom rvest html_elements
#'
#' @noRd
extract_post_task_question_information <- function(webpage) {
  message("The question information (post-task questions) is being extracted...")

  webpage |>
    html_elements("div#profile") |>
    html_elements("div.tab-pane") |>
    html_attrs() |>
    bind_rows() |>
    select("data-questionid", "data-type") |>
    mutate(question_stage = "post-task") |>
    print()
}

#' @title Extract question information (post-test questions)
#'
#' @param webpage A specific HTML webpage.
#'
#' @return a table with `data-questionid` `data-type` columns
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom rvest html_attrs
#' @importFrom rvest html_elements
#'
#' @noRd
extract_post_test_question_information <- function(webpage) {
  message("The question information (post-test questions) is being extracted...")

  webpage |>
    html_elements("div#messages") |>
    html_elements("div.tab-pane") |>
    html_attrs() |>
    bind_rows() |>
    select("data-questionid", "data-type") |>
    mutate(question_stage = "post-test") |>
    print()
}

#' @title Extract opened question
#'
#' @param webpage A specific HTML webpage.
#'
#' @return An answer is returned.
#'
#' @importFrom rvest html_element
#' @importFrom rvest html_text
#'
#' @noRd
extract_opened_question <- function(webpage) {
  message("The opened question information is being extracted...")

  tryCatch({
    webpage |>
      html_element("p[class='clearfix']") |>
      html_text(trim = TRUE) |>
      print()
  },
  error = function(e) {
    warning("Impossible to extract the answer from the opened question. Value replaced by NA")

    NA
  })
}

#' @title Extract single choice question
#'
#' @param webpage A specific HTML webpage.
#'
#' @return An answer is returned.
#'
#' @importFrom rvest html_elements
#' @importFrom rvest html_text
#'
#' @noRd
extract_single_choice_question <- function(webpage) {
  message("The single choice question information is being extracted...")

  tryCatch({
    answer <- webpage |>
      html_element('div[style$="rgba(51, 173, 13,0.5);"] ~ label') |>
      html_text()

    if(!is.na(answer)) {
      print(answer)
      return(answer)
    }

    else if (is.na(answer)) {
      answer <- webpage |>
        html_elements('p')

        answer <- html_text(answer[2])

        print(paste0("OTHER: ", answer))

        return(answer)
    } else {
      print(NA)
      answer <- NA
    }


  },
  error = function(e){
      warning("Impossible to extract the answer from the single choice question. Value replaced by NA.")

      NA
  })
}

#' @title Extract multiple choice question
#'
#' @param webpage A specific HTML webpage.
#'
#' @return An answer is returned.
#'
#' @importFrom rvest html_element
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove
#' @importFrom stringr str_subset
#'
#' @noRd
extract_multiple_choice_question <- function(webpage) {
  message("The multiple choice question information is being extracted....")

  tryCatch({
    answer <- webpage |>
      html_elements('div:not(div[style="width:0%;background-color:rgba(255, 0, 0,0.5);"]) + div + label') |>
      html_text(trim = TRUE) |>
      paste0(collapse = "-")


    if(!(length(answer) == 0)) {
      print(answer)
      return(answer)
    }

    else if (length(answer) == 0) {
      answer <- webpage |>
        html_elements('p')

        answer <- html_text(answer[2], trim = TRUE)

        print(paste0("OTHER: ", answer))

        return(answer)
    } else {
      print(NA)
      answer <- NA
    }

  },
  error = function(e){
    warning("Impossible to extract the answer from the multiple choice question. Value replaced by NA.")

    NA
  })
}

#' @title Extract likert scale question
#'
#' @param webpage A specific HTML webpage.
#'
#' @return An answer is returned.
#'
#' @importFrom rvest html_element
#' @importFrom rvest html_text
#'
#' @noRd
extract_likert_scale_question <- function(webpage) {
  message("The likert scale question information is being extracted...")

  tryCatch({
    webpage |>
      html_element('strong[class="large-size"]') |>
      html_text() |>
      print()
  },
  error = function(e){
    warning("Impossible to extract the answer from the likert scale question. Value replaced by NA.")

    NA
  })

}

#' @title Extract ranking question
#'
#' @param webpage A specific HTML webpage.
#'
#' @return An answer is returned.
#'
#' @importFrom rvest html_elements
#' @importFrom rvest html_text
#'
#' @noRd
extract_ranking_question <- function(webpage) {
  message("The ranking question information is being extracted...")

  tryCatch({
    webpage |>
      html_elements('span[class="stat-inner-rank"]') |>
      html_text() |>
      paste0(collapse = "-") |>
      print()
  },
  error = function(e){
    warning("Impossible to extract the answer from the ranking question. Value replaced by NA.")

    NA
  })
}

#' @title Extract grid question
#'
#' @param webpage A specific HTML webpage.
#'
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom rvest html_element
#' @importFrom rvest html_table
#'
#' @noRd
extract_grid_question <- function(webpage) {
  message("The grid question information is being extracted...")

  tryCatch({
    answer <- webpage |>
    html_element("table") |>
    html_table() |>
    select(-X1)

    table_into_rows <- asplit(answer, 1)

    answers <-
      sapply(table_into_rows, function(x) {
        if(sum(x) == 0) {
          return(NA)
        }
        for(i in seq_along(x)) {
          if(x[i] == 1) {
            return(i)
          }
        }
      })

    paste0(answers, collapse = "-") |>
    print()
  },
  error = function(e){
    warning("Impossible to extract the answer from the grid question. Value replaced by NA.")
    NA
  })

}

#' @title Extract sus question
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A character string with the SUS answer is returned.
#'
#' @importFrom dplyr select
#' @importFrom rvest html_element
#' @importFrom rvest html_table
#'
#' @noRd
extract_sus_question <- function(webpage) {
    message("The SUS question information is being extracted...")

    tryCatch({

      answer <- webpage |>
        html_element("table") |>
        html_table(header = FALSE) |>
        select(-X1)

      table_into_rows <- asplit(answer, 1)

      sus <-
        sapply(table_into_rows, function(x) {
          if(sum(x) == 0) {
            return(NA)
          }
          for(i in seq_along(x)) {
            if(x[i] == 1) {
              return(i)
            }
          }
        })

      paste0(sus, collapse = "-") |>
      print()
    },
    error = function(e) {
      warning("Impossible to extract the answer from the SUS question. Value replaced by NA.")

      NA
    })
}

#' @title Extract nps question
#'
#' @param webpage A specific HTML webpage.
#'
#' @return A character string with the NPS answer is returned.
#'
#' @noRd
extract_nps_question <- function(webpage) {
    message("The NPS question information is being extracted...")

    tryCatch({
      webpage |>
        html_element('div[title="100.0% des réponses"] + label') |>
        html_attr("data-index") |>
        print()
    },
    error = function(e) {
      warning("Impossible to extract the answer from the NPS question. Value replaced by NA.")

      NA
    })
}

#' @title Extract video url
#'
#' @param webpage A specific HTML webpage.
#'
#' @return video url is returned.
#'
#' @noRd
extract_video_url <- function(webpage) {
  message("The video url is being extracted...")

  webpage |>
    html_elements("source") |>
    html_attr("src") |>
    str_subset(".mp4")
}
