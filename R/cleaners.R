#' Clean the table of user information
#'
#' @param table A table with specific data.
#'
#' @return A cleaned `data.frame` table.
#'
#' @importFrom stringr str_extract
#'
#' @noRd
clean_user_table <- function(table) {
  message("The table of information is being cleaned...")

  # Renaming columns
  names(table)[names(table) == "ID"] <- "user_id"
  names(table)[names(table) == "Date"] <- "datetime"
  names(table)[names(table) == "Durée totale"] <- "total_duration"
  names(table) <- gsub("Tâche ", "task_", names(table))

  # Extracting the user ids
  table$user_id <- stringr::str_extract(table$user_id, "[[:digit:]]+")

  table[] <-
    lapply(
      table,
      function(col) {
        col <- gsub("Vraie réussite", "true success", col)
        col <- gsub("Fausse réussite", "false success", col)
        col <- gsub("Vrai échec", "true failure", col)
        col <- gsub("Invalide", "invalid status", col)
      }
    )

  # Handle column types
  table[] <- as.character(table[])
  table$user_id <- as.factor(table$user_id)
  table$datetime <- as.POSIXct(table$datetime, format = "%d/%m/%Y %H:%M")

  table <- as.data.frame(table)
}

#' Clean the user behavior table
#'
#' @param table A `data.frame` or a `tibble` table.
#'
#' @return A cleaned `data.frame` table
#'
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom tidyselect any_of
#'
#' @noRd
clean_behaviors <- function(table) {
  table |>
    mutate(
      across(any_of("id"), as.numeric),
      across(any_of("timestamp"), as.numeric),
      across(any_of("url_id"), as.numeric),
      across(any_of("results_task_id"), as.numeric),
      across(any_of("event_id"), as.numeric),
      across(any_of("url"), as.character),
      across(any_of("type"), as.character),
      across(any_of("x"), as.numeric),
      across(any_of("y"), as.numeric),
      across(any_of("button"), as.numeric),
      across(any_of("nodename"), as.character),
      across(any_of("id_node"), as.character),
      across(any_of("x_node"), as.numeric),
      across(any_of("y_node"), as.numeric),
      across(any_of("dim"), as.numeric),
      across(any_of("id_scroll"), as.numeric),
      across(any_of("axe_x"), as.numeric),
      across(any_of("axe_y"), as.numeric),
      across(any_of("distance"), as.numeric),
    )
}
