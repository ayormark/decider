######### Info ################################################################
# Adam Yormark
# asana_import function for Decider

######### Documentation #######################################################

#' Gets tasks from a project in Asana
#' @param ASANA_ACCESS_TOKEN An Asana access token.
#' You can get your own personal Asana access token here:
#' https://asana.com/guide/help/api/api#gl-access-tokens
#' @param project_gid The gid identifier for a specific project in Asana,
#' found in the URL
#' @param board_column If importing from an Asana project in board view,
#' specify the column that you would like to import and sort
#' @param section If importing from an Asana project in list view,
#' specify a section that you would like to import and sort
#' @export
#' @details Follow the instructions for the asana package to make usage easier
#' https://github.com/datacamp/asana/blob/master/README.md
#' @examples
#' asana_import(
#' ASANA_ACCESS_TOKEN = "add_your_34_chr_asana_access_token",
#' project_gid = "123456789101112")

######### Function ########################################################

asana_import <- function(
# see https://github.com/datacamp/asana for information on these inputs
  ASANA_ACCESS_TOKEN = Sys.getenv("ASANA_ACCESS_TOKEN"),
  project_gid = Sys.getenv("ASANA_MYTASKS_PROJECT_ID"),
  board_column = NA, section = NA, shuffle = FALSE) {

  # ASANA_ACCESS_TOKEN <- Sys.getenv("ASANA_ACCESS_TOKEN")
  # project_gid <- Sys.getenv("ASANA_MYTASKS_PROJECT_ID")
  endpoint <- paste0("projects/", project_gid, "/tasks")

  # Functions for Asana Tasks:
  # https://github.com/datacamp/asana/blob/master/R/tasks.R

  # call asana API to get list of tasks and transform into tibble
  # response <- call_asana_api(endpoint, ASANA_ACCESS_TOKEN) %>% as_tibble()
  # response_data <- response$data %>% as_tibble

  # Get tibble of not-yet-completed mytasks
  todo <- asn_tasks_find_by_project(project_gid,
                                    completed_since = "now")

  # Error handling for if asana project has no tasks within it
  if (class(todo)[[1]] == "asana_api") {
    stop("project is empty, no tasks to rate")
  }

  # Dispatch a full GET request to Asana
  # asana::asn_get("/projects/932414416064709/tasks", completed_since="now")

  # WORKING_MYTASK_URL:
  # "https://app.asana.com/api/1.0/projects/932414416064709/tasks"
  # Pretty Version:
  # https://app.asana.com/api/1.0/projects/932414416064709/tasks/?opt_pretty

  # Example: get details for a specific task by id
  # asn_tasks_find_by_id("955209932596796")

  todo <- todo %>% rename(Task = name)

  # Get the expected path of the list of already-rated tasks
  prerated_todo_csv_path <- paste0(getwd(), "/todo.csv")

  # Merge in past ratings of tasks to avoid re-rating
  if (file.exists("prerated_todo_csv_path")) {
    prerated_todo <- read_csv(prerated_todo_csv_path) %>%
      mutate(gid = as.character(gid))

  } else {
    # If there are no past ratings, create an empty "prerated" tibble
    prerated_todo <- tibble(gid = NA, Task = NA) %>% filter(!is.na(gid)) %>%
      mutate(gid = as.character(gid))
  }

  if (shuffle == T) {
    todo <- todo %>% sample_n(nrow(todo))
  }

  output <- list(todo, prerated_todo)
  return(output)
}

