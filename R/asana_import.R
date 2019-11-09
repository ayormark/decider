######### Info ################################################################
# Adam Yormark
# asana_import function for Decider

######### Packages ############################################################
# library(tidyverse)
# library(rje) # quickSort()
# library(stringr)
# library(shiny)
# library(crayon)
# library(httr)
# library(jsonlite)
# library(qdapRegex)
# library(asana) # Development build, remotes::install_github("datacamp/asana")
# library(lubridate)
# library(RCurl)
# library(rvest)
# library(rebus)


######### Asana Import ########################################################

asana_import <- function(
# see https://github.com/datacamp/asana for information on these inputs
  ASANA_ACCESS_TOKEN = Sys.getenv("ASANA_ACCESS_TOKEN"),
  project_gid = Sys.getenv("ASANA_MYTASKS_PROJECT_ID")) {

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
  output <- list(todo, prerated_todo)
  return(output)
}

