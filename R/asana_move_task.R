######### Info ################################################################
# Adam Yormark
# asana_move_task function for Decider

######### Documentation #######################################################

#' Move A Task Within A Project Relative To Other Tasks
#' @param task_to_move The gid of the task that you want to move
#' @param insert_before The gid of the task that you would like to use as reference for
#' task placement. Defaults to top of list
#' @param project The project gid
#' @param ASANA_ACCESS_TOKEN An Asana access token.
#' You can get your own personal Asana access token here:
#' https://asana.com/guide/help/api/api#gl-access-tokens
#' @export
#' @details Follow the instructions for the asana package to make usage easier
#' https://github.com/datacamp/asana/blob/master/README.md
#' @examples
#' asana_move_task() # example coming soon

######### Function ########################################################

# library(httr)

asana_move_task <- function (
  task_to_move,
  insert_before = NULL,
  project,
  ASANA_ACCESS_TOKEN = Sys.getenv("ASANA_ACCESS_TOKEN")) {

  # task_to_move <- "1148844364427354"
  #
  # insert_before <- "1148825278352627"
  #
  # project <- "1148823248153567"
  #
  # ASANA_ACCESS_TOKEN <- Sys.getenv("ASANA_ACCESS_TOKEN")

  endpoint1 <- "https://app.asana.com/api/1.0/tasks/"
  endpoint2 <- "/addProject"

  endpoint <- paste0(endpoint1, task_to_move, endpoint2)

  Authorization <- paste0("Bearer ", ASANA_ACCESS_TOKEN)

  POST(
    endpoint,
    body = list(insert_before = insert_before,
                project = project),
    add_headers(Authorization = Authorization)
  )

}
