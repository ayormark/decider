######### Info ################################################################
# Adam Yormark
# asana_move_to_section function for Decider

######### Documentation #######################################################

#' Move A Task To a Specific Section Within A Project
#' @param task_to_move The gid of the task that you want to move
#' @param section The gid of the section that you would like to place a task in
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

asana_move_to_section <- function (
  task_to_move,
  section = NULL,
  project,
  ASANA_ACCESS_TOKEN = Sys.getenv("ASANA_ACCESS_TOKEN")) {

  # task_to_move <- "1148844364427354"
  #
  # section <- "1150999038969028"
  #
  # project <- "1148823248153567"
  #
  # ASANA_ACCESS_TOKEN <- Sys.getenv("ASANA_ACCESS_TOKEN")


  endpoint1 <- "https://app.asana.com/api/1.0/sections/"
  endpoint2 <- "/addTask"

  endpoint <- paste0(endpoint1, section, endpoint2)

  Authorization <- paste0("Bearer ", ASANA_ACCESS_TOKEN)

  POST(
    endpoint,
    body = list(task = task_to_move,
                project = project),
    add_headers(Authorization = Authorization)
  )

}
