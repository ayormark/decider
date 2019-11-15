######### Info ################################################################
# Adam Yormark
# place_task function for Decider

######### Documentation #######################################################

#' Move tasks to the proper section within a project
#' @param task a one-row tibble of a task to be placed, contains Task and gid
#' @param project_gid The gid identifier for a specific project in Asana
#' @param section The section to move the task to
#'
#' @export
#' @examples
#' project_gid <- "1234567891011121"
#' sections <- c("Do", "Delegate", "Schedule")
#' build_sections(sections, project_gid)
#'
#' @import tidyverse
#'
#' @remotes asana

######### Function ########################################################

# library(asana)
# library(tidyverse)

place_task <-
  function(task, project_gid, section) {

    # Get list of sections that currently exist in the project
    current_sections <-
      asn_sections_find_by_project(project = project_gid) %>%
      filter(name != "(no section)")

    if (!(section %in% current_sections$name)) {
      stop(paste0(section,
                  " is not a section within this project. ",
                  "Consider creating this section first."))}



    # Leftover from other function - to be revised

    # # Convert input sections into a tibble for easier manipulation
    # section <- section %>% enframe(name = NULL) %>% rename(name = value)
    #
    # # Filter out tasks that already exist in the project
    # new_sections <- full_join(sections, current_sections) %>% filter(is.na(gid))
    #
    # # Create the sections in the project
    # for (section in new_sections$name) {
    #   asn_sections_create_in_project(project = project_gid, name = section)
    # }
  }


task <- todo %>% head(1)







