######### Info ################################################################
# Adam Yormark
# build_sections function for Decider

######### Documentation #######################################################

#' Gets tasks from a project in Asana
#' @param sections A vector of new section titles to be added to the
#' project, in order. If a section already exists it will not create duplicates
#' @param project_gid The gid identifier for a specific project in Asana,
#' @export
#' @examples
#' asana_import(
#' ASANA_ACCESS_TOKEN = "add_your_34_chr_asana_access_token",
#' project_gid = "123456789101112")

######### Function ########################################################

# library(asana)
# library(tidyverse)
# library(dplyr)

build_sections <- function(sections, project_gid = Sys.getenv("ASANA_MYTASKS_PROJECT_ID")) {

    current_sections <-
      asn_sections_find_by_project(project = project_gid) %>%
      filter(name != "(no section)")

    sections <- sections %>% enframe(name = NULL) %>% rename(name = value)

    new_sections <- full_join(sections, current_sections) %>% filter(is.na(gid))

    for (section in new_sections) {

      asn_sections_create_in_project(project = project_gid, name = section)

    }

  }

project_gid <- "1148823248153567"
sections <- c("Do", "Delegate", "Schedule", "Ya Ya", "okayokay", "Woodledoodleoodle")

build_sections(sections, project_gid)

