######### Info ################################################################
# Adam Yormark
# build_sections function for Decider

######### Documentation #######################################################

#' Create new sections in a project in Asana
#' @param sections A vector of new section titles to be added to the
#' project, in order. If a section already exists it will not be duplicated.
#' @param project_gid The gid identifier for a specific project in Asana,
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
# library(dplyr)

build_sections <-
  function(sections, project_gid) {

    # Get list of sections that currently exist in the project
    current_sections <-
      asn_sections_find_by_project(project = project_gid)

    # If there are no sections yet within a project, the returned
    # class is "asana_api" and requires special handling
    if (class(current_sections) != "asana_api") {
      current_sections <- current_sections %>%
        filter(name != "(no section)")
    }

    # Convert input sections into a tibble for easier manipulation
    sections <- sections %>% enframe(name = NULL) %>% rename(name = value)

    if (class(current_sections) != "asana_api") {
      # Filter out section names that already exist in the project
      new_sections <- full_join(sections, current_sections) %>% filter(is.na(gid))
    } else {
      new_sections <- sections
    }

    # If there are new sections to add, create the sections in the project
    if (nrow(new_sections) != 0) {
      for (section in new_sections$name) {
        asn_sections_create_in_project(project = project_gid, name = section)
      }
    }
    }


