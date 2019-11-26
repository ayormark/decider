######### Info ################################################################
# Adam Yormark
# asana_move_to_section function for Decider

######### Documentation #######################################################

#' Get A Tibble Of All Sections Within A Project And Their gids
#' @param project The project gid
#' @param section_str A character string contained within the section name(s)
#' you'd like to return.
#' @param ASANA_ACCESS_TOKEN An Asana access token.
#' You can get your own personal Asana access token here:
#' https://asana.com/guide/help/api/api#gl-access-tokens
#' @export
#' @details Follow the instructions for the asana package to make usage easier
#' https://github.com/datacamp/asana/blob/master/README.md
#' @examples
#' asana_section_gids() # example coming soon

######### Function ########################################################

asana_section_gids <- function (project,
                                section_str = NA,
                                include_no_section = FALSE,
                                ASANA_ACCESS_TOKEN = Sys.getenv("ASANA_ACCESS_TOKEN")) {

  # Test Project:
  # project <- "1148823248153567"
  # section_str <- "Tier"
  # ASANA_ACCESS_TOKEN <- Sys.getenv("ASANA_ACCESS_TOKEN")
  # task_to_move <- "1148844364427354" # "useless thing" task
  # section <- "1151006716370421" # Tier 3 in Test Project


  # Get gid for each section within the project
  sections_gid_endpoint <- paste0("https://app.asana.com/api/1.0/projects/",
                                  project,
                                  "/sections")

  Authorization <- paste0("Bearer ", ASANA_ACCESS_TOKEN)

  section_gids <- GET(
    sections_gid_endpoint,
    body = list(project = project),
    add_headers(Authorization = Authorization)
  ) %>%
    content("text") %>%
    fromJSON() %>%
    as_tibble()

  # Remove unneccesary list format
  section_gids <- section_gids$data %>%
    rename(section = name) %>%
    arrange(section) %>% filter()

  no_section_gid <- section_gids %>%
    filter(str_detect(section, "(no section)"))

  # Get gids of only the sections that contain the text string
  if (!is.na(section_str)) {
    section_gids <- section_gids %>%
      filter(str_detect(section, section_str))
  }

  if (include_no_section) {
    section_gids <- bind_rows(section_gids, no_section_gid) %>% unique()
  } else if (!include_no_section) {
    section_gids <- section_gids %>% filter(section != "(no section)")
  }

  # Get rid of unneeded resource_type column
  section_gids <- section_gids %>% select(-resource_type)

  return(section_gids)

}
