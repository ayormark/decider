######### Info ################################################################
# Adam Yormark
# asana_move_to_section function for Decider

######### Documentation #######################################################

#' Move A Task Within A Project Relative To Other Tasks
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
                                ASANA_ACCESS_TOKEN = Sys.getenv("ASANA_ACCESS_TOKEN")) {


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

  # Remove uneccesary list format
  section_gids <- section_gids$data %>%
    rename(section = name) %>%
    arrange(section) %>% filter()

  # Get gids of the sections that contain the text string only
  if (!is.na(section_str)) {
    section_gids %>% filter(str_detect(section, "Tier"))
  }

  # Get rid of unneeded column
  section_gids <- section_gids %>% select(-resource_type)

  return(section_gids)

}
