test_that("Tasks are moved to the proper section", {

  # project <- "932414416064709" # My Tasks:
  project <- "1148823248153567" # Test Project
  # Get the details for the project
  project_details <- asn_projects_find_by_id(project = project)
  project <- project_details$content$data$gid
  project_name <-  project_details$content$data$name
  # Use the first task in the list
  task_details <- asn_tasks_find_by_project(project_id = project) %>% head(1)
  task_to_move <- task_details$gid
  task_to_move_name <- task_details$name
  # Use the first (hand-added) section in the list
  expected_section_details <-
    asana_section_gids(project = project, include_no_section = FALSE) %>% head(1)
  expected_section_gid <-  expected_section_details$gid
  expected_section_name <- expected_section_details$section
  ASANA_ACCESS_TOKEN <- Sys.getenv("ASANA_ACCESS_TOKEN")
  cat(green("Project:", project_name, "\n",
            "Moving '", task_to_move_name,
            "' to section:",
            expected_section_name))

  asana_move_to_section(task_to_move, expected_section_gid, project)

  actual_section_details <- asn_tasks_find_by_id(task_to_move)
  actual_section_name <-
    actual_section_details$content$data$memberships$section$name
  actual_section_gid <-
    actual_section_details$content$data$memberships$section$gid

  expect_equal(actual_section_name, expected_section_name)
  expect_equal(actual_section_gid, expected_section_gid)
})
