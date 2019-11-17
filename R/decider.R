######### Info ################################################################
# Adam Yormark
# Decider

# Decider dramatically reduces the amount of time and mental effort it takes
# to decide which tasks to do and in what order to do them
# This script is designed to work with Asana, but can be modified
# to work with any csv or list of tasks

# We accomplish this goal by combining together these established concepts:
# The Eisenhower Matrix
# https://www.eisenhower.me/eisenhower-matrix/
# Likert Scale
# https://www.simplypsychology.org/likert-scale.html
# Quick Sort
# https://www.youtube.com/watch?v=WaNLJf8xzC4
# Binary Search Algorithm
# https://en.wikipedia.org/wiki/Binary_search_algorithm

# After importing the to do list, each task is then rated from 1-5 on both
# Urgency and Importance. These make up 25 "bins"
# The order of completion of bins is determined by moving a diagonal line
# through a 5x5 grid of points, each with coordinates (x, y) synonymous with
# (i, u). As the line touches a point, that bin is performed.

# When tackling a bin, all tasks within that bin are sorted using Quicksort

#### Future Goals ####

# Automatically rearrange tasks in asana
# Account for whether or not the task boomerangs and how long it's loop is
# If the task is very quick, but the boomerang loop is long, then it should
  # be done sooner to "get the ball rolling"- have the task running on its own
# Automatically create Sections in asana list
  # Delegate, Do Now, Schedule
  # Bins
# Use a tool like mechanical turk to determine the actual order in which EUEI
  # bins should be completed
# Modifications need to be made to account for a todo item becoming
  # more urgent as time passes
# It may be safe to say that Importance remains constant, but Urgency changes
# If a task is a dependency of another higher-ranked task, then that task
  # takes on the ranking of its parent task
# The need to re-rate aspects of specific items should be eliminated
# Create a way for a single new item or small set of new items to be
  # efficiently inserted into an already-sorted list. bisection insert?
# Account for time it will take to complete task
# Account for Boomerang Y/N
# Account for "flight time" of boomerang
# When figuring out the relationship between prioritization and other metrics,
  # Use supervised machine learning (regression) to figure out how to
  # reprioritize
# Test out new metrics and measure how accurately they contribute to correct
  # Prioritization ordering, A/B test these questions until they're ideal
# Account for task size? Extra Large task, Large Task, Medium, etc.

######### Documentation #######################################################

#' Decide What To Do And When To Do It
#' @description Decider dramatically reduces the amount of time and mental
#' effort it takes
#' to decide which tasks to do and in what order to do them
#' Primarily focused on integrating with Asana, but can be modified to work
#' with any csv or list of tasks
#'
#' We accomplish this goal by combining together these established concepts:
#'
#' The Eisenhower Matrix:
#' https://www.eisenhower.me/eisenhower-matrix/
#'
#' Likert Scale:
#' https://www.simplypsychology.org/likert-scale.html
#'
#' QuickSort:
#' https://www.youtube.com/watch?v=WaNLJf8xzC4
#' @param input_type The method to import tasks into decider.
#' Can be "asana" or "csv"
#' @param asana_project_gid The global identifier for an asana project.
#' Can usually be found in the project URL.
#' @param run_shiny Choose whether to run the GUI for decider
#' @param csv_path The filepath to a csv with a list of tasks
#' @param csv_task_column_name The name of the column containing the task list
#' within a csv file
#' @param testing_task_num A number of tasks you'd like to sample from the
#' total list of tasks. For testing decider functionality.
#' @export
#' @details Follow the instructions for the asana package to make usage easier
#' https://github.com/datacamp/asana/blob/master/README.md
#' @examples
#' decider(
#' input_type = "asana",
#' asana_project_gid = "123456789101112,
#' asana_project_gid = Sys.getenv("ASANA_MYTASKS_PROJECT_ID"),
#' csv_task_column_name = "Task",
#' testing_task_num = 4)


#### Packages ####

#' @import tidyverse
#' @import rje
#' @import stringr
#' @import shiny
#' @import crayon
#' @import httr
#' @import qdapRegex
#' @import lubridate
#' @import asana

########## Decider Function ###################################################

decider <- function(input_type = "asana",
                    asana_project_gid = Sys.getenv("ASANA_MYTASKS_PROJECT_ID"),
                    run_shiny = FALSE,
                    csv_path,
                    csv_task_column_name = "Task",
                    testing_task_num = NA) {

  # setwd("~/Google Drive/Personal/R/decider")

  # Choose whether to import csv or connect with Asana API
  # input_type <- "asana"
  # input_type <- "csv"
  if (input_type == "csv") {
    csv_folder <- "/Users/adamyormark/Downloads/"
    csv_filename_base <- "asana2go_output_csv_basic"
  }

  # If testing, select how many tasks to rank
  # rm(testing_task_num)
  # testing_task_num <- 4

  ######### CSV Import ##########################################################

  if (input_type == "csv") {

    # Import csv file from path
    todo <- read_csv(csv_path)

    # rename selected task column
    todo <- rename(Task = csv_task_column_name)

  }

  ######### Asana Import ######################################################

  if (input_type == "asana") {

    source(paste0(getwd(), "/R/asana_import.R"))

    data <- asana_import(project_gid = asana_project_gid)

    todo <- data[[1]]
    prerated_todo <- data[[2]]

    rm(data)

    }

  ######### Functions ###########################################################

  #### eisenlikert function ####
  # The eisenlikert() function asks user to rank Urgency and Importance from 1-5
  # 5 is the most Urgent or most Important
  # Output is a tibble with two elements, Urgency and Importance, as integers
  source(paste0(getwd(), "/R/eiskenlikert.R"))

  #### compare function ####
  # A function to choose which of two options is more important
  # To be used within the quickSort() and bisection_insert() function
  # Output is either -1 or 1, similar output to greaterThan() from rje
  source(paste0(getwd(), "/R/compare.R"))

  #### wait_for_key function ####
# wait for a key to be pressed before proceeding
  source(paste0(getwd(), "/R/wait_for_key.R"))

  #### Use GUI with shiny ####
  source(paste0(getwd(), "/R/run_shiny.R"))

  ######### Shiny ###############################################################
  if (run_shiny == TRUE) {

    shiny(run_shiny)

    }
  ######### Pre-Rating Prompts ################################################

  # Tell user how many tasks they are about to rate
  asana_user_info <- asn_users_me()
  asana_first_last <- asana_user_info$content$data$name
  first_name <- strsplit(asana_first_last, " ")[[1]][1]
  cat(green(paste0("Welcome, ", first_name, "\n")))

  cat(green("You have", todo %>% nrow(), "tasks to rate", "\n"), sep = "")

  # If the testing number isn't NA, sample that many Tasks
  if (!is.na(testing_task_num)) {
    cat(red("Note: testing with", testing_task_num, "Tasks\n"))
  }

  wait_for_key("c")

  ######### Rate Each Task ######################################################

  # Column names
  ratings_columns <- c("Task", "Urgency", "Importance", "Date_Recorded")

  # Create empty dataframe with same number of columns
  ratings <- data.frame(matrix(nrow = 0, ncol = ratings_columns %>% length()))
  # Name the columns
  colnames(ratings) <- ratings_columns

  # Tasks to Test with
  # When testing, you may select a subset of tasks to reduce testing time
  print(testing_task_num)
  print(nrow(todo))

# If there is a testing number, sample that many tasks from todo list
  if (!is.na(testing_task_num)) {

    if (testing_task_num > nrow(todo)) {
      # Stop is testing number is larger than tasks available
      stop("testing number must be smaller than table size")
    }

    todo <- todo %>% sample_n(testing_task_num)
  }

  # Run the eisenlikert function on each task and add ratings to the dataframe
  for (task in todo$Task) {

    # If the task has not already been rated, proceed with rating it
    if (!(task %in% prerated_todo$Task)) {
      # Ask for ratings and store input
      rating <- eisenlikert(task)

      # Convert task string to dataframe
      task <- enframe(task, name = NULL)
      colnames(task) <-  "Task"

      # Combine into one dataframe
      task_scores <- bind_cols(task, rating)

      # Record Current Date
      task_scores <- task_scores %>% mutate(Date_Recorded = now())

      # Combine input from this loop into full rating list
      ratings <- bind_rows(ratings, task_scores) %>% as.tbl()

    }

  }

  # Merge ratings back into original list
  todo <- full_join(ratings, todo, by = "Task")

  # Add composite score for user to gain an intuitive sense of task value
  todo <- todo %>% mutate(Composite = 4 * Urgency * Importance)

  # Order by most recently rated
  todo <- todo %>% arrange(-as.numeric(Date_Recorded))

  ######### Save csv File of Rated Tasks ######################################

  # Create new todo.csv or save over the previous version
    write_csv(todo, paste0(getwd(), "/todo.csv"))

  ######### Post-Rating Prompts ###############################################

  cat(green("You have completed ranking Urgency and Importance \n\n"),
      sep = "")

  wait_for_key("c")

  ######### EUEI Abbreviations ################################################

  # Create four-letter EUEI abbreviation
  todo <- todo %>% mutate(EUEI = paste0(

    case_when(as.character(Urgency) == 5 ~ "EU",
              as.character(Urgency) == 4 ~ "VU",
              as.character(Urgency) == 3 ~ "MU",
              as.character(Urgency) == 2 ~ "SU",
              as.character(Urgency) == 1 ~ "NU",
              TRUE ~ as.character(Urgency)),

    case_when(as.character(Importance) == 5 ~ "EI",
              as.character(Importance) == 4 ~ "VI",
              as.character(Importance) == 3 ~ "MI",
              as.character(Importance) == 2 ~ "SI",
              as.character(Importance) == 1 ~ "NI",
              TRUE ~ as.character(Importance))
  ))

  # Create column with string for Urgency phrase
  todo <- todo %>% mutate(Urgency_str = case_when(
    as.character(Urgency) == 5 ~ "Extremely Urgent",
    as.character(Urgency) == 4 ~ "Very Urgent",
    as.character(Urgency) == 3 ~ "Moderately Urgent",
    as.character(Urgency) == 2 ~ "Somewhat Urgent",
    as.character(Urgency) == 1 ~ "Not Very Urgent",
    TRUE ~ as.character(Urgency)))

  # Create column with string for Importance phrase
  todo <- todo %>% mutate(Importance_str = case_when(
    as.character(Importance) == 5 ~ "Extremely Important",
    as.character(Importance) == 4 ~ "Very Important",
    as.character(Importance) == 3 ~ "Moderately Important",
    as.character(Importance) == 2 ~ "Somewhat Important",
    as.character(Importance) == 1 ~ "Not Very Important",
    TRUE ~ as.character(Importance)))

  ######### EUEI Bin Do-Order #################################################

  # The order of task sections to move through and actions to be taken

  # Determined by the chart below and a traveling set of diagonal lines
  # https://docs.google.com/drawings/d/1XWlp9sYhuP-iULGH5so0Q9qumT0z1by
  # hWzqo9fzRe7g/preview

  do_order <- list(
    "EUEI" = list("EUEI", "Delegate if Possible"),
    "EUVI" = list("EUVI", "Delegate if Possible"),
    "VUEI" = list("VUEI", "Delegate if Possible"),
    "EUMI" = list("EUMI", "Delegate if Possible"),
    "VUVI" = list("VUVI", "Delegate if Possible"),
    "EUSI" = list("EUSI", "Delegate if Possible"),
    "MUEI" = list("MUEI", "Delegate if Possible"),
    "VUMI" = list("VUMI", "Delegate if Possible"),
    "SUEI" = list("SUEI", "Schedule"),
    "EUNI" = list("EUNI", "Delegate if Possible"),
    "MUVI" = list("MUVI", "Delegate if Possible"),
    "VUSI" = list("VUSI", "Delegate if Possible"),
    "SUVI" = list("SUVI", "Schedule"),
    "SUEI" = list("SUEI", "Delegate if Possible"), # again, after scheduling
    "MUMI" = list("MUMI", "Delegate if Possible"),
    "NUEI" = list("NUEI", "Schedule"),
    "VUNI" = list("VUNI", "Delegate if Possible"),
    "SUMI" = list("SUMI", "Schedule"),
    "SUVI" = list("SUVI", "Delegate if Possible"), # again, after scheduling
    "MUSI" = list("MUSI", "Delegate if Possible"),
    "NUVI" = list("NUVI", "Schedule"),
    "NEUI" = list("NUEI", "Delegate if Possible"), # again, after scheduling
    "SUMI" = list("SUMI", "Delegate if Possible"), # again, after scheduling
    "MUNI" = list("MUNI", "Delegate if Possible"),
    "NUMI" = list("NUMI", "Schedule"),
    "NUVI" = list("NUVI", "Delegate if Possible"), # again, after scheduling
    "SUSI" = list("SUSI", "Delegate, Schedule, or Delete"),
    "NUMI" = list("NUMI", "Delegate if Possible"), # again, after scheduling
    "SUNI" = list("SUNI", "Delegate, Schedule, or Delete"),
    "NUSI" = list("NUSI", "Delegate, Schedule, or Delete"),
    "NUNI" = list("NUNI", "Delegate, Schedule, or Delete"))


  ######### Reorder in Asana - Rough ##########################################

  # (Future Plan)

  ######### QuickSort and Do Tasks ############################################

  # Create list of actions that can be selected after moving past each task
  action_completed <- list("Done", "Delegated", "Scheduled", "Can't Do Now")

  # Go through each EUEI bin in pre-determined order
  for (i in 1:length(do_order)) {

    # Set abbrev to ith entry in do_order
    abbrev <- do_order %>% names %>% .[[i]]

    # Filter todo list by most pressing EUEI bin
    bin <- todo %>% filter(EUEI == abbrev)

    # Display Green Message prompt when moving to the next EUEI bin
    if (nrow(bin) > 0) {
      cat(green(abbrev, " Tasks: ",
                # Show Composite score, 1-100 to help intuition
                "(Scored: ", bin$Composite[[1]], "/100) ",
                bin$Urgency_str[[1]], " & ", bin$Importance_str[[1]],
                sep = ""), "\n\n")

      # Perform quicksort for all tasks in this bin
      bin <- bin %>%
        mutate(bin_order = quickSort(bin$Task, compare)) %>%
        # rearrange order by quickSort results
        arrange(by = bin_order)

      if (nrow(bin) > 1) {
        # Only state that ordering is completed if more than 1 task in process
        cat(green("You have ordered all ",
                  bin$Urgency_str[[1]], " & ", bin$Importance_str[[1]],
                  " ( rank: ", bin$Composite[[1]], ")" ," tasks!",
                  "\n\n", sep = ""))

        wait_for_key("c")
      }


      # Prompt user to Do/Delegate/etc. the task
      for (task in bin$Task) {

        # Display suggested action based on EUEI, e.g. "Delegate if Possible"
        if (do_order[[i]][[2]] == "Schedule") {
          cat(red("Do not do yet, just schedule:\n"))
        } else {
          cat(green(do_order[[abbrev]][[2]]), "\n")
        }


        # Display menu to choose what kind of action has been taken
        action <- menu(action_completed, title = task)

        # todo <-
        a <- todo %>% filter(Task == task) %>%
          mutate(Last_Action = action, Date_last_action = now())

      }

    }
  }

  ######### Completion Prompt #################################################

  return(cat(bold(green("\nCongratulations!", "\n\n",
                        "You have completed all tasks!", sep = ""))))

  # You can reorder tasks in a project using the addProject API call
  # (and the insertBefore and insertAfter properties).
  # See https://asana.com/developers/api-reference/tasks
  # projects for more information.

}

########## Testing with Parameters ############################################
#
# decider(input_type = "asana",
#         # An Asana project to test with "Test Project" gid: 1148823248153567
#         asana_project_gid = "1148823248153567",
#         # asana_project_gid = Sys.getenv("ASANA_MYTASKS_PROJECT_ID"),
#         # run_shiny = FALSE,
#         # csv_path,
#         # csv_task_column_name = "Task",
#         testing_task_num = NA)
#

