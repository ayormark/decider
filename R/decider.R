######### Info ################################################################
# Adam Yormark
# Decider

# The purpose of this script is to dramatically reduce the amount of time and
# mental effort it takes to decide which tasks to do and in what order
# This script is designed to work with Asana, but can be modified
# to work with any csv or list of tasks

# This script accomplishes this goal by combining together these concepts:
# The Eisenhower Matrix
# https://www.eisenhower.me/eisenhower-matrix/
# Likert Scale
# https://www.simplypsychology.org/likert-scale.html
# Quick Sort
# https://www.youtube.com/watch?v=WaNLJf8xzC4

# After importing the to do list, each task is then rated from 1-5 on both
# Urgency and Importance. These make up 25 "buckets"
# After Analysis the order of completion for buckets has been determined to be:
# "EUEI" "EUVI" "VUEI" "EUMI" "VUVI" "EUSI" "MUEI" "VUMI" "SUEI" "EUNI"
# "MUVI" "VUSI" "SUVI" "MUMI""NUEI" "VUNI" "SUMI" "MUSI" "NUVI" "MUNI"
# "NUMI" "SUSI" "SUNI" "NUSI" "NUNI"
# When tackling a bucket, all tasks are sorted using Quicksort

# source(paste0("/Users/adamyormark/Google Drive/Personal/R/To Do",
#               "Prioritization Optimization Tool.R"))

########## Future Goals #######################################################

# Modifications need to be made to account for a todo item becoming
# more urgent as time passes
# It may be safe to say that Importance remains constant, but Urgency changes
# The need to re-rate aspects of specific items should be eliminated
# Create a way for a single new item or small set of new items to be
# efficiently inserted into an already-sorted list (append()?)
# Account for task size? Extra Large task, Large Task, Medium, etc.

#' @import tidyverse
#' @import rje
#' @import stringr
#' @import shiny
#' @import crayon
#' @import httr
#' @import jsonlite
#' @import qdapRegex
#' @import here
#' @import lubridate
#' @import asana
# library(RCurl)
# library(rvest)
# library(rebus)

# library(asana)


# Handy function to open working directory for testing
opendir <- function(directory = getwd()){
  system(sprintf('open %s', shQuote(directory)))
}

########## Decider Function ###################################################

#' @export
decider <- function(csv_path, csv_task_column_name = Task, testing_task_num) {

  # setwd("~/Google Drive/Personal/R/decider")

  # Choose whether to import csv or connect with Asana API
  input_type <- "asana"
  # input_type <- "csv"
  if (input_type == "csv") {
    csv_folder <- "/Users/adamyormark/Downloads/"
    csv_filename_base <- "asana2go_output_csv_basic"
  }

  # If testing, select how many tasks to rank
  # rm(testing_task_num)
  # testing_task_num <- 4

  # Run shiny app?
  shiny = FALSE

  ######### CSV Import ##########################################################

  if (input_type == "csv") {

    # Import csv file from path
    todo <- read_csv(csv_path)

    # rename selected task column
    todo <- rename(Task = csv_task_column_name)

  }

  ######### Asana Import ######################################################

  if (input_type == "asana") {asana_import()}

  ######### Functions ###########################################################

  #### eisenlikert() function ####
  # The eisenlikert() function asks user to rank Urgency and Importance from 1-5
  # 5 is the most Urgent or most Important
  # Output is a tibble with two elements, Urgency and Importance, as integers
  source(paste0(getwd(), "/R/eiskenlikert.R"))


  #### compare() function ####
  # A function to choose which of two options is more important
  # To be used within the quickSort() function
  # Output is either -1 or 1, similar output to greaterThan() from rje
  source(paste0(getwd(), "/R/compare.R"))

  wait_for_key <- function(key) {
    if (readline(prompt = cat("Press ", key, " to continue ", sep = "")) != key) {
      wait_for_key(key)
    }
  }

  ######### Shiny ###############################################################
  if (shiny == TRUE) {

    ui <- fluidPage(
      # *Input() functions here
      # *Output() functions here
      actionButton("1Button", "1"),
      actionButton("1Button", "2"),
      actionButton("1Button", "3"),
      actionButton("1Button", "4"),
      actionButton("1Button", "5")
    )

    # Input: user rating
    # Output: Task list, ranked, then sorted
    server <- function(input, output) {}

    shinyApp(ui = ui, server = server)

  }

  ######### Rate Each Task ######################################################
  # eisenlikert Rating

  # Tell user how many tasks they are about to rate
  asana_user_info <- asn_users_me()
  asana_first_last <- asana_user_info$content$data$name
  first_name <- strsplit(asana_first_last, " ")[[1]][1]
  cat(green(paste0("Welcome, ", first_name, "\n")))

  cat(green("You have", todo %>% nrow(), "tasks to rate", "\n"), sep = "")

  if (exists("testing_task_num")) {
    cat(red("Note: testing with", testing_task_num, "Tasks\n"))
  }

  wait_for_key("c")


  # Choose column names
  ratings_columns <- c("Task", "Urgency", "Importance", "Date_Recorded")

  # Create empty dataframe with same number of columns
  ratings <- data.frame(matrix(nrow = 0, ncol = ratings_columns %>% length()))
  # Name the columns
  colnames(ratings) <- ratings_columns

  # When testing, you may select a subset of tasks to reduce testing time
  if (exists("testing_task_num")) { todo <- todo %>% sample_n(testing_task_num) }

  # Run the eisenlikert function on each task and add ratings to the dataframe
  for (task in todo$Task) {

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

  todo <- todo %>% mutate(Composite = 4 * Urgency * Importance)

  todo <- todo %>% arrange(-as.numeric(Date_Recorded))

  cat(green("You have completed ranking Urgency and Importance \n\n"), sep = "")
  wait_for_key("c")

  if (!file.exists(prerated_todo_csv_path)) {
    write_csv(todo, prerated_todo_csv_path)
  }

  ######### EUEI Abbreviation and strings #######################################

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


  # The order of tasks to move through
  do_order <- list(
    "EUEI" = list("EUEI","Delegate if Possible"),
    "EUVI" = list("EUVI", "Delegate if Possible"),
    "VUEI" = list("VUEI", "Delegate if Possible"),
    "EUMI" = list("EUMI", "Delegate if Possible"),
    "VUVI" = list("VUVI", "Delegate if Possible"),
    "EUSI" = list("EUSI", "Delegate if Possible"),
    "MUEI" = list("MUEI","Delegate if Possible"),
    "VUMI" = list("VUMI", "Delegate if Possible"),
    "SUEI" = list("SUEI", "Schedule"),
    "EUNI" = list("EUNI", "Delegate if Possible"),
    "MUVI" = list("MUVI", "Delegate if Possible"),
    "VUSI" = list("VUSI", "Delegate if Possible"),
    "SUVI" = list("SUVI", "Schedule"),
    "MUMI" = list("MUMI", "Delegate if Possible"),
    "NUEI" = list("NUEI", "Schedule"),
    "VUNI" = list("VUNI", "Delegate if Possible"),
    "SUMI" = list("SUMI", "Schedule"),
    "MUSI" = list("MUSI", "Delegate if Possible"),
    "NUVI" = list("NUVI", "Schedule"),
    "MUNI" = list("MUNI", "Delegate if Possible"),
    "NUMI" = list("NUMI", "Schedule"),
    "SUSI" = list("SUSI", "Delegate, Schedule, or Delete"),
    "SUNI" = list("SUNI", "Delegate, Schedule, or Delete"),
    "NUSI" = list("NUSI", "Delegate, Schedule, or Delete"),
    "NUNI" = list("NUNI", "Delegate, Schedule, or Delete"))


  # Get rank of any EUEI in do_order
  # match(EUEI, names(do_order))

  ######### Reorder in Asana ####################################################

  # (Future Plan)

  ######### QuickSort and Do Tasks ##############################################

  # Create list of actions that can be selected after moving past each task
  action_completed <- list("Done", "Delegated", "Scheduled", "Can't Do Now")

  # Go through each EUEI bucket in pre-determined order
  for (abbrev in names(do_order)) {

    # Filter todo list by most pressing EUEI bucket
    set <- todo %>% filter(EUEI == abbrev)

    # Display Green Message when starting a new EUEI Bucket
    if (nrow(set) > 0) {
      cat(green(abbrev, " Tasks: ",
                # Show a composite score, 1-100 to help intuition
                "(", set$Composite[[1]], ") ",
                set$Urgency_str[[1]], " & ", set$Importance_str[[1]],
                sep = ""), "\n\n")

      # Perform quicksort for all tasks in this bucket
      set <- set %>%
        mutate(set_order = quickSort(set$Task, compare)) %>%
        # rearrange order by quickSort results
        arrange(by = set_order)

      if (nrow(set) > 1) {
        # Only state that ordering is completed if more than 1 task in process
        cat(green("You have ordered all ",
                  set$Urgency_str[[1]], " & ", set$Importance_str[[1]],
                  " (", set$Composite[[1]], ")" ," tasks!",
                  "\n\n", sep = ""))

        wait_for_key("c")
      }


      for (task in set$Task) {

        # Display suggested action based on EUEI, e.g. "Delegate if Possible"
        if (do_order[[abbrev]][[2]] == "Schedule") {
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

  return(cat(bold(green("\nCongratulations!", "\n\n",
                        "You have completed all tasks!", sep = ""))))

  # You can reorder tasks in a project using the addProject API call
  # (and the insertBefore and insertAfter properties).
  # See https://asana.com/developers/api-reference/tasks
  # projects for more information.



}