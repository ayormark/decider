######### Info ################################################################
# Adam Yormark
# bisection_insert function for Decider

######### Documentation #######################################################

#' Insert a single task efficiently into an already-sorted list
#' @param task The tast to be sorted, a character
#' @param tasks The pre-sorted list, a tibble
#' @param tasks_column The column name of the tasks within the presorted list
#' @export

#' @examples
#' task <- "Fix the roof"
#' tasks <- tibble("To-Do" = c("task a", "task b", "task c",
#' "task d", "task e", "task f", "task g"))
#' bisection_insert(task, tasks, tasks_column = "To-Do")

######### Function ############################################################

bisection_insert <- function (task, tasks, tasks_column = "Task") {

  # library(dplyr)

  #### compare function ####
  # A function to choose which of two options is more important
  # To be used within the quickSort() and bisection_insert() function
  # Output is either -1 or 1, similar output to greaterThan() from rje
  source(paste0(getwd(), "/R/compare.R"))

  # Select the column with the tasks
  tasks <- tasks %>% select(tasks_column)

  # Save max and min values for bisection
  max <- nrow(tasks)
  min <- 0

  # Continue bisecting until place in list is found
  while (max - min > 1) {
    cat(max-min)

    # Find midpoint of the relevant section of the task list
    midpoint <- round((max-min)/2)

    # Select the task at the midpoint
    midpoint_task <- slice(tasks, midpoint)

    # Compare the new task with the midpoint task
    comparison_val <- compare(task, midpoint_task)

    # Move task up in completion order
    if (comparison_val == -1) {

      min <- min + round((max-min)/2)

      # Move task down in completion order
    } else if (comparison_val == 1) {


      max <- max - round((max-min)/2)

    }
    # Save current task to insert before
    insert_before <- slice(tasks, midpoint)

  }

  # Return value after bisection converges on a task
  return(insert_before)

}

# bisection_insert(task, tasks, tasks_column = "To-Do")

