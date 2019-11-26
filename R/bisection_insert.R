######### Info ################################################################
# Adam Yormark
# bisection_insert function for Decider

######### Documentation #######################################################

#' Insert A Single Task Efficiently Into An Already-Sorted List
#' @param task_to_insert The tast to be sorted, a character
#' @param tasks The presorted list, a tibble
#' @param name The column of the character description of tasks
#' @param gid The column with a unique global id for each task
#' @export

#' @examples
#' task_to_insert <- "Fix the roof"
#' tasks <- tibble("To-Do" = c("task a", "task b", "task c",
#' "task d", "task e", "task f", "task g"))
#' bisection_insert(task_to_insert, tasks, name = "To-Do")

######### Function ############################################################

bisection_insert <- function (task_to_insert,
                              tasks,
                              name = "Task",
                              gid = "gid") {

  # library(dplyr)

  #### compare function ####
  # A function to choose which of two options is more important
  # To be used within the quickSort() and bisection_insert() function
  # Output is either -1 or 1, similar output to greaterThan() from rje
  source(paste0(getwd(), "/R/compare.R"))

  # Select the column with the tasks
  tasks <- tasks %>% select(name, gid)

  # rename columns
  tasks <- tasks %>% rename(Task = name, gid = gid)

  # Add an additional task in the event the new task should be last
  last_task <- enframe("__Last_Task__", name = NULL)
  colnames(last_task) <- name

  tasks <- bind_rows(tasks, last_task)

  # Save max and min values for bisection
  max <- nrow(tasks)
  min <- 0

  # Continue bisecting until place in list is found
  while (max - min > 1) {
    # For mathematical troubleshooting help
    # cat(paste0("range = ", max-min, "\n"))
    # cat(paste0("max = ", max, "\n"))
    # cat(paste0("min = ", min, "\n"))

    # Find midpoint of the relevant section of the task list
    midpoint <- (max-min)/2
    # cat(paste0("midpoint = ", midpoint, "\n"))

    # Select the task at the midpoint
    midpoint_task <- tasks %>% select(Task) %>% slice(min + round(midpoint))

    # Compare the new task with the midpoint task
    comparison_val <- compare(task_to_insert, midpoint_task)

    # Move task down in completion order
    if (comparison_val == -1) {

      max <- max - (max-min)/2

      # Move task up in completion order
    } else if (comparison_val == 1) {

      min <- min + (max-min)/2

    }
    # Save current task to insert before
    anchor_task <- tasks %>% select(gid) %>% slice(round(max)) %>% .$gid
    anchor_task_name <-
      tasks %>% select(Task) %>% slice(round(max)) %>% .$Task

  }

  # Return value after bisection converges on a task
  cat(paste0("range = ", max-min, "\n"))
  cat(paste0("max = ", max, "\n"))
  cat(paste0("min = ", min, "\n"))

  # If task should be placed last, then use insert_after instead
  if (anchor_task_name != "__Last_Task__") {
    # Insert before a task
    placement <- "insert_before"
  } else if (anchor_task_name == "__Last_Task__") {
    # Insert after last task
    placement <- "insert_after"
    anchor_task <-
      tasks %>% select(gid) %>% slice(nrow(tasks)-1) %>% .$gid
    anchor_task_name <-
      tasks %>% select(Task) %>% slice(nrow(tasks)-1) %>% .$Task
    }

  output <- tibble(placement, anchor_task_name, anchor_task)

    return(output)

}

# bisection_insert(task_to_insert, tasks, name = "To-Do")

