######### Info ################################################################
# Adam Yormark
# compare, a function for Decider

# A function to choose which of two options is more important
# To be used within the quickSort() function from package rje
# Output is either -1 or 1, similar output to greaterThan() from rje
compare <- function (choice1, choice2) {

  choice1 <- choice1 %>% as.character()
  choice2 <- choice2 %>% as.character()

  x <- c(choice1, choice2)

  cat(green("Which should be done first?", "\n"))

  # menu(choices, graphics = FALSE, title = NULL)
  selection <- menu(x, graphics = F, title = NULL)

  if (selection == 1) {
    return(-1)
  } else if (selection == 2) {
    return(1)
  }

}
