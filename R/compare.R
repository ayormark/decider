######### Info ################################################################
# Adam Yormark
# compare, a function for Decider

######### Documentation #######################################################

#' Choose Which Of Two Options Is More Important
#' @description
#' To be used within the quickSort() function from package rje
#' Output is either -1 or 1, similar output to greaterThan() from rje
#' @param choice1 The first item, a character
#' @param choice2 The second item, a character
#' @export

#' @examples
#' choice1 <- "Fix the roof"
#' choice2 <- "Clean the living room"
#' compare(choice1, choice2)

######### Function ############################################################

compare <- function (choice1, choice2) {

  choice1 <- choice1 %>% as.character()
  choice2 <- choice2 %>% as.character()

  x <- c(choice1, choice2)

  cat(green("Which should be done first?", "\n"))

  selection <- menu(x, graphics = F, title = NULL)

  if (selection == 1) {
    return(-1)
  } else if (selection == 2) {
    return(1)
  }

}
