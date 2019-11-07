######### Info ################################################################
# Adam Yormark
# eisenlikert function
# Part of Decider

######### Packages ############################################################
library(tidyverse)
library(crayon)

######### Function ############################################################

# This function asks the user to rank Urgency and Importance from 1-5
# 5 is the most Urgent or most Important
# Input is a character string
# Output is a two-column tibble with Urgency and Importance as integers
eisenlikert <- function(task) { 
  
  # Get Input on task urgency
  u <- readline(prompt = cat(white("---> ", 
                                   unlist(task), " <---"), 
                             green("\n",  
                                   "5 = Extremely Urgent\n",
                                   "4 = Very Urgent\n",
                                   "3 = Moderately Urgent\n",
                                   "2 = Somewhat Urgent\n",
                                   "1 = Not Very Urgent\n", 
                                   sep = ""))) %>% 
    as.numeric %>% round %>% as.integer %>% 
    enframe(name = NULL) %>% rename(Urgency = value)
  
  # Get Input on task importance
  i <- readline(prompt = cat(white("---> ", 
                                   unlist(task), " <---"), 
                             green("\n",
                                   "5 = Extremely Important\n",
                                   "4 = Very Important\n",
                                   "3 = Moderately Important\n",
                                   "2 = Somewhat Important\n",
                                   "1 = Not Very Important\n", 
                                   sep = ""))) %>% 
    as.numeric %>% round %>% as.integer %>% 
    enframe(name = NULL) %>% rename(Importance = value)
  
  return(bind_cols(u, i))
}

# eisenlikert(task)

