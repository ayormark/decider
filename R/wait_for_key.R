######### Info ################################################################
# Adam Yormark
# wait_for_key function for Decider

wait_for_key <- function(key) {
  if (readline(prompt = cat("Press ", key, " to continue ", sep = "")) != key) {
    wait_for_key(key)
  }
}
