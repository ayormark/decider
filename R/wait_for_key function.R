# Wait for Key before moving on to next line function

wait_for_key <- function(key) {
  if (readline(prompt = cat("Press ", key, " to continue ", sep = "")) != key) {
    wait_for_key(key)
  }
}
