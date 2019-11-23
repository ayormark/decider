######### Info ################################################################
# Adam Yormark
# bin_performance_order function for Decider

######### Documentation #######################################################

#' Get the order to do tasks based on Urgency, Importance, and Urgency Bias.
#'
#' @description
#' Each combination of Urgency and Importance can be imagined as a set of x and
#' y coordinates. A line can then be shifted from the point which represents
#' tasks that should be performed soonest to the point that represents tasks
#' that should be performed last. This finds the order in which that line
#' passes through those points
#'
#' Returns a dataframe with three columns, the u rank, the i rank, and
#' performance rank. A lower number for performance rank means it should be
#' performed sooner.
#'
#' @param urgency_bias A bias toward weighting Urgency more heavily than
#' Importance. Urgency bias above 1 favors Urgency, below 1 favors importance.
#' @param bin_u_max The highest integer tank for Urgency
#' @param bin_i_max The highest integer tank for Importance
#' @param bin_u_min The lowest integer tank for Urgency
#' @param bin_i_min The lowest integer tank for Importance
#' @examples
#' bin_performance_order(urgency_bias = 1.4)

######### Function ############################################################

# library(tidyr)
# library(dplyr)


bin_performance_order <- function (urgency_bias = 1.4,
                             bin_u_max = 5,
                             bin_i_max = 5,
                             bin_u_min = 1,
                             bin_i_min = 1) {

  # Urgency bias above 1 favors urgency, below favors importance
  # Mathematically, this is the negative slope of the shifting line
  slope <- -urgency_bias

  # Mathematical constraints on urgency bias
  if (urgency_bias == 1) {
    stop("Indeterminate. Urgency Bias can't be 1")
  }
  if (urgency_bias < 0) {
    stop("Urgency Bias can't be below 0")
  }
  if (urgency_bias > 4) {
    warning(paste0("An Urgency Biases above _", bin_i_max - 1, "_ will not alter results"))
  }


  u <- seq.int(bin_u_min, bin_u_max)
  i <- seq.int(bin_i_min, bin_i_max)
  # All combinations of u and i
  ui_combinations <- expand_grid(u, i)

  # Calculate starting and ending i intercept values, b
  b_start <- bin_i_max - (slope)*bin_u_max
  b_end <- bin_i_min - (slope)*bin_u_min

  # Move b down by the minimal step distance after each task
  b_step <- (b_end - b_start)/(nrow(ui_combinations))

  ui_rankings <- matrix(ncol = 3, nrow = 0)
  colnames(ui_rankings) <- c("u", "i", "distance_rank")
  ui_rankings <- as_tibble(ui_rankings)

  for (ui_combo in 1:nrow(ui_combinations)) {

    ui_point <- ui_combinations[ui_combo,]

    # Distance formula for a point to a line
    dist <- abs(b_start + (slope * ui_point[1]) - ui_point[2]) / sqrt(1 + slope^2)
    dist <- dist %>% as_tibble %>% rename(distance_rank = u)

    ui_ranked <- bind_cols(ui_point, dist)
    # dist <- dist2Line(ui_point, line)

    ui_rankings <- bind_rows(ui_rankings, ui_ranked)

    # dist <- sqrt(dist[[2]]^2 + dist[[3]]^2)

  }

  ui_rankings <- ui_rankings %>% arrange(by = distance_rank) %>%
    # Add generalized_score to help with intuition on value of tasks in a bin
    mutate(generalized_score = seq.int(1, nrow(ui_rankings))) %>%
    # Remove distance_rank because it is unintuitive
    select(-distance_rank)

  # a lower rank means it should be done sooner
  return(ui_rankings)

}
