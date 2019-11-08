######### Info ################################################################
# Adam Yormark
# EUEI "Yoo-ee" Progression Visualization for Decider

######### Packages ############################################################
# library(dplyr)
library(ggplot2)

######### Packages ############################################################


######### Function ############################################################

todo <- read_csv(paste0(getwd(), "/todo.csv"))

euei_progression <- function () {

  # urgency <- seq.int(5, 1)
  # importance <- seq.int(5, 1)
  # enframe(importance, name = NULL)
  # matrix()
  # square_color <-


  # plot task dots
  ggplot(todo, aes(x = Importance, y = Urgency, color = Composite,
                   ymin = 0.5, xmin = 0.5, ymax = 5, xmax = 5)) +
    geom_point() + #plot points
    scale_x_reverse(position = "top") + # flip x axis
    # Make each point vary in size by quantity of tasks at that point
    stat_sum(aes(size = factor(..n..)), geom = "point") +
    scale_size_discrete() +
    # Add some variation in placement to help visualize quantity
    # Make all cells square
    coord_fixed() +
    # Add EUEI bucket squares
    # geom_raster(data = , aes(fill = Importance * Urgency), show.legend = FALSE) +
    theme_minimal() # minimal theme
    # geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1,
    #               fill = Urgency * Importance),
    #           show.legend = FALSE, alpha=0.1) +

}

# Dummy data
# x <- LETTERS[1:20]
# y <- paste0("var", seq(1,20))
# data <- expand.grid(X=x, Y=y)
# data$Z <- runif(400, 0, 5)
#
# # Heatmap
# ggplot(data, aes(X, Y, fill= Z)) +
#   geom_tile()


euei_progression()


