######### Info ################################################################
# Adam Yormark
# Shiny app for Decider

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
