library(shiny)
library(shinyjs)
# To do: use UTM strings to input group and user codes
shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Drunk History Submission Form"),

  textInput("group_code", "Group code:"),
  textInput("user_code", "Your code:"),
  textInput("user_submission", "Your submission:"),

  actionButton("action_submit", "Submit"),
  helpText("If you submit more than once, only your most recent submission will be used.")
))
