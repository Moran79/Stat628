library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("BODYFAT Calculator"),
  sidebarPanel(
    numericInput("a", "ABDOMEN(cm) :", min = 0, max = 1000,value=100),
    numericInput("b", "HEIGHT(inch) :", min = 0, max = 1000,value=70),
    helpText("Warning: Our model only apply to  normal body-sized male due to data limitation")
  ),
  mainPanel(
    submitButton(text="get ur bodyfat"),
    h3("BODYFAT"),
    verbatimTextOutput("B")
  )
))

