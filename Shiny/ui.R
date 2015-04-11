shinyUI(fluidPage(
  titlePanel("Word Predictor"),
  sidebarLayout(
    sidebarPanel(
      textInput("phrase", "Enter a phrase:", value = ""),
      h4("The most likely next word is:"),
      textOutput("pred"),
      h4("You entered:"),
      p(textOutput("text"))
    ),
    mainPanel(
      plotOutput("cloud",width = "400px", height = "400px")
    )
  )
))