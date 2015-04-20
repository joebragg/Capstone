shinyUI(fluidPage(
  titlePanel("Word Predictor"),
  sidebarLayout(
    sidebarPanel(
       p("Enter a phrase of one or more words and click submit to get a prediction of the next word."),
      textInput("phrase", "Enter a phrase:", value = ""),
      submitButton("Submit"),
      h4("The most likely next word is:"),
      textOutput("pred"),
      h4("You entered:"),
      p(textOutput("text"))
    ),
    mainPanel(
      plotOutput("cloud",width = "500px", height = "500px")
    )
  )
))