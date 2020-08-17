library(shiny)
library(shinythemes)

shinyUI(fluidPage(
    
    titlePanel("Next Word Prediction"),
    
    navbarPage("Predict Next Word",
               
               tabPanel("Application",
                        sidebarLayout(
                            sidebarPanel(
                                textInput(inputId="text1", label = "Please enter your text here: ", value =""),
                            ),
                            
                            
                            
                            
                            mainPanel(
                                htmlOutput("html1")
                            )
                        )
               )
               
    )
))