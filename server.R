library(shiny)
library(tm)
library(stringr)
library(stylo)
library(quanteda)
library(ngram)
library(dplyr)
library(data.table)
library(tokenizers)
library(stringr)
library(dplyr)
library(tm)
library(stylo)

source("function_wordprediction.R")

shinyServer(function(input, output) {
    
    
    output$html1 <- renderUI({
        
        inputText <- input$text1
        inputText <- cleanInput(inputText)
        prediction <- predictNextWord(inputText)
        
        str1 <- "Predicted word(s): "
        
        str2 <- ""
        
        for(i in 1:length(prediction))
        {
            if(!is.na(prediction[i]))
            {
                
                if(prediction[i] == "Please enter something")
                {
                    str2 <-paste(str2, "<span style= color:green>",h4("Please enter something", align = "left"), "</span>")
                    str1 <- ""
                }
                else if(prediction[i] == "Sorry, I can not predict that words"){
                    str2 <-paste(str2, "<span style= color:red>",h4(prediction[i],align = "left"), "</span>")
                    str1 <- ""
                }
                else{
                    prediction[i] <- paste(i,". ", prediction[i])
                    str2 <- paste(str2, h4(prediction[i], align = "left"), "</span>")
                }
            }
        }
        str1 <- h4(str1, align = "left")
        HTML(paste(str1, str2))
    })
    
    
    
    
})