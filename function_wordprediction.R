library(stringr)
library(dplyr)
library(tm)
library(stylo)

ngramFour <- readRDS("ngramFour.rds")
ngramThree <- readRDS("ngramThree.rds")
ngramTwo <- readRDS("ngramTwo.rds")

predictNextWord <- function(input)
{
        wordInput <- cleanInput(input)
        wordCount <- length(wordInput)
        prediction <- c()
        
        if(wordCount>3)
        {
                wordInput <- wordInput[(wordCount-2):wordCount]
                prediction <- matchinFourGranm(wordInput[1],wordInput[2],wordInput[3])
        }
        
        if(wordCount ==3)
        {
                prediction <- matchinFourGranm(wordInput[1],wordInput[2],wordInput[3])
        }
        
        if(wordCount ==2)
        {
                prediction <- matchThreeGram(wordInput[1],wordInput[2])
        }
 
        if(wordCount ==1)
        {
                prediction <- matchTwoGram(wordInput[1])
        }
        
        if(wordCount == 0)
        {
                prediction <- "Please enter the word"
        }
        
        if(length(prediction)==0)
        {
                prediction <- "Sorry, I can not predice the next word"
        }
        
        if(length(prediction) < 5)
        {
                prediction
        }
        else
        {
                prediction[1:5]
        }
        
        
}

cleanInput <- function(text){
        textInput <- tolower(text)
        textInput <- removePunctuation(textInput)
        textInput <- removeNumbers(textInput)
        textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
        textInput <- stripWhitespace(textInput)
        textInput <- txt.to.words.ext(textInput)
        return(textInput)
}

matchinFourGranm <- function (inputWord1,inputWord2,inputWord3)
        
{
        predictWord <- filter(ngramFour,(word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
        if(length(predictWord) == 0)
        {
                
                predictWord <- filter(ngramFour,( word2 == inputWord2 & word3 == inputWord3))$word4
                if(length(predictWord) == 0)
                {
                        predictWord <- filter(ngramFour,( word1 == inputWord2 & word2 == inputWord3))$word3
                        
                        
                        if(length(predictWord) ==0)
                        {
                                predictWord <- matchThreeGram(inputWord2,inputWord3)
                        }
                        
                }
                
        }
        
        predictWord
        
}

matchThreeGram <- function(inputWord1,inputWord2)
{
        predictWord <- filter(ngramThree,( word1 == inputWord1 & word2 == inputWord2))$word3
        if(length(predictWord)==0)
        {
                predictWord <- filter(ngramThree,(word2 == inputWord2))$word3 
                
                if(length(predictWord)== 0)
                {
                        predictWord <- filter(ngramThree,(word1 == inputWord2))$word2 
                        
                        if(length(predictWord) ==0 )
                        {
                                predictWord <- matchTwoGram(inputWord2)
                        }
                        
                }
        }
        predictWord
}

matchTwoGram <- function(inputWord1)
{
        predictWord <- filter(ngramTwo,( word1 == inputWord1 ))$word2
        
        predictWord
        
}

