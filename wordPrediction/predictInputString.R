library(stringi)
library(tm)
library(SnowballC)
library(RWeka)
library(wordcloud)
library(plyr)
library(ggplot2)
library(tidytext)
library(stringr)

load("tdmList.RData")

###############################
# Get exact match
###############################
getExactMatch <- function(inputStr, ngNames) {
  i <- 1
  result <- FALSE
  
  if (is.null(inputStr))
    return (0)
  
  for (i in 1:length(ngNames$names)) {
    if (inputStr == ngNames$names[i]) {
      result <- TRUE
      break
    }
  }
  
  if (FALSE == result)
    i <- 0
  
  return (i)
}

###############################
#
###############################
getNGramMatch <- function(inputStr, ngList) {
  matchList <- NULL
  
  #Get exact match from n-gram models
  for (i in 0:3) {
    matchList <- c(matchList, getExactMatch(inputStr, ngList[1 + i * 2]))
  }
  
  return (matchList)
}

###############################
# Get partial match
###############################
getAnyMatch <- function(inputStr, ngNames) {
  i <- 1
  result <- FALSE
  
  for (i in 1:length(ngNames$names)) {
    if (grepl(inputStr, ngNames$names[i]) == TRUE) {
      result <- TRUE
      break
    }
  }
  
  if (FALSE == result)
    i <- 0
  
  return (i)
}

####################################################
# Get all partial matches (match from the beginning)
####################################################
getAllMatch <- function(inputStr, ngNames) {
  i <- 1
  resultList <- NULL
  tagetStr <- paste("^", inputStr, "\\>", sep = "")
  
  for (i in 1:length(ngNames$names)) {
    #  if (grepl(paste("^", inputStr, sep = ""), ngNames$names[i]) == TRUE) {
    if (grepl(tagetStr, ngNames$names[i]) == TRUE) {
      resultList <- c(resultList, i)
    }
  }
  
  return (resultList)
}

###############################
# Get number of 1 gram which occurs once
###############################
get1GramFreq1 <- function(ngList) {
  count1 <- 0
  total1Gram <- length(ngList[2]$x)
  
  for (i in 1:total1Gram) {
    if (1 == ngList[2]$x[i])
      count1 <- count1 + 1
  }
  
  return (count1)
}

###############################
# Get number of 2 grams which occurs once
###############################
get2GramFreq1 <- function(ngList) {
  count1 <- 0
  total2Gram <- length(ngList[4]$x)
  
  for (i in 1:total2Gram) {
    if (1 == ngList[4]$x[i])
      count1 <- count1 + 1
  }
  
  return (count1)
}


num1GramFreq1 <- get1GramFreq1(tdmList)
num2GramFreq1 <- get2GramFreq1(tdmList)

###############################
#
###############################
getNGramScore = function(inputStr, ngList) {
  score <- 0
  matched4gramCount <- 0
  matched3gramCount <- 0
  matched2gramCount <- 0
  
  inputLen <- str_count(inputStr, "\\S+")
  
  #No match found
  if (is.null(inputStr)) {
    score <- num1GramFreq1 / length(ngList[2]$x)
  }
  else{
    
    if (inputLen >= 4){
      matched4gramCount <- getExactMatch(inputStr, ngList[7]) 
      matched3gramCount <- getExactMatch(inputStr, ngList[5])
      matched2gramCount <- getExactMatch(inputStr, ngList[3]) 
    }
    else if (inputLen == 3){
      matched3gramCount <- getExactMatch(inputStr, ngList[5])
      matched2gramCount <- getExactMatch(inputStr, ngList[3]) 
    }
    else if (inputLen == 2){
      matched2gramCount <- getExactMatch(inputStr, ngList[3]) 
    }
    
    if (matched4gramCount > 0) {
      input3gramCount <- getExactMatch(getNMinusGram(inputStr), ngList[5])
      score <- 0.4 * matched4gramCount / input3gramCount
    }
    else if (matched3gramCount > 0) {
      input2gramCount <- getExactMatch(getNMinusGram(inputStr), ngList[3]) 
      score <- 0.4 *  0.4 * matched3gramCount / input2gramCount
    }
    else if (matched2gramCount > 0) {
      input1gramCount <- getExactMatch(getNMinusGram(inputStr), ngList[1])
      
      #if the unigram is unseen, calculate N1/N
      #(total 1 gram which occured once) / total 1 gram
      if (input1gramCount == 0) {
        score <- num1GramFreq1 / length(ngList[2]$x)
      }
      #if the unigram is seen once, calculate 2 * N2/N1
      #2 * total 2 gram which occured once / total 1 gram which occured once
      else if (input1gramCount == 1) {
        score <- num2GramFreq1 / num1GramFreq1
      }
      else{
        score <- 0.4 * 0.4 * 0.4 * matched2gramCount / input1gramCount
      }
    }
  }
  return (score)
  #
  # if (candidateIs4gram) {
  #   score = 0.4 * matched4gramCount / input3gramCount
  # } else if (candidateIs3gram) {
  #   score = 0.4 * 0.4 * matched3gramCount / input2gramCount
  # } else if (candidateIs2gram) {
  #   score = 0.4 * 0.4 * 0.4 * matched2gramcount / input1gramCount
  # }
}

###############################
#
###############################
getNMinusGram <- function(inputStr) {
  oriLen <- str_count(inputStr, "\\S+")
  
  return (paste(tail(
    strsplit(inputStr, split = " ")[[1]], oriLen - 1
  ), collapse = ' '))
}


###############################
#
############################### 
predictInputString <- function(inputStr, inputTDMList){
  #Get current word count
  currentCount <- stri_count_words(inputStr)
  
  #Truncate into 3 gram if it is longer
  if (currentCount > 3){
    oriLen <- str_count(inputStr, "\\S+")
    
    inputStr <- (paste(tail(strsplit(inputStr,split=" ")[[1]], 3), collapse = ' '))
    
    currentCount <- 3
  }
  
  nextCount <- currentCount + 1
  potentialList <- NULL
  
  #Get all possible NGrams for the inputstr
  repeat{
    potentialList <- getAllMatch(inputStr, inputTDMList[nextCount * 2 -1])
    
    if ((!is.null(potentialList)) || (currentCount == 1)){
      break
    }
    
    nextCount <- currentCount 
    currentCount <- currentCount - 1
    
    inputStr <- (paste(tail(strsplit(inputStr,split=" ")[[1]], currentCount), collapse = ' '))
    
  }

  #Calculate score for the list
  scoreList <-c(NULL)
  
  if (is.null(potentialList)){
    #Return the result and scores
    resultDF <- data.frame(1, inputTDMList[1]$names[1], inputTDMList[2]$x[1] / length(inputTDMList[2]$x))
  }
  else{
    scoreList <- vector()
    scoreList <- c(scoreList, 1:length(potentialList))
    
     for (i in 1:length(potentialList)){
        scoreList[i] <- getNGramScore(inputTDMList[nextCount * 2 -1]$names[potentialList[i]], inputTDMList)
     }
  
    #Sort the score list
    scoreIndex <- sort(scoreList, decreasing = TRUE, index.return=TRUE)$ix
    
    #Return the result and scores
    resultDF <- data.frame(scoreIndex, inputTDMList[nextCount * 2 -1]$names[potentialList], scoreList)
  }
  
  #Give mearningful names
  colnames(resultDF) <- c("index", "predicted", "odds")
  
  return (resultDF)
}

###############################
#
############################### 
predictTop <- function(inputStr){
  #Get all prediction
  resultDF <- predictInputString(inputStr, tdmList)
  
  #Get the index of the top 5
  topIndex <- which(resultDF[3] == max(resultDF[3]))
  
  topStr <- resultDF$predicted[topIndex]
  
#  topResult <- data.frame(resultDF$predicted[topIndex], resultDF$odds[topIndex])

  #Give mearningful names
#  colnames(topResult) <- c("predicted", "odds")
  
  return (tail(strsplit( as.character(topStr[1]), split = " ")[[1]], 1))
}