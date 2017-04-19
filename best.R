##Function Best - Finds best Hospital in a state##

best <- function (state, outcome){
    ##Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## ignores Warning message
    
    outcomedata[,11] <- suppressWarnings(as.numeric(outcomedata[,11])) ## heart attack
    outcomedata[,17] <- suppressWarnings(as.numeric(outcomedata[,17])) ## heart failure
    outcomedata[,23] <- suppressWarnings(as.numeric(outcomedata[,23])) ## pneumonia
    
    # The three types of outcomes are stored in a vector 
    
    outcome_reason <- c('heart attack', 'heart failure', 'pneumonia')
    
    if (!outcome %in% outcome_reason) { 
        stop('invalid outcome') 
    }
    
    # choose a outcome column based on result
    if (outcome == 'heart attack' ) { 
        column <- 11 
    }
    
    if (outcome == 'heart failure' ) { 
        column <- 17 
    }
    
    if (outcome == 'pneumonia' ) {
        column <- 23 
    }
    
    
    ##Check that state and outcome are valid
    statesList <- unique(outcomedata$State)
    if (!state %in% statesList) { 
        stop('invalid state') 
    }
    
    # state subset
    outcomedatabyState <- outcomedata[grep(state, outcomedata$State, ignore.case = TRUE),]
    Best <- outcomedatabyState[order(outcomedatabyState[,column],outcomedatabyState[,2]),]
    
    ##Return hospital name in that state with lowest 30-day death rate
    Best[1,2]
    
}

    