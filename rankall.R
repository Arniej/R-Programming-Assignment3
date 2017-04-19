#Ranking hospitals in all states Function rankall

rankall <- function(outcome, num = "best"){
    ## Read outcome data
    myData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
    
    ##Outcomes vector
    
    causes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
    myData <- na.omit(myData[, c(2, 7, causes[outcome])])
    
    ## Check that outcome is valid
    
    if(!outcome %in% names(causes)) {
        stop("invalid outcome")
    }
    
    
    myData[,3] <- as.numeric(myData[,3]) #Coerce outcome column to numeric
    myData <- myData[order(myData[,2],myData[,3],myData[,1]),] #Sort by State&Outcome&Name
    
    ## For each state, find the hospital of the given rank
    
    splitData <- split(myData,myData$State)
    hospitalNameFunction <- function(x) {
        num <- ifelse(num == "best", 1, ifelse(num == "worst", nrow(x), num))
        return(c(x[num,1]))
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    hospList <<- unlist(lapply(splitData, hospitalNameFunction))
    return(data.frame(hospital=hospList,state=names(hospList)))
    
    
    
    
}