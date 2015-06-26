## best.R - rprog-015 assessment 3 part 2
##

best <- function(state, outcome) {
    ## state   - two character abbreviation of state
    ## outcome - "heart attack" | "heart failure" | "pneumonia" 
   
    ## read the outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## create outcome lookup table
    lookup <- matrix(c("heart attack", "heart failure", "pneumonia",
                       11,             17,              23),
                     ncol=2)

    ## check the parameters are valid
    if !(outcome %in% lookup[,1]) {
        stop("invalid outcome")
    }
    if !(state %in% df[,7]) {
        stop("invalid state")
    }
    
    ## rename the dataframe columns to be same as input parameter
    names(df)[11] <- "HEART ATTACK"
    names(df)[17] <- "HEART FAILURE"
    names(df)[23] <- "PNEUMONIA"
       
    ## check that the state and outcome are valid (stop if not)
    ## return hospital name in the state with the lowest 30-day death rate
}

