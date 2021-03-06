## rankhospital.R - rprog-015 assessment 3 part 2
##

rankhospital <- function(state, outcome, num="best") {
    ## state   - two character abbreviation of state
    ## outcome - "heart attack" | "heart failure" | "pneumonia"
    ## num     - "best", "worst", 1..n (rank)
   
    ## read the outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## create outcome lookup table
    lookup <- matrix(c("heart attack", "heart failure", "pneumonia",
                       11,             17,              23),
                     ncol=2)

    ## check that the state and outcome are valid (stop if not)
    if (!(outcome %in% lookup[,1])) {
        stop("invalid outcome")
    }
    if (!(state %in% df[,7])) {
        stop("invalid state")
    }

    ## subset the data on interest (hospital name, state, outcome)
    adata <- df[df$State==state,c(2,7,as.numeric(lookup[which(lookup[,1]==outcome),2]))]
    names(adata) <- c("name","state","outcome")
    ## get only the complete cases
    cdata <- adata[adata$outcome!="Not Available",]
    ## order data by outcome
    odata <- cdata[order(as.numeric(cdata$outcome),cdata$name),]

    maxidx <- length(odata[,1])

    if (is.numeric(num)) {
        idx <- as.numeric(num)
        ## check that requested index is in range
        if ((idx < 1) || (idx > maxidx)) { idx <- NA }
    } else {
        if (num=="best") {
            idx <- 1
        }
        else {
            if (num=="worst") {
                idx <- maxidx
            }
            else {
                idx <- NA
            }
        }
    }

    ## if num is in range of a valid result then return it
    if (!is.na(idx))
        odata[idx,1]        
    else
        NA
}


