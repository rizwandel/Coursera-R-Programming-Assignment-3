rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data.
        hospitalData <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        
        ## Check that state and outcome are valid.
        if (!state %in% hospitalData[, 7]) {
                stop("invalid state")
        }
        
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        
        ## Return hospital name in that state with the given rank 
        ## 30-day death rate by selected outcome.
        ## ----------------------------------------------------------------
        
        ## Keep only hospitals of the selected state.
        hospitalData <- hospitalData[(hospitalData[, 7] == state), ]
        
        
        ## Set the outcome column index.
        if (outcome == "heart attack") outcomecol = 11
        else if (outcome == "heart failure") outcomecol = 17
        else if (outcome == "pneumonia") outcomecol = 23
        
        ## Convert the outcomecol column to numeric (suppressing the warning), 
        ## remove rows with NA outcomecol values, and then order by outcomecol
        ## and name.
        suppressWarnings(
                hospitalData[, outcomecol] <- 
                        as.numeric(hospitalData[, outcomecol])
        )
        
        hospitalData <- hospitalData[!is.na(hospitalData[, outcomecol]), ]
        
        hospitalData <- hospitalData[order(
                hospitalData[, outcomecol], hospitalData[, 2]), ]
        
        ## Change num value to row id.
        if (num == "best") num <- 1
        else if (num == "worst") num <- nrow(hospitalData)
        else return
        
        ## Return the hospital name.
        hospitalName <- hospitalData[num, 2]
        hospitalName
}
