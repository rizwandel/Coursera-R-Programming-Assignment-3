rankall <- function(outcome, num = "best") {
        ## Read outcome data.
        hospitalDataOriginal <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        
        ## Check that outcome is valid.
        
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name with the given rank 
        ## 30-day death rate by selected outcome.
        ## ----------------------------------------------------------------
        
        hospitalDataFrame <- data.frame()
        
        for (i in sort(unique(hospitalDataOriginal[, 7]))) {
                
                ## Keep only hospitals of the selected state.
                hospitalData <- hospitalDataOriginal[(hospitalDataOriginal[, 7] == i), ]
                
                
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
                if (num == "best") rownum <- 1
                else if (num == "worst") rownum <- nrow(hospitalData)
                else rownum <- num
               
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
                hospitalName <- hospitalData[rownum, 2]
                hospitalDataFrame <- rbind(hospitalDataFrame, 
                                           data.frame(hospital = hospitalName, 
                                                      state = i), 
                                           stringsAsFactors = FALSE)
        }
        
        hospitalDataFrame
}
