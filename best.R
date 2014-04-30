

best <- function (statecall,outcomecall) {
        ##read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colclasses='character')
        ## check that state and outcome are valid
        If is.na (match(state,outcome$State)) {
                stop("invalid state")
        }
        ##how do you do this for outcome
        If is.na (match(outcome,outcome$)) {
                stop("invalid state")
        }
        ## Return hospital name in that state with lowest 30-day death rate
        ## First it sorts it by state
        statedata <- subset(outcome,State==statecall)
        outcomecall <- paste(outcomecall,"+",sep="")
        outcomeindex <- min(agrep(outcom`ecall,colnames(outcome),ignore.case=TRUE)) 
}