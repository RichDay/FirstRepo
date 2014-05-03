best <- function (statecall,outcomecall){
        ##read in the data
        path <- "C:\\Users\\Rich\\Documents\\Github\\FirstRepo\\outcome-of-care-measures.csv"
        data <- read.csv(path,stringsAsFactors=FALSE,na.strings="Not Available+")
        
        ##check for valid calls
        outcomecall <- paste(outcomecall,"+",sep="")
        if (!any(agrep(outcomecall,colnames(data),ignore.case=TRUE))) {
                stop('invalid outcome')
        } 
        if (!any(data$State==statecall)) {
                stop('invalid state')
        } 
        
        ##filter by state
        statedata <- subset(data,State==statecall)
        
        ##determine which column corresponds to outcome of interest     
        outcomeindex <- min(agrep(outcomecall,colnames(data),ignore.case=TRUE))
        
        ##build answermatrix and then clean out NAs
        answermatrix <- statedata[,c(2,outcomeindex)]
        answermatrix[,2] <- as.numeric(as.character(answermatrix[,2]))
        gooddata <- complete.cases(answermatrix)
        answermatrix <- answermatrix[gooddata,]
        
        ##find the hospital with the lowest score
        answerindex <- which.min(answermatrix[,2])
        besthospital <- answermatrix[answerindex,1]

        print (besthospital)
}
