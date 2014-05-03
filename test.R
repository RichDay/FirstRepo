rankhospital <- function (statecall,outcomecall,num="best"){
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
 
        
        ##assign values for best
        if (num=="best") {
                num<-1
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

        ##sort the hospitals
        colnames(answermatrix) <- c("hospital", "score")
        r <- order(answermatrix$score,answermatrix$hospital)
        sorted <- answermatrix[r,]

        ##determine hospital selected in num
        if (num=="worst") {
                num<-nrow(sorted)
        }
        if (num>nrow(statedata)) {
                answer=NA
        } else {
                answer<-sorted[num,1]    
        }
        
        print(answer)
}

