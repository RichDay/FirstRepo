rankall <- function (outcomecall,num="best"){
        ##read in the data
        path <- "C:\\Users\\Rich\\Documents\\Github\\FirstRepo\\outcome-of-care-measures.csv"
        data <- read.csv(path,stringsAsFactors=FALSE,na.strings="Not Available+")
        
        ##check for valid calls
        outcomecall <- paste(outcomecall,"+",sep="")
        if (!any(agrep(outcomecall,colnames(data),ignore.case=TRUE))) {
                stop('invalid outcome')
        } 
        
        ##assign values for best
        if (num=="best") {
                num<-1
        }
                
        ##determine which column corresponds to outcome of interest     
        outcomeindex <- min(agrep(outcomecall,colnames(data),ignore.case=TRUE))
        
        ##generate list of unique"states"
        states<-data$State
        states<-unique(states)
        states<-sort(states)
        numstates<-length(states)
        ##create list s split by state
        s<-split(data,data$State)
        summarylist <- lapply(s,"[",c(2,outcomeindex))
        stateanswer<-NULL
        valueanswer<-NULL
        tempvalues<-NULL
        hospitalanswer<-NULL
        for(i in 1:numstates){
                print(i)
                statename<-states[i]
                print(statename)
                tempstatematrix<-summarylist[i]               
                tempstatematrix<-matrix(unlist(tempstatematrix),ncol=2,byrow=F)
                tempvalues<-tempstatematrix[,2]
                temphospital<-tempstatematrix[,1]
                tempvalues<-as.numeric(tempvalues)
                temphospital<-temphospital[order(tempvalues)]
                tempvalues<-tempvalues[order(tempvalues)]
                hospital=temphospital[num]
                stateanswer<-rbind(stateanswer,statename)
                hospitalanswer<-rbind(hospitalanswer,hospital)
               
                colnames(answermatrix) <- c("hospital", "score")
                r <- order(answermatrix$score,answermatrix$hospital)
                sorted <- answermatrix[r,]
                
                
                
        }
        
        ##take the list with the summary of answers and turn into data frame
        
        answer<-cbind(hospitalanswer,stateanswer)
        print(answer)
        

}

answerdataframe <- data.frame(matrix(unlist(summarylist), nrow=numstates, byrow=T))
##determine hospital selected in num
if (num=="worst") {
        num<-nrow(sorted)
}
print(answer)
rankall("heart attack")