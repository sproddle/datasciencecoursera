##best.R
## takes two arguments: the 2-character abbreviated name of a state and an outcome name. 
##The outcomes can be one of "heart attack", "heart failure", or "pneumonia".
## The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state.

best <- function(state, outc) {
      ## Read outcome data
      ## cols are:
      ## [2] "Hospital.Name"  
      ## [7] "State"  
      ## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"    
      ## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
      ## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
      
      ## Read outcome data
      ## Convert to numeric
      ## Clean up data
      outcome[outcome=="Not Available"|outcome=="NA"]<-NA
      df <- data.frame( outcome[,2] , outcome[,7],as.numeric(outcome[,11]),as.numeric(outcome[,17]),as.numeric(outcome[,23]))
      
      names(df)<-c("hospital", "state","heart_attack", "heart_failure",  "pneumonia")
      df[df=="Not Available"|df=="NA"]<-NA
      ## Replace space with _
      outc <- gsub('([[:punct:]])|\\s+','_',outc)
     
      
      
      ## Check that state and outcome are valid
      if ( sum(df[,2] == state)==0) {
            message("Invalid state")
            return(NULL) 
      }
      
      if ( sum(colnames(df)[3:5] == outc)==0) {
            message("Invalid outcome")
            return(NULL) 
      }
      
    
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
     
      
      ret_min <- function( state, colname)
            {
            colname <- paste("hosp$", colname,sep = "")
            hosp <- df[df$state==state,]
            sorted <-hosp[order(eval(parse(text =(colname))),hosp$hospital), ]
            sorted [1,1]
         
      }
      
      a<- ret_min(state,outc)
      toString(a)
      
      
}
