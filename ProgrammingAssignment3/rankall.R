##best.R
## takes two arguments: the 2-character abbreviated name of a state and an outcome name. 
##The outcomes can be one of "heart attack", "heart failure", or "pneumonia".
## The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state.

rankall <- function(outc,num="best") {
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
      outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      outcome[outcome=="Not Available"|outcome=="NA"]<-NA
      df <- data.frame( outcome[,2] , outcome[,7],as.numeric(outcome[,11]),as.numeric(outcome[,17]),as.numeric(outcome[,23]))
      
      names(df)<-c("hospital", "state","heart_attack", "heart_failure",  "pneumonia")
      df[df=="Not Available"|df=="NA"]<-NA
      ## Replace space with _
      outc <- gsub('([[:punct:]])|\\s+','_',outc)
     
      ##DEFINE FUNCTION TO RETURN required VALUES
      ret_min <- function( state, colname, num)
      {
            colname <- paste("hosp$", colname,sep = "")
            hosp <- df[df$state==state,]
            hosp <-hosp[order(eval(parse(text =(colname))),hosp$hospital), ]
            a<- data.frame(hosp$hospital, eval(parse(text =(colname))))
            if ( num=="best" ) {
                  ret<-toString(a[1,1])
            }
            
            else if ( is.numeric(num) ){
                  ret<-toString(a[num,1])
                  
            }
            else if ( num=="worst" ) { n <- nrow(a)
            ret<-toString(a[n,1])
            
            }
      }
      ## Check that state and outcome are valid
    
      
      if ( sum(colnames(df)[3:5] == outc)==0) {
            message("Invalid outcome")
            return(NULL) 
            }
      if ( is.numeric(num) || num=="best" || num=="worst") {
            TRUE==TRUE}
            else
            {message("Invalid num")
            return(NULL) }
      
           
 
   ##LOOP Through each state
            
     us <- unique(df$state)
     
      for (st in sort(us)  )
      {
            
            hosp_name <- ret_min(st, outc, num)
            ##print (hosp_name)
            #rowa<- c(st,hosp_name)
            if (exists("df_res") ){
                  df_res<- rbind(df_res, data.frame(hosp_name,st))
            }
            else {df_res<- data.frame(hosp_name, st)

            
            }
            
            
     
      }
     names(df_res)<-c("hospital", "state")
      df_res
}    
      
 
