## this function takes  a particular outcome among Heart Attack, Heart Failure and Pneumonia
## and a rank or num. The num argument can take in "best","worst" or interger i.e a particular rank
## and Return hospital name in every state with that rank based on 30-day death or mortality rate.
## should not call rankhospital function for the assignment.

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("./ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses = "character")
        
        ##define the storing vectors
        hospital<-vector()
        state<-vector()
        iter<-1
        
        ## Check that state and outcome are valid
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome")
        }else {
                state_wise<-split(outcome_data, outcome_data$State)
                for(i in 1:length(state_wise)){
                        #print(i)
                        ##add the state name for output
                        state[iter]<-names(state_wise[i])
                        
                        ##prepare the df for calculation
                        df_state<-state_wise[i]
                        df_state<-df_state[[names(state_wise[i])]]
                        df_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(as.character(df_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                        df_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(as.character(df_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                        df_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(as.character(df_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                        
                        #consider the outcome
                        if(outcome=="heart attack"){
                                #try with order() to sort the value and hospital name alphabetically
                                rowidx<-order(df_state[,11], df_state[,2])
                                Final_sort<-df_state[rowidx, c(2,11), drop=FALSE]
                                
                                
                        }else if(outcome=="heart failure"){
                                #try with order() to sort the value and hospital name alphabetically
                                rowidx<-order(df_state[,17], df_state[,2])
                                Final_sort<-df_state[rowidx, c(2,17), drop=FALSE]
                               
                                
                        }else if(outcome=="pneumonia"){
                                #try with order() to sort the value and hospital name alphabetically
                                rowidx<-order(df_state[,23], df_state[,2])
                                Final_sort<-df_state[rowidx, c(2,23), drop=FALSE]
                        
                        
                        }
                        #Calculate the rank
                        rank_h<-rank(Final_sort[,2], ties.method= "first") 
                        Final_sort$Rank<-rank_h
                        Final_sort<-na.omit(Final_sort)
                        
                        #extract and return the hospital name
                        if(num=="best"){
                                h_name<-Final_sort$Hospital.Name[Final_sort$Rank==1]
                        }else if(num=="worst"){
                                h_name<-Final_sort$Hospital.Name[Final_sort$Rank==length(Final_sort$Rank)]
                        }else if(num<=length(Final_sort$Rank)){
                                h_name<-Final_sort$Hospital.Name[Final_sort$Rank==num] 
                        }else{
                                h_name<-"NA"
                        }
                        
                        #update output vector with hospital name
                        hospital[iter]<-h_name
                        iter<-iter+1
                }
                
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
                df_output<-data.frame(hospital,state)
                
        }
       #df_output 
       
}