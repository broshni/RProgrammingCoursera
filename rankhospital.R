## this function takes intial of a state(eg. TX) and a particular outcome among Heart Attack, Heart Failure and Pneumonia
## and a rank or num. The num argument can take in "best","worst" or interger i.e a particular rank
## and Return hospital name in that state with that rank based on 30-day death or mortality rate.
#Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.Should not call the best Function

rankhospital<-function(state, outcome, num = "best" ){
        outcome_data <- read.csv("./ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        #2 %in% B$C
        if(!state %in% outcome_data$State){
                stop("invalid state")
        }else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome")
        }else {
                ##Subset the data based on state
                state_wise<-subset(outcome_data, outcome_data$State==state)
                
                #change the class of the Mortality columns
                state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(as.character(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(as.character(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(as.character(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                
                
               ##calculation based on outcome 
              
                if(outcome=="heart attack"){
                        #try with order() to sort the value and hospital name alphabetically
                        rowidx<-order(state_wise[,11], state_wise[,2])
                        Final_sort<-state_wise[rowidx, c(2,11), drop=FALSE]
                        
                        
                }else if(outcome=="heart failure"){
                        #try with order() to sort the value and hospital name alphabetically
                        rowidx<-order(state_wise[,17], state_wise[,2])
                        Final_sort<-state_wise[rowidx, c(2,17), drop=FALSE]
                        
                        
                }else if(outcome=="pneumonia"){
                        #try with order() to sort the value and hospital name alphabetically
                        rowidx<-order(state_wise[,23], state_wise[,2])
                        Final_sort<-state_wise[rowidx, c(2,23), drop=FALSE]
                        
                }
                ## Return hospital name in that state with the given rank
                ## for 30-day death rate
                
                #names(Final_sort)[2]<-Rate
                
                #Calculate the rank
                rank_h<-rank(Final_sort[,2], ties.method= "first") 
                Final_sort$Rank<-rank_h
                Final_sort<-na.omit(Final_sort)
                
                #extract and return the hospital name based on the num argument provided
                if(num=="best"){
                        h_name<-Final_sort$Hospital.Name[Final_sort$Rank==1]
                }else if(num=="worst"){
                        h_name<-Final_sort$Hospital.Name[Final_sort$Rank==length(Final_sort$Rank)]
                }else if(num<=length(Final_sort$Rank)){
                        h_name<-Final_sort$Hospital.Name[Final_sort$Rank==num] 
                }else{
                        return(NA)
                }
                
                
                
                
        }
        #Final_sort
        h_name
}