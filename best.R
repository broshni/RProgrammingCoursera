## this function takes intial of a state(eg. TX) and a particular outcome among Heart Attack, Heart Failure and Pneumonia
## and Return hospital name in that state with lowest 30-day death or mortality rate.

best <- function(state, outcome) {
       
        ## reads the outcome-of-care-measures.csv file
        outcome_data <- read.csv("./ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
       
        if(!state %in% outcome_data$State){
                stop("invalid state")
        }else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome")
        }else {
                ##Subset the data based on state
                state_wise<-subset(outcome_data, outcome_data$State==state)
                
                #change the class of the Mortality columns to numeric for proper calculation
                state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(as.character(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(as.character(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(as.character(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                
                
                ## Return hospital name in that state with lowest 30-day death rate
                
                #Sorting option
                #sorting<-state_wise[order(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                #sorting[,c(2,7,11)]
                
                ##calculation based on state
                if(outcome=="heart attack"){
                        #try with order() to sort the value and hospital name alphabetically
                        rowidx<-order(state_wise[,11], state_wise[,2])
                        Final_sort<-state_wise[rowidx, c(2,11), drop=FALSE]
                        #minimun<-state_wise[which.min(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                        #Name_of_Hospital<- state_wise$Hospital.Name[which.min(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]
                        
                }else if(outcome=="heart failure"){
                        #try with order() to sort the value and hospital name alphabetically
                        rowidx<-order(state_wise[,17], state_wise[,2])
                        Final_sort<-state_wise[rowidx, c(2,17), drop=FALSE]
                        #minimun<-state_wise[which.min(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                        #Name_of_Hospital<- state_wise$Hospital.Name[which.min(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]
                        
                }else if(outcome=="pneumonia"){
                        #try with order() to sort the value and hospital name alphabetically
                        rowidx<-order(state_wise[,23], state_wise[,2])
                        Final_sort<-state_wise[rowidx, c(2,23), drop=FALSE]
                        #minimun<-state_wise[which.min(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                        #Name_of_Hospital<- state_wise$Hospital.Name[which.min(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)]
                }
                
               
                #get the name of the hospital with min mortality rate
                Final_sort[1,]
               
                #Name_of_Hospital
                
                #get the row with lowest 30 day death
                #minimun<-state_wise[which.min(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                #return name of the hospital directly
                #Name_of_Hospital<- state_wise$Hospital.Name[which.min(state_wise$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]
                
               
                
                
        }
         
       #Final_sort[1,1]
}


