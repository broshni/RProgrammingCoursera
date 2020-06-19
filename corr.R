#Data contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter
#(PM) air pollution at 332 locations in the United States.
#Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. 
#For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:
#Date: the date of the observation in YYYY-MM-DD format (year-month-day)
#sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
#nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

#Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation
#between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is 
#greater than the threshold. The function should return a vector of correlations for the monitors that meet the 
#threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric
#vector of length 0.



corr<- function(directory, threshold=0){
        ## get the number of complete cases by removing missing values
        complete_cases<-complete(directory)
        
        ## get the subset of the complete cases are greater than the threshold
        subset_complete <- subset(complete_cases, complete_cases$nobs>threshold)
        
        ##Creates the vector to contain all the data for final correlations
        correlation<-vector()
        
        for(i in subset_complete$id){
                #print(i)
                ##Create the link for csv file reading
                link<-paste(directory,"/", formatC(i, width=3, flag="0"), ".csv", sep="")
                df<- read.csv(link)
                df_omit_na<- na.omit(df)
                
                ##calculate correalation
                cor_vector<-cor(df_omit_na$sulfate, df_omit_na$nitrate)
                
                correlation<-c(correlation,cor_vector)
                
                ##concatanate the data to the masterdf
                ##masterdf<-rbind(masterdf, df)
               
                
        }
        
        return(correlation)
        
}

#cr <- corr("specdata", 150)
#cr <- corr("specdata", 400)
#cr <- corr("specdata", 5000)
#cr <- corr("specdata")
