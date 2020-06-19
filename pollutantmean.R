#Data contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter
#(PM) air pollution at 332 locations in the United States.
#Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. 
#For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:
#Date: the date of the observation in YYYY-MM-DD format (year-month-day)
#sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
#nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)


#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
#across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant',
#and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the
#directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, 
#ignoring any missing values coded as NA. 



pollutantmean<- function(directory, pollutant, id=1:332){
        #setwd(directory)
        
        ##Creates the master dataframe to contain all the data for final calculation
        masterdf<-data.frame()
        for(i in id){
                #print(i)
                ##Create the link for csv file reading
                link<-paste(directory,"/", formatC(i, width=3, flag="0"), ".csv", sep="")
                df<- read.csv(link)
                
                
                ##concatanate the data to the masterdf
                masterdf<-rbind(masterdf, df)
                
               
        }
        #masterdf
        
        ##Calculate the mean based on pollutant
        if(pollutant=="sulfate"){
                mean_col<-mean(masterdf$sulfate, na.rm=TRUE)
        } else if(pollutant=="nitrate"){
                mean_col<-mean(masterdf$nitrate, na.rm=TRUE)
        }
       print(mean_col) 
}

#ColMean<-pollutantmean("specdata", "sulfate", id=1:10)
#ColMean<- pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)
