#Data contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter
#(PM) air pollution at 332 locations in the United States.
#Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. 
#For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:
#Date: the date of the observation in YYYY-MM-DD format (year-month-day)
#sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
#nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

#Write a function that reads a directory full of files and reports the number of completely observed cases in each data 
#file. The function should return a data frame where the first column is the name of the file and the second column is
#the number of complete cases



complete <- function(directory, id =1:332){
        nobs<-numeric(length(id))
        iter=1  ##iterator for nobs
        for(i in id){
                #print(i)
                ##Create the link for csv file reading
                link<-paste(directory,"/", formatC(i, width=3, flag="0"), ".csv", sep="")
                df<- read.csv(link)
                ## omit rows with missiing values
                result<-na.omit(df)
                #final[complete.cases(final), ] allows partial selection
                
                ##Add to the nobs vector number of rows after removing missing value
                nobs[iter]<-nrow(result)
                iter=iter+1
        }
        ## Construct the final dataframe 
        df<- data.frame(id , nobs)
        #print(df)
}

#complete("specdata", 1)

#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 30:25)