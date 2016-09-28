BearSumFunc <- function(data, summary=FALSE, output.csv = NULL, type="proximity"){
  library(dplyr)
  library(mosaic)##Required Libraries
  sampobs<-read.csv(data) ##Reading in data
  
  #Gender: Sex = 204.25 -> Male, Sex= 250.25 ->Female
  sampobs$Group<-rep("M",nrow(sampobs))
  sampobs$Group[sampobs$Sex==250.25]<-"F" 
  
  #Create a data set that contains a count of the number of times each bear
  #was seen for each unique site x period combination.  Also determine
  #sex of each individual and tabulate the number of males and femlaes
  caphist<- sampobs%>%group_by(Individual, site, Period)%>%
    summarize(Count= n())
  sexid<-unique(select(sampobs, Individual, Group))
  table(sexid$Group)
  caphist2<-merge(caphist, sexid, all=FALSE)

  
  ##optional summary output - TODO: Verify output with john's, something looks off 
  if(summary==TRUE){
    sum.out<-matrix(NA, 3, 4)
    rownames(sum.out)<- c("Same bear, site, period", "n detections per site & period", "Unique bears at each site")
    colnames(sum.out)<- c("Max", "Mean", "sd", "n")
    
    capnums<-caphist%>%group_by(site, Period)%>% summarize(ncap=n() )
    timecap<-caphist%>%group_by(Period)%>% summarize(nbears=n_distinct(Individual) )
    sitecap<-caphist%>%group_by(site)%>% summarize(nbears=n_distinct(Individual) )
    
    sum.out[1,]<-as.double(favstats(caphist$Count)[5:8]) #same bear, site, period
    sum.out[2,]<-as.double(favstats(capnums$ncap)[5:8]) #n detections per site&period
    sum.out[3,]<-as.double(favstats(sitecap$nbears)[5:8]) #n detections per site&period
    
    outputsummary<-list(Captures.by.Period=timecap, Summary=sum.out)
    print(outputsummary)
  }
  
  #Write out proximity detector (if a path is set, write csv, otherwise just output the data frame itself)
  bearCH<-data.frame(Session="BearMR", ID=caphist$Individual, Occassion=caphist2$Period, Detector=caphist2$site, Sex=caphist2$Group)
  bearCHP<-data.frame(Session="BearMR", ID=sampobs$Individual, Occassion=sampobs$Period, Detector=sampobs$site, Sex=sampobs$Group)
  # names(bearCH)<-c("Session", "ID", "Occasion", "Detector", "Count", "Sex") ##
   if (!(is.null(output.csv))){
             if (type=="proximity"){
              write.table(bearCH, file=output.csv, row.names=FALSE, col.names=FALSE, sep=",")
             } else if (type=="multi"){
               write.table(bearCHP, file=output.csv, row.names=FALSE, col.names=FALSE, sep=",")
             } else print("Unrecognized detection type")
   }
  
    if (is.null(output.csv)){
            if (type=="proximity"){
              return(bearCH)
            } else if (type=="multi"){
              return(bearCHP)
            } else print("Unrecognized detection type")
  }
}