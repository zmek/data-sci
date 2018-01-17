#---
#title: "Analysis of over 65s in South East England - processing health data"
#author: "Zella King"
#---

#This programme prepares the health data already downloaded from the POPPI website 

setwd("~/GitHubRepos/data-sci/Health and ageing")

#install packages
library(data.table)

#get names all downloaded files - NOTE change directory to correct path
filenames<-list.files(path="~/Google Drive/Datasets/POPPI downloads 180116", full.names = TRUE)

#read all downloaded files into two dataframes
healthnext5<-data.frame()
healthto2035<-data.frame()
for (i in 1:length(filenames)) {
        a<-read.csv(filenames[i],stringsAsFactors = FALSE, skip = 3)
        a<-head(a, -3) #remove last four rows 
        if(names(a[3])=="X2018") {
                healthnext5<-rbind(healthnext5,a)  
        }
        else {
                healthto2035<-rbind(healthto2035,a)
        }
        
}

colnames(healthnext5)<-c("description","y2017","y2018","y2019","y2020","y2021")
colnames(healthto2035)<-c("description","y2017","y2020","y2025","y2030","y2035")

#remove duplicated rows and merge the two datasets into one called health
healthnext5<-healthnext5[!duplicated(healthnext5),]
healthto2035<-healthto2035[!duplicated(healthto2035),]
health<-merge(healthnext5,healthto2035,by = c("description","y2017","y2020"))
rm(healthnext5); rm(healthto2035)

#parse the description into separate fields
a<-strsplit(health$description,":")
b<-matrix(data = unlist(a),ncol = 2, byrow = TRUE)
c<-strsplit(b[,2]," whose day-to-day activities are limited ")
d<-matrix(data = unlist(c),ncol = 2, byrow = TRUE)
b<-as.data.frame(b); d<-as.data.frame(d)

#make data table with desired fields
health<-data.table(health,district = b$V1,Limited=gsub("a l","A l",d$V2),age=d$V1)
health<-health[,y2017:age]

#remove commas in numeric fields
health$y2017<-as.numeric(gsub(",", "", health$y2017))
health$y2018<-as.numeric(gsub(",", "", health$y2018))
health$y2019<-as.numeric(gsub(",", "", health$y2019))
health$y2020<-as.numeric(gsub(",", "", health$y2020))
health$y2021<-as.numeric(gsub(",", "", health$y2021))
health$y2025<-as.numeric(gsub(",", "", health$y2025))
health$y2030<-as.numeric(gsub(",", "", health$y2030))
health$y2035<-as.numeric(gsub(",", "", health$y2035))

#calculate CAGR - NOTE hardcoding the year interval
health$cagr2035<-(health$y2035/health$y2017)^(1/18)-1
health$cagr2025<-(health$y2025/health$y2017)^(1/8)-1

#identify the county level categories by looking for county names in the names of the downloaded files
z<-gregexpr("Limiting_long_term_illness_",filenames[1],fixed = TRUE)[[1]]
y<-nchar("Limiting_long_term_illness_")
tempdate<-substr(filenames,z[[1]]+y,z[[1]]+y+8) #use download date to split filenames 
a<-strsplit(filenames,tempdate)
b<-matrix(data = unlist(a),ncol = 2, byrow = TRUE)
c<-strsplit(b[,2],"_and")
d<-matrix(data = unlist(c),ncol = 2, byrow = TRUE)
county_names<-unique(d[,1])
county_names<-sub("_"," ", county_names)
health<-health[,county_level:=health$district %in% county_names]

#add a column to health identifying which rows are totals of each district or country
health<-health[,categ_total:=grepl("Total",health$age)]

##Creating other versions of the data to use in charts

#make a version with only the totals for each district
health_onlytot<-health[ categ_total==1]
health_onlytot<-health_onlytot[,y2017:county_level]

#take subset without totals or county level data
health_onlytot_nocounty<-copy(health_onlytot)
health_onlytot_nocounty<-health_onlytot_nocounty[ county_level==0]

#make long thin dataset - note this includes county level and a summary for the South East
healthlong<-melt(health, id.vars = c("district","age","Limited","county_level", "categ_total"), measure.vars = c("y2017","y2018","y2019","y2020","y2021","y2025","y2030","y2035"))
colnames(healthlong)<-c("district","Age","Limited","county_level","categ_total", "year","pop")
healthlong$year<-as.numeric(gsub("y", "", healthlong$year)) #remove y from col names to make plot easier

#make another version without counties or South East
healthlong_notot<-healthlong[ county_level!=1 & categ_total!=1]
healthlong_notot<-healthlong_notot[, c("county_level","categ_total"):=NULL] #remove columns that are no longer needed
