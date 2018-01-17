#---
#title: "Analysis of over 65s in South East England - processing population data"
#author: "Zella King"
#---

#This programme prepares the population data already downloaded from the POPPI website 

setwd("~/GitHubRepos/data-sci/Health and ageing")

#install packages
library(data.table)

#get names all downloaded files - NOTE change directory to correct path
filenames<-list.files(path="~/Google Drive/Datasets/POPPI downloads 180115", full.names = TRUE)

#read all downloaded files into two dataframes
popnext5<-data.frame()
popto2035<-data.frame()
for (i in 1:length(filenames)) {
        a<-read.csv(filenames[i],stringsAsFactors = FALSE, skip = 3)
        a<-head(a, -4) #remove last four rows 
        if(names(a[3])=="X2018") {
                popnext5<-rbind(popnext5,a)  
        }
        else {
                popto2035<-rbind(popto2035,a)
        }
        
}

colnames(popnext5)<-c("description","y2017","y2018","y2019","y2020","y2021")
colnames(popto2035)<-c("description","y2017","y2020","y2025","y2030","y2035")

#remove duplicated rows and merge the two datasets into one called pop
popnext5<-popnext5[!duplicated(popnext5),]
popto2035<-popto2035[!duplicated(popto2035),]
pop<-merge(popnext5,popto2035,by = c("description","y2017","y2020"))
rm(popnext5); rm(popto2035)

#parse the description into separate fields
a<-strsplit(pop$description,":")
b<-matrix(data = unlist(a),ncol = 2, byrow = TRUE); rm(a)
b<-as.data.frame(b)

#make data table with desired fields
pop<-data.table(pop,district = b$V1,age=b$V2); rm(b)
pop<-pop[,y2017:age]

#remove commas in numeric fields
pop$y2017<-as.numeric(gsub(",", "", pop$y2017))
pop$y2018<-as.numeric(gsub(",", "", pop$y2018))
pop$y2019<-as.numeric(gsub(",", "", pop$y2019))
pop$y2020<-as.numeric(gsub(",", "", pop$y2020))
pop$y2021<-as.numeric(gsub(",", "", pop$y2021))
pop$y2025<-as.numeric(gsub(",", "", pop$y2025))
pop$y2030<-as.numeric(gsub(",", "", pop$y2030))
pop$y2035<-as.numeric(gsub(",", "", pop$y2035))

#calculate CAGR - NOTE hardcoding the year interval
pop$cagr2035<-(pop$y2035/pop$y2017)^(1/18)-1
pop$cagr2025<-(pop$y2025/pop$y2017)^(1/8)-1

#identify the county level categories by looking for county names in the names of the downloaded files
z<-gregexpr("Population_by_age_",filenames[1],fixed = TRUE)[[1]]
y<-nchar("Population_by_age_")
tempdate<-substr(filenames,z[[1]]+y,z[[1]]+y+8) #use download date to split filenames 
#tempdate<-substr(filenames,z[[1]]+18,z[[1]]+26) #delete this once checked
a<-strsplit(filenames,tempdate)
b<-matrix(data = unlist(a),ncol = 2, byrow = TRUE)
c<-strsplit(b[,2],"_and")
d<-matrix(data = unlist(c),ncol = 2, byrow = TRUE)
county_names<-unique(d[,1])
county_names<-sub("_"," ", county_names)
pop<-pop[,county_level:=pop$district %in% county_names]

#add a column to pop identifying which rows are totals of each district or country
pop<-pop[,categ_total:=grepl("Total",pop$age)]

##Creating other versions of the data to use in charts

#make a version with only the totals for each district
pop_onlytot<-pop[ categ_total==1]
pop_onlytot<-pop_onlytot[,y2017:county_level]

#take subset without totals or county level data
pop_onlytot_nocounty<-copy(pop_onlytot)
pop_onlytot_nocounty<-pop_onlytot_nocounty[ county_level==0]

#make long thin dataset - note this includes county level and a summary for the South East
poplong<-melt(pop, id.vars = c("district","age","county_level", "categ_total"), measure.vars = c("y2017","y2018","y2019","y2020","y2021","y2025","y2030","y2035"))
colnames(poplong)<-c("district","age","county_level","categ_total", "year","pop")
poplong$year<-as.numeric(gsub("y", "", poplong$year)) #remove y from col names to make plot easier

#make another version without counties or South East
poplong_notot<-poplong[ county_level!=1 & categ_total!=1]
poplong_notot<-poplong_notot[, c("county_level","categ_total"):=NULL] #remove columns that are no longer needed
