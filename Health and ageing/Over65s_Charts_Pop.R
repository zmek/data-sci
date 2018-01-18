#---
#title: "Analysis of over 65s in South East England - processing population data"
#author: "Zella King"
#---

#This programme takes the population data and creates charts from it. Each is a tiff file printed to the working directory

setwd("~/GitHubRepos/data-sci/Health and ageing")

#install packages
library(ggplot2)
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)



### --- plot overall growth in over 65 pop in the South East ---  ###
a<-pop[ categ_total == 1 & district=="South East"]
a<-a[,y2017:y2035]
names(a)<-(gsub("y", "", names(a))) #remove y from col names to make plot easier
b<-data.frame(year = as.numeric((names(a))), pop = round((as.numeric(a[1,]))/1000000,2))

#print plot to tiff file
tiff('SEgrowth.tiff', units="in", width=8, height=3, res=100, compression = 'lzw')
p<-ggplot(data=b, aes(x=year, y=pop)) +
        geom_bar(stat="identity", fill=alpha("skyblue", 0.7), width = .7) + 
        labs(title="Growth in number of people over 65 in the South East", x="Year", y = "Population (millions)") +
        geom_text(aes(label=pop), vjust=1.6, size=3) +
        theme_minimal() 
p
dev.off()

### --- plot growth in over 65 pop in the South East divided into age categories --- ###
c<-data.frame(poplong_notot)
c$pop<-c$pop/1000

#print plot to tiff file
tiff('SEgrowthbyage.tiff', units="in", width=8, height=3, res=100, compression = 'lzw')
p<-ggplot(data=c, aes(x=year, y=pop, fill=age)) +
        geom_bar(stat="identity",  width = .7) + 
        labs(title="Growth in number of people over 65 in the South East, by age", x="Year", y = "Population (thousands)") +
        theme_minimal()
p
dev.off()

### ---plot CAGR for years 2025 and 2035 --- ###
d<-pop[ district=="South East"]
d<-d[,age:cagr2025]
d$cagr2025<-d$cagr2025*100
d$cagr2035<-d$cagr2035*100
label_text<-paste(round(d$cagr2025,2),"%", sep = "")
label_text2<-paste(round(d$cagr2035,2),"%", sep = "")

#print plot to tiff file
tiff('SE_CAGR2025.tiff', units="in", width=4, height=6, res=100, compression = 'lzw')
p<-ggplot(data=d, aes(x=age, y=cagr2025, fill = age)) +
        geom_bar(stat="identity",  width = .5) + 
        labs(title="CAGR to 2025 (%)", y = "CAGR (%)") +
        theme_minimal() + theme(legend.position = "none") + 
        geom_text(aes(label=label_text), hjust=1.2, size=3)
p + coord_flip()
dev.off()

tiff('SE_CAGR2035.tiff', units="in", width=4, height=6, res=100, compression = 'lzw')
p<-ggplot(data=d, aes(x=age, y=cagr2035, fill = age)) +
        geom_bar(stat="identity",  width = .5) + 
        labs(title="CAGR to 2035 (%)", y = "CAGR (%)") +
        theme_minimal() + theme(legend.position = "none") + 
        geom_text(aes(label=label_text2), hjust=1.2, size=3)

p + coord_flip()
dev.off()


### --- circular barplot for raw pop numbers by district --- ###

#make dataframe with raw pop numbers
pop_onlytot_nocounty<-pop_onlytot_nocounty[order(-y2021)] #order by relevant field - do this every time
plotdata=data.frame( 
        id<-seq(1,nrow(pop_onlytot_nocounty)),
        district<-pop_onlytot_nocounty$district,
        y2021_orig<-pop_onlytot_nocounty$y2021,      
        y2021<-pop_onlytot_nocounty$y2021/max(pop_onlytot_nocounty$y2021)*100,
        topten<-factor(c(rep(0,10),rep(1,nrow(pop_onlytot_nocounty)-10))))
colnames(plotdata)<-c("id","district","y2021_orig","y2021","topten")

#create dataset for labels
label_data=plotdata[,1:2]
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

#add new CAGR labels to the label data
label_data$district<-as.factor(paste(as.character(plotdata$district)," (",round(plotdata$y2021_orig/1000,0),")",sep=""))

#print to tiff file
tiff('District_poptotals.tiff', units="in", width=5, height=5, res=100, compression = 'lzw')
p = ggplot(plotdata, aes(x=as.factor(id), y=y2021, fill = topten)) +                                         geom_bar(stat="identity") + ylim(-100,200) + 
        theme_minimal() +
        theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), plot.margin = unit(rep(-1,4), "cm")) +
        coord_polar(start = 0) +
        theme(legend.position = "none") +
        geom_text(data=label_data, aes(x=id, y=y2021+10, label=district, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p
dev.off()

### --- circular barplot for CAGR2025 by district --- ###

#edit dataset for plotting CAGR - NB without changing order for this plot
plotdata$cagr2025_orig<-pop_onlytot_nocounty$cagr2025
plotdata$cagr2025<-pop_onlytot_nocounty$cagr2025/max(pop_onlytot_nocounty$cagr2025)*100
colnames(plotdata)<-c("id","district","y2021_orig","y2021","topten","cagr2025_orig","cagr2025")

#to calc top ten cagr without disturbing ordering of this data table
temp<-copy(pop_onlytot_nocounty)
temp<-temp[order(-cagr2025)] #first reorder temp DT in order of CAGR
temp<-temp[,toptenCAGR2025:=factor(c(rep(0,10),rep(1,nrow(pop_onlytot_nocounty)-10)))] #create new top ten factor in the correct order
temp<-temp[order(-y2021)] #put back into original order

#write to plot data
plotdata$toptenCAGR<-temp$toptenCAGR2025
rm(temp)

#add new CAGR labels to the label data
label_data$district<-as.factor(paste(as.character(plotdata$district)," (",round(plotdata$cagr2025_orig*100,1),"%)",sep=""))
cagr2025<-plotdata$cagr2025 #?? not sure why this plot which is same as above doesn't work without this

#print to tiff file
tiff('District_CAGR2025.tiff', units="in", width=5, height=5, res=100, compression = 'lzw')
p = ggplot(plotdata, aes(x=as.factor(id), y=cagr2025, fill = toptenCAGR)) +                                         geom_bar(stat="identity") + ylim(-100,200) + 
        theme_minimal() +
        theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), plot.margin = unit(rep(-1,4), "cm")) +
        coord_polar(start = 0) +
        theme(legend.position = "none") +
        geom_text(data=label_data, aes(x=id, y=cagr2025+10, label=district, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p
dev.off()
