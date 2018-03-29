#---
#title: "Analysis of over 65s in South East England - creating charts from health data"
#author: "Zella King"
#---

#This programme takes the health data and creates charts from it. Each is a tiff file printed to the working directory

setwd("~/GitHubRepos/data-sci/Health and ageing")

#install packages
library(ggplot2)
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)



### --- plot growth in illness over 65 pop in the South East --- ###
a<-data.frame(healthlong_notot)
a$pop<-a$pop/1000

#print plot to tiff file
tiff('SEgrowth_illness.tiff', units="in", width=8, height=3, res=100, compression = 'lzw')
p<-ggplot(data=a, aes(x=year, y=pop, fill = Limited)) +
        geom_bar(stat="identity", width = .7) + 
        labs(title="Growth in number of people over 65 limited by illness in the South East", x="Year", y = "Population (thousands)") +
        theme_minimal() 
p
dev.off()

### --- plot growth in illness over 65 pop in the South East by age  --- ###


#print plot to tiff file
tiff('SEgrowth_illnessbyage.tiff', units="in", width=8, height=3, res=100, compression = 'lzw')
p<-ggplot(data=a, aes(x=year, y=pop, fill=Age)) +
        geom_bar(stat="identity",  width = .7) + 
        labs(title="Growth in number of people over 65 limited by illness in the South East, by age", x="Year", y = "Population (thousands)") +
        theme_minimal()
p
dev.off()



### --- circular barplot for raw pop numbers - limited a little - by district --- ###

#make dataframe with raw pop numbers 
b<-copy(health_onlytot_nocounty)
b<-b[ Limited == "A little"]
b<-b[order(-y2021)] #order by relevant field - do this every time
plotdata=data.frame( 
        id<-seq(1,nrow(b)),
        district<-b$district,
        y2021_orig<-b$y2021,      
        y2021<-b$y2021/max(b$y2021)*100,
        topten<-factor(c(rep(0,10),rep(1,nrow(b)-10))))
colnames(plotdata)<-c("id","district","y2021_orig","y2021","topten")

#create dataset for labels
label_data=plotdata[,1:2]
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

#add new CAGR labels to the label data
label_data$district<-as.factor(paste(as.character(plotdata$district)," (",round(plotdata$y2021_orig/1000,1),"K)",sep=""))

#print to tiff file
tiff('District_limitedalittle_totals.tiff', units="in", width=5, height=5, res=100, compression = 'lzw')
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
plotdata$cagr2025_orig<-b$cagr2025
plotdata$cagr2025<-b$cagr2025/max(b$cagr2025)*100
colnames(plotdata)<-c("id","district","y2021_orig","y2021","topten","cagr2025_orig","cagr2025")

#to calc top ten cagr without disturbing ordering of this data table
temp<-copy(b)
temp<-temp[order(-cagr2025)] #first reorder temp DT in order of CAGR
temp<-temp[,toptenCAGR2025:=factor(c(rep(0,10),rep(1,nrow(b)-10)))] #create new top ten factor in the correct order
temp<-temp[order(-y2021)] #put back into original order

#write to plot data
plotdata$toptenCAGR<-temp$toptenCAGR2025
rm(temp)

#add new CAGR labels to the label data
label_data$district<-as.factor(paste(as.character(plotdata$district)," (",round(plotdata$cagr2025_orig*100,1),"%)",sep=""))
cagr2025<-plotdata$cagr2025 #?? not sure why this plot which is same as above doesn't work without this

#print to tiff file
tiff('District__limitedalittle_CAGR2025.tiff', units="in", width=5, height=5, res=100, compression = 'lzw')
p = ggplot(plotdata, aes(x=as.factor(id), y=cagr2025, fill = toptenCAGR)) +                                         geom_bar(stat="identity") + ylim(-100,200) + 
        theme_minimal() +
        theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), plot.margin = unit(rep(-1,4), "cm")) +
        coord_polar(start = 0) +
        theme(legend.position = "none") +
        geom_text(data=label_data, aes(x=id, y=cagr2025+10, label=district, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p
dev.off()



### --- circular barplot for raw pop numbers - combining limited and a lot - by district --- ###

#make dataframe with raw pop numbers 
e<-copy(health_onlytot_nocounty_nolimited)
e<-e[order(-y2021)] #order by relevant field - do this every time
plotdata=data.frame( 
        id<-seq(1,nrow(e)),
        district<-e$district,
        y2021_orig<-e$y2021,      
        y2021<-e$y2021/max(e$y2021)*100,
        topten<-factor(c(rep(0,10),rep(1,nrow(b)-10))))
colnames(plotdata)<-c("id","district","y2021_orig","y2021","topten")

#create dataset for labels
label_data=plotdata[,1:2]
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

#add new CAGR labels to the label data
label_data$district<-as.factor(paste(as.character(plotdata$district)," (",round(plotdata$y2021_orig/1000,1),")",sep=""))

#print to tiff file
tiff('District_healthtotals.tiff', units="in", width=6, height=6, res=100, compression = 'lzw')
p = ggplot(plotdata, aes(x=as.factor(id), y=y2021, fill = topten)) +                                         geom_bar(stat="identity") + ylim(-100,200) + 
        theme_minimal() +
        scale_fill_manual(values = c("1" = "blue", "0" = "orange")) +
        theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), plot.margin = unit(rep(-1,4), "cm")) +
        coord_polar(start = 0) +
        theme(legend.position = "none") +
        geom_text(data=label_data, aes(x=id, y=y2021+10, label=district, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p
dev.off()



### --- circular barplot for CAGR2025 by district - combining limited and a lot - --- ###

#edit dataset for plotting CAGR - NB without changing order for this plot
plotdata$cagr2025_orig<-e$cagr2025
plotdata$cagr2025<-e$cagr2025/max(e$cagr2025)*100
colnames(plotdata)<-c("id","district","y2021_orig","y2021","topten","cagr2025_orig","cagr2025")

#to calc top ten cagr without disturbing ordering of this data table
temp<-copy(e)
temp<-temp[order(-cagr2025)] #first reorder temp DT in order of CAGR
temp<-temp[,toptenCAGR2025:=factor(c(rep(0,10),rep(1,nrow(b)-10)))] #create new top ten factor in the correct order
temp<-temp[order(-y2021)] #put back into original order

#write to plot data
plotdata$toptenCAGR<-temp$toptenCAGR2025
rm(temp)

#add new CAGR labels to the label data
label_data$district<-as.factor(paste(as.character(plotdata$district)," (",round(plotdata$cagr2025_orig*100,1),"%)",sep=""))
cagr2025<-plotdata$cagr2025 #?? not sure why this plot which is same as above doesn't work without this

#print to tiff file
tiff('District_healthCAGR2025.tiff', units="in", width=6, height=6, res=100, compression = 'lzw')
p = ggplot(plotdata, aes(x=as.factor(id), y=cagr2025, fill = toptenCAGR)) +                                         geom_bar(stat="identity") + ylim(-100,200) + 
        theme_minimal() +
        scale_fill_manual(values = c("1" = "blue", "0" = "orange")) +
        theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), plot.margin = unit(rep(-1,4), "cm")) +
        coord_polar(start = 0) +
        theme(legend.position = "none") +
        geom_text(data=label_data, aes(x=id, y=cagr2025+10, label=district, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p
dev.off()
