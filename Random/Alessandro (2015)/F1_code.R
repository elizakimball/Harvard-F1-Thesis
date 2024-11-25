############################ webscrapping 
library(rvest)
#install.packages("XML")
library(XML)
library(httr)
library(dplyr)

year = c(2015,2016,2017,2018,2019)

for (k in year) {
  if(k<2018){
    url_race_data <- paste("https://www.racefans.net/",k,"-f1-season/statistics/race-data/",sep = "")
    url_pit_data <- paste("https://www.racefans.net/",k,"-f1-season/statistics/strategy-pit-stops/",sep = "")
    url_penalties_data <- paste("https://www.racefans.net/",k,"-f1-season/statistics/retirements-penalties/",sep="")
    url_qualify_data <- paste("https://www.racefans.net/",k,"-f1-season/statistics/qualifying-data/",sep = "")
    url_points_data <- paste("https://www.racefans.net/",k,"-f1-season/statistics/championship-points/",sep = "")
  }
  else{
    url_race_data <- paste("https://www.racefans.net/",k,"-f1-season/",k,"-f1-statistics/",k,"-f1-race-data/",sep = "")
    url_pit_data <- paste("https://www.racefans.net/",k,"-f1-season/",k,"-f1-drivers-teams/",k,"-f1-strategy-pit-stops/",sep="")
    url_penalties_data <- paste("https://www.racefans.net/",k,"-f1-season/",k,"-f1-statistics/",k,"-f1-retirements-penalties/",sep = "")
    url_qualify_data <- paste("https://www.racefans.net/",k,"-f1-season/",k,"-f1-statistics/",k,"-f1-qualifying-data/",sep = "")
    url_points_data <- paste("https://www.racefans.net/",k,"-f1-season/",k,"-f1-statistics/",k,"-f1-championship-points/",sep = "")
  }
  page_race_data <- read_html(url_race_data) #reading the page as html
  tables_race <- html_table(page_race_data,header = FALSE) #extracting dataframes out of the html page race data
  View(tables_race)
  
  rownames(tables_race[[1]])<-tables_race[[1]][,1]
  tables_race[[1]]<-tables_race[[1]][,-1]
  names(tables_race[[1]])<-c("FirstPosition","SecondPosition","ThirdPosition")
  write.csv(tables_race[[1]][,1:3], paste("lapsperpos",k,".csv",sep = ""))
  
  names(tables_race[[2]])<-list("Driver","Started","Classified","Complete")
  rownames(tables_race[[2]])<-tables_race[[2]][,1]
  tables_race[[2]]<-tables_race[[2]][-1,-1]
  write.csv(tables_race[[2]], paste("started&finished",k,".csv",sep = ""))
  
  names(tables_race[[3]])<-list("Driver","Full seasons laps led", "Driver\'s season laps led")
  rownames(tables_race[[3]])<-tables_race[[3]][,1]
  tables_race[[3]]<-tables_race[[3]][-1,-1]
  for(i in 1:dim(tables_race[[3]])[1]){
    for(j in 1:2){
      tables_race[[3]][i,j]<-gsub(".*\\(","",tables_race[[3]][i,j])
      tables_race[[3]][i,j]<-as.numeric(gsub("\\%).*","",tables_race[[3]][i,j]))
    }
  }
  write.csv(tables_race[[3]], paste("lapsled",k,".csv",sep = ""))
  
  #strategy and pit stops
  page_pit_data <- read_html(url_pit_data) #reading the page as html
  tables_pit <- html_table(page_pit_data,header = FALSE,fill = TRUE) #extracting dataframes out of the html page race data
  View(tables_pit)
  
  names(tables_pit[[1]])<-tables_pit[[1]][1,]
  rownames(tables_pit[[1]])<-tables_pit[[1]][,1]
  tables_pit[[1]]<-tables_pit[[1]][-1,-1]
  tables_pit[[1]]["Average Pit Stop"] <- rowMeans(tables_pit[[1]],na.rm = TRUE) #data transformation step
  write.csv(tables_pit[[1]][,"Average Pit Stop"], paste("pitstop",k,".csv",sep = ""),row.names = rownames(tables_pit[[1]]))   
  
  if(k==2015){
    names(tables_pit[[3]])<-list("Driver","Hard","Medium","Soft","Super-soft","Wet","Intermediate")
  }
  else if(k > 2015 & k <= 2017){
    names(tables_pit[[3]])<-list("Driver","Hard","Medium","Soft","Super-soft","Ultra-soft","Wet","Intermediate")
  }
  else{
    names(tables_pit[[3]])<-list("Driver","Hard","Medium","Soft","Super-soft","Ultra-soft","Hyper-soft","Wet","Intermediate")
  }
  rownames(tables_pit[[3]])<-tables_pit[[3]][,1]
  tables_pit[[3]]<-tables_pit[[3]][,-1]
  for(i in 1:dim(tables_pit[[3]])[1]){
    for(j in 1:length(names(tables_pit[[3]]))){
      tables_pit[[3]][i,j]<-gsub(".*\\(","",tables_pit[[3]][i,j])
      tables_pit[[3]][i,j]<-as.numeric(gsub("\\%).*","",tables_pit[[3]][i,j]))
    }
  }
  write.csv(tables_pit[[3]], paste("tyres",k,".csv",sep=""))
  
  #penalties dataset
  page_penalties_data <- read_html(url_penalties_data) #reading the page as html
  tables_penalties <- html_table(page_penalties_data,header = FALSE) #extracting dataframes out of the html page race data
  View(tables_penalties)
  
  tables_penalties[[1]]<-data.frame(tables_penalties[[1]])
  names(tables_penalties[[1]])<-list("Driver","Accident")
  rownames(tables_penalties[[1]])<-tables_penalties[[1]][,1]
  tables_penalties[[1]]<-tables_penalties[[1]][,(-3:-8)]
  write.csv(tables_penalties[[1]][,2], paste("accident",k,".csv",sep = ""),row.names = rownames(tables_penalties[[1]]))
  
  names(tables_penalties[[2]])<-list("Driver","Penalties due to driver","Penalties due to team","No action")
  rownames(tables_penalties[[2]])<-tables_penalties[[2]][,1]
  write.csv(tables_penalties[[2]][,2:4], paste("penalties",k,".csv",sep = ""),row.names = rownames(tables_penalties[[2]]))
  
  #starting pole position
  page_qualify_data <- read_html(url_qualify_data) #reading the page as html
  tables_qualify <- html_table(page_qualify_data,header = FALSE) #extracting dataframes out of the html page race data
  View(tables_qualify)
  
  if(k==2015){
    names(tables_qualify[[3]])<-list("Driver","Average Pol Pos")
    rownames(tables_qualify[[3]])<-tables_qualify[[3]][,1]
    write.csv(tables_qualify[[3]][,2], paste("startingpolpos",k,".csv"),row.names = rownames(tables_qualify[[3]])) #have to delete rest of the data and only keep starting pol pos  
  }
  else{
    names(tables_qualify[[2]])<-list("Driver","Average Pol Pos")
    rownames(tables_qualify[[2]])<-tables_qualify[[2]][,1]
    write.csv(tables_qualify[[2]][,2], paste("startingpolpos",k,".csv"),row.names = rownames(tables_qualify[[2]])) #have to delete rest of the data and only keep starting pol pos  
  }
  
  #championship points
  page_point_data <- read_html(url_points_data) #reading the page as html
  tables_points <- html_table(page_point_data,header = FALSE,fill = TRUE) #extracting dataframes out of the html page race data
  View(tables_points)
  
  if(k==2015){
    rownames(tables_points[[1]])<-tables_points[[1]][,1]
    write.csv(tables_points[[1]][,2], paste("finished",k,".csv",sep = ""),row.names = rownames(tables_points[[1]]))
  }
  else{
    tables_points[[1]]<-tables_points[[1]][-1,-1]
    rownames(tables_points[[1]])<-tables_points[[1]][,1]
    write.csv(tables_points[[1]][,2], paste("finished",k,".csv",sep = ""),row.names = rownames(tables_points[[1]]))
  }
  assign(paste("combined",k,sep=""),value=(Reduce(function(x,y) merge(x,y,all=TRUE,by="X"), list(assign(paste("data_pitstop_",k,sep=""),value=read.csv(paste("pitstop",k,".csv",sep = ""),col.names = c("X","Average Pit Stop"))),
                                                                                                 assign(paste("data_tyre_",k,sep=""),value=read.csv(paste("tyres",k,".csv",sep=""))),
                                                                                                 assign(paste("data_lapsperpos_",k,sep=""),value=read.csv(paste("lapsperpos",k,".csv",sep = ""))),
                                                                                                 assign(paste("data_startfinish_",k,sep=""),value=read.csv(paste("started&finished",k,".csv",sep = ""))),
                                                                                                 assign(paste("data_lapsled_",k,sep=""),value=read.csv(paste("lapsled",k,".csv",sep = ""))),
                                                                                                 assign(paste("data_accident_",k,sep=""),value=read.csv(paste("accident",k,".csv",sep = ""),col.names = c("X","Accident"))),
                                                                                                 assign(paste("data_penalties_",k,sep=""),value=read.csv(paste("penalties",k,".csv",sep = ""))),
                                                                                                 assign(paste("data_startingpolpos_",k,sep=""),value=read.csv(paste("startingpolpos",k,".csv"),col.names = c("X","AvgPolPos"))),
                                                                                                 assign(paste("data_finished_",k,sep=""),value=read.csv(paste("finished",k,".csv",sep = ""),col.names = c("X","Total Points")))))))
}
combined2015_new<-subset(combined2015,select = -c(X))
combined2016_new<-subset(combined2016,select = -c(X))
combined2017_new<-subset(combined2017,select = -c(X))
combined2018_new<-subset(combined2018,select = -c(X,Hyper.soft))
combined2019_new<-subset(combined2019,select = -c(X,Hyper.soft))

#appending data for all the years
combined_data <- bind_rows(combined2015_new,combined2016_new,combined2017_new,combined2018_new,combined2019_new)
summary(combined_data)
combined_data_na_removed <- combined_data[complete.cases(combined_data),]
col_order<-c("Average.Pit.Stop","Hard","Medium","Soft","Super.soft","Ultra.soft","Wet","Intermediate","FirstPosition","SecondPosition","ThirdPosition","Started","Classified","Complete","Full.seasons.laps.led","Driver.s.season.laps.led","Accident","Penalties.due.to.driver","Penalties.due.to.team","No.action","AvgPolPos","Total.Points")
combined_data_na_removed<-combined_data_na_removed[,col_order]

write.csv(combined_data_na_removed, "combined_data.csv")

#correlation
# install.packages("ggpubr")
library("ggpubr")

# install.packages("polycor")
library("polycor")


#Principal componenet analysis
princi_cmp=prcomp(subset(combined_data_na_removed,select = -c(Total.Points)),scale=TRUE)
princi_cmp$rotation
summary(princi_cmp)
write.csv(princi_cmp$rotation,"prcomp.csv")


# install.packages("factoextra")
library(factoextra)
fviz_eig(princi_cmp) #scree plot
fviz_pca_biplot(princi_cmp,repel = TRUE,col.var = "#2E9FDF",col.ind = "#696969")

# Correlation plot
CR = hetcor(combined_data_na_removed, ML = FALSE, std.err = TRUE,  use = c("complete.obs", "pairwise.complete.obs"), bins=4, pd=TRUE)
# install.packages("corrplot")
library(corrplot)
corrplot(CR$correlations,type = "upper",method = "square",title = "Correlation Plot F1 Races",tl.col = "black",mar = c(1,1,5,1))

#Linear regression model
linearreg<-lm(Total.Points~Average.Pit.Stop+Hard+Medium+Soft+Super.soft+Ultra.soft+Wet+Intermediate+FirstPosition+SecondPosition+ThirdPosition+Started+Classified+Complete+Full.seasons.laps.led+Driver.s.season.laps.led+Accident+Penalties.due.to.driver+Penalties.due.to.team+No.action+AvgPolPos,data = combined_data_na_removed)
summary(linearreg)
  
  