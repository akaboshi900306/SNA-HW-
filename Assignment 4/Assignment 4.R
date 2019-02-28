library(data.table)
library(igraph)
library(stringr)
library(splitstackshape)
library(plotly)
library(network)
library(tidyr)
library("plyr")

producer = fread(file = "C:/Users/akabo/Downloads/social network analysis/Assignment 4/producers_and_films.csv", header = TRUE)
keyword = fread(file = "C:/Users/akabo/Downloads/social network analysis/Assignment 4/film_keywords.csv", header = TRUE)
producer=producer[V6=="us]",]
revenue = fread(file = "C:/Users/akabo/Downloads/social network analysis/Assignment 4/box_office_revenues.csv", header = TRUE)
sub=fread(file = "C:/Users/akabo/Downloads/social network analysis/Assignment 4/production_subsidiaries.csv", header = TRUE)
# ans <- producer[, .(producer$year, producer$project)]
# bns <- producer[year == "2017"&country == "us",.N]
# cns <- producer[country=="us", .N, keyby= .(year,prod_company)]
# dns <- flights[country=="us",
#                .(mean(arr_delay), mean(dep_delay)),
#                by = .(origin, dest, month)]
# ens <-producer[year==1985, print(.SD)]
# bns <- producer[year=="2017",.N,by=.(prod_company)]
# producer$general =NA
# name = as.array(unique(ens$prod_company))
# producer <- producer[order(year)]

producer<- producer[,count:=.N,by=.(prod_company,year)]
producer$count[producer$count<=1] ="0"
producer$count[producer$count>=1] ="1"
producer[,sum:= sum(as.numeric(count)),by=project]
producer[,count_num:=.N,by=.(project,year)]
producer$type=NA
producer$type[producer$count_num==producer$sum&producer$sum>1] <-"Central co-productions"
producer$type[producer$count_num=="1"&producer$sum=="0"]<-"Peripheral solo productions"
producer$type[producer$count_num>"1"&producer$sum=="0"]<-"Peripheral co-productions"
producer$type[producer$count_num=="1"&producer$sum=="1"]<-"Central solo productions"
producer$type[producer$count_num > producer$sum& producer$sum>"0"]<- "Hybrid co-productions"




producer1 <- producer[,.(pindex,year,project,pcindex,prod_company,type)]
producer1 <- producer1[,.SD,by=type]
setkey(producer1, pindex)
setkey(keyword,pindex)
producer2 <- merge(producer1, keyword, allow.cartesian=TRUE)
producer2 <-producer2[,min_year:=min(year),by=.(keyword,type)]
new_word <-producer2[producer2$year-producer2$min_year<=3]
new_word <-as.data.table(new_word)
new_word<-new_word[,-(5:6)]
new_word <-unique(new_word)

produce_1<-new_word[type=="Peripheral solo productions",.N,by=.(year)]
produce_2<-new_word[type=="Central solo productions",.N,by=.(year)]
produce_3<-new_word[type=="Central co-productions",.N,by=.(year)]
produce_4<-new_word[type=="Peripheral co-productions",.N,by=.(year)]
produce_5<-new_word[type=="Hybrid co-productions",.N,by=.(year)]

list <- c("produce_1","produce_2","produce_3","produce_4","produce_5") #just one character vector as the titles are the same as the names of the data frames

myplot <- function(data, title){
  ggplot(data, aes(x = year, y = N)) +
    geom_line() +
    labs(title = title)
}

for(i in list){
  print(myplot(get(i), i))
}


# 1B ----------------------------------------------------------------------

library(stats)
library(proxy)



producer4 <-producer1[,.N,keyby=.(prod_company,year,type)]
producer4 <- producer4[order(prod_company,year)]

producer2 <- merge(producer1, keyword, allow.cartesian=TRUE)
producer2 <-producer2[,min_year:=min(year),by=.(keyword)]
producer2 <-producer2[producer2$year-producer2$min_year<=3]
producer_y <- producer2[,.N,by=.(prod_company,year,pindex,pcindex)]
final_merge <-merge(producer_y,producer4,by=c("prod_company","year"))
colnames(final_merge)<- c("prod_company","year","pindex","pcindex","newkey","type","number")
final_merge <- merge(final_merge,revenue,by="pindex")
final_merge[type=="Central co-productions",num_ccp:=sum(number),by=.(pindex,prod_company,year,newkey,total_box,number)]
final_merge[type=="Peripheral co-productions",num_pcp:=sum(number),by=.(pindex,prod_company,year,newkey,total_box,number,num_ccp)]
final_merge[type=="Hybrid co-productions",num_hcp:=sum(number),by=.(pindex,prod_company,year,newkey,total_box,number,num_ccp,num_pcp)]
final_merge1 <-final_merge[,startyear:=min(year),by=.(prod_company)]
final_merge1$operation<-final_merge1$year-final_merge1$startyear
final_merge2 <-merge(final_merge1,sub,by="pcindex",all.x= TRUE,all.y= TRUE)

edge_list_t <-data.frame(matrix(NA,10000,3))
colnames(edge_list_t)<-c("from","to","weight")
edge_list<-data.frame(matrix(NA,10000,3))
colnames(edge_list)<-c("from","to","weight")
for(z in(1986:2016)){
  yearly <- unique(producer2[year==z,]$prod_company)
}
for (i in 1:length(yearly)){
  for(j in i:length(yearly)){
    len<-length(intersect(unlist(producer2[year<=z&year>=(z-2)&prod_company==yearly[i],]$keyword),
                          unlist(producer2[year<=z & year>=(z-2)&prod_company==yearly[j],]$keyword)))
    edge_list_t$from<- yearly[i]
    edge_list_t$to <-yearly[j]
    edge_list_t$weight<-len
    edge_list <- rbind(edge_list,edge_list_t)
  }
}
edge_list_1 <-unique(edge_list[edge_list$weight!=0,])
i1 <- graph.data.frame(edge_list_1,directed=FALSE)
table1 <- as_adjacency_matrix(i1)
table1<-as.matrix(table1)
table2 <-cmdscale(dist(table1,"Jaccard"),2)

for(z in(1986:2016)){
  yearly <- unique(producer2[year==z,]$prod_company)
for(k in 1:length(rownames(table2))){
  final_merge2[year==z&producer==rownames(table2)[k],"coordinate1"]<-table2[k,1]
  final_merge2[year==z&producer==rownames(table2)[k],"coordinate2"]<-table2[k,2]
}
}




  
  
  
  
  