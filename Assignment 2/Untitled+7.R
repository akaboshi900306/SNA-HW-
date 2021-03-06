library(igraph)
library(data.table)
install.packages("tidyr")
install.packages("gdata")
library(gdata)
library(tidyr)
library(ggplot2)
VC1<-fread("C:/Users/akabo/Downloads/social network analysis/Assignment 2/Funding_events_7.14.csv",header = TRUE)
VC2 <-fread("C:/Users/akabo/Downloads/social network analysis/Assignment 2/Funding_events_7.14_page2.csv",header = TRUE)
VC <-rbind(VC1,VC2)

# Firstly, i use seperate rows to transform the data, but i forgot to put in the "Deal Date " first.
#Also, i have eliminated the loop in network_update1 and trimmed the blank in the data to avoid the incomformity.


VC$mon=month(VC$`Deal Date`)
VC$year<-year(VC$`Deal Date`)
network_update <- data.table(From=VC$Investors,To=VC$Investors,Month=VC$mon,Year=VC$year)
network_update<-separate_rows(network_update,To,sep = ",")
network_update<-separate_rows(network_update,From,sep = ",")
network_update[network_update==" Inc"|network_update==" Inc."| network_update==" LLC"|network_update==" LLC."|network_update==" Ltd"|network_update==" Ltd."|network_update ==" Co"|network_update==" Co."|network_update==" Corp"|network_update==" Corp."]<-NA
network_update[network_update==""] <-NA
network_update1 <-network_update[complete.cases(network_update), ]
network_update1<-unique(network_update1)
# Transformation process with trimws!!
network_update1 <-network_update1[network_update1$From != network_update1$To]
#hard to transform without blanks!
network <- data.frame(lapply(network_update1, trimws), stringsAsFactors = FALSE)

network2 <- data.frame(network)
firm <- graph.data.frame(network2, directed = FALSE)
firm_simple = simplify(firm, remove.multiple = TRUE, remove.loops = TRUE)
firm_closeness <- closeness(firm_simple, vids = V(firm_simple))
max(firm_closeness)
V(firm)[which.max(firm_closeness)]#Intel Capital/1.623871e-07


# B -----------------------------------------------------------------------

distance_firm <-shortest.paths(firm_simple)
distance_firm[is.infinite(distance_firm)] <- nrow(distance_firm)
avg_firm <- rowSums(distance_firm)/ncol(distance_firm)
avg_firm[which.min(avg_firm)]#Intel Capital 493.0445

# C ----------------------------------------------------------------------

sum(avg_firm )/ncol(distance_firm) # 965.845

# 2A -----------------------------------------------------------------------


# run loop to find the coreness based on mon and year ---------------------
network_update1$monthsum <- (network_update1$Year-1982)*12+network_update1$Month
count=0
final <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Monthsum", "k")
colnames(final) <- x
for (i in (1:396)){
    newdata <- subset(network_update1, network_update1$monthsum<=i)
    firm_update <- graph.data.frame(newdata, directed = FALSE)
    firm_update = simplify(firm_update, remove.multiple = TRUE, remove.loops = TRUE)
    firm_corn <- coreness(firm_update, mode = c("all", "out", "in"))
    loop <- data.frame(i,mean(firm_corn))
    colnames(loop)=c("Monthsum","k")
    final <-rbind(final,loop)
    count=count+1
    print(final)
}


final <- final[order(final$Year,final$Month),] 
final1 <- unique(final)   

for (i in (7:396)){
  if(is.nan(final1[i,3])==TRUE){
  final1[i,3]=final1[i-1,3]
  }
}




# ggplot ------------------------------------------------------------------

final_plot <-ggplot(data=final1,aes(x=final1$Monthsum,y=final1$k))+geom_line()+geom_smooth(method = "loess")


# 2B ----------------------------------------------------------------------
count=0
final2 <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Month", "Year", "k")
colnames(final2) <- x
for (j in (1982:2014)){
  for (i in (1:12)){
    newdata <- subset(network_update1, network_update1$Month==i& network_update1$Year<=j&network_update1$Year>j-10)
    firm_update2 <- graph.data.frame(newdata, directed = FALSE)
    firm_update2 = simplify(firm_update2, remove.multiple = TRUE, remove.loops = TRUE)
    firm_corn2 <- coreness(firm_update2, mode = c("all", "out", "in"))
    loop2 <- data.frame(i,j,mean(firm_corn2))
    colnames(loop2)=c("Month","Year","k")
    final2 <-rbind(final2,loop2)
    count=count+1
    print(final2)
  }
}

final2 <- final2[order(final2$Year,final2$Month),] 
final2 <- unique(final2)   

for (i in (7:396)){
  if(is.nan(final2[i,3])==TRUE){
    final2[i,3]=final2[i-1,3]
  }
}

# ggplot ------------------------------------------------------------------

final_plot1 <-ggplot(data=final2,aes(x=final1$monthsum,y=final1$k))+geom_line()
#There is no difference between two plots.The relationship between the co investment venture company 
#will change little with the passing years. The bind is firm and consisitent with same company ties.

# 3 -----------------------------------------------------------------------


# 4A -----------------------------------------------------------------------
#print the corrlation based on the year 
VC3<-fread("C:/Users/akabo/Downloads/social network analysis/Assignment 2/Venture_capital_firm_outcomes.csv",header = TRUE)
final3 <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Year", "correlation")
count=0
for (i in (1982:2014)){
    newdata1 <- subset(network_update1,network_update1$Year==i)
    firm_update2 <- graph.data.frame(newdata1, directed = FALSE)
    firm_update2 = simplify(firm_update2, remove.multiple = TRUE, remove.loops = TRUE)
    firm_closeness2 <- closeness(firm_update2, vids = V(firm_update2))
    firm_closeness2=data.frame(firm_closeness2)
    firm_closeness2$firm_name <-rownames(firm_closeness2)
    firm_closeness2 <- data.frame(lapply(firm_closeness2, trimws), stringsAsFactors = FALSE)
    VC_merge = subset(VC3[,1:3],VC3$year==2000)
    success <-merge(firm_closeness2,VC_merge,by.x ="firm_name")
    a= data.frame(i,cor(as.numeric(success$firm_closeness2),as.numeric(success$successful_investments)))
    colnames(a)=c("Year","correlation")
    final3 = rbind(final3,a)
    print(a)
    count=count+1
}

# 4B ----------------------------------------------------------------------

  
