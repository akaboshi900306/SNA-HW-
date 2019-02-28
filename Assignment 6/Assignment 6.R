library(data.table)
library(igraph)
library(stringr)
library(splitstackshape)
library(plotly)
library(network)
install.packages("ergm")
library(ergm)
install.packages("tidyverse")
library(tidyverse)
exe<- fread("C:/Users/akabo/Downloads/social network analysis/Assignment 6/execs.csv")
investor <- fread("C:/Users/akabo/Downloads/social network analysis/Assignment 6/investors.csv")
deal <- fread("C:/Users/akabo/Downloads/social network analysis/Assignment 6/deal_detailed.csv")
investor_detail<-fread("C:/Users/akabo/Downloads/social network analysis/Assignment 6/investor_firm_details.csv")
people<-fread("C:/Users/akabo/Downloads/social network analysis/Assignment 6/people.csv")
colnames(investor)[1] <-c("investor")
relation <-merge(exe,investor,by="CompanyId",allow.cartesian=TRUE)
relation1<-merge(relation,deal,by="DealId",allow.cartesian=TRUE)
relation2 <- relation1[(Deal_Type_1!="Merger/Acquisition")&(Deal_Type_1!="Buyout/LBO")&(Deal_Type_1!="IPO")&
                         (Deal_Type_2!="Merger/Acquisition")&(Deal_Type_2!="Buyout/LBO")&(Deal_Type_2!="IPO"),]
relation2<-relation2[,-11]
colnames(relation2)[2]<-c("CompanyId")
merge <- merge(relation2,investor_detail,by="InvestorId",allow.cartesian=TRUE)
merge<-merge[Country=="United States",]
merge<-merge[,year:=word(Deal_Date,-1)]
merge<-merge[year>"0"&year<"20",]
exp<-merge[,.N,by=City]
city <- exp[N>1000,1]
city<- city[-51,]
city<- as.matrix(city)
merge1<- merge[merge$City%in%city,]
merge1<- merge1[,c(8,4)]
g1 <- network(merge1,directed=TRUE)
g2<-graph.data.frame(merge1,directed=TRUE)
q1 <- ergm(g1 ~ edges + triangle)
summary(q1)
Formula:   g1 ~ edges + triangle

# Iterations:  20 out of 20 
# 
# Monte Carlo MLE Results:
#   Estimate Std. Error MCMC % p-value    
# edges    -10.578112   0.008004      4  <1e-04 ***
#   triangle 396.825973   2.030865      3  <1e-04 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Null Deviance: 1.276e+10  on 9.202e+09  degrees of freedom
# Residual Deviance: 1.191e+10  on 9.202e+09  degrees of freedom
# 
# AIC: 1.191e+10    BIC: 1.191e+10    (Smaller is better.) 
#The result suggests that the edge and the triangle all have siginifiant influence over investment decision.

# 2 ---------------------------------------------------------------
g2_gender<- data.table(rep(NA,95930))
g2_gender$name <- V(g2)$name
g2_gender <- g2_gender[,2]
colnames(g2_gender)<-c("PersonId")
person_gender <-merge(g2_gender,people,by="PersonId",all.x=TRUE)
set.vertex.attribute(g1, 'gender',person_gender$Gender)
g2_edu<- data.table(rep(NA,95930))
g2_edu$name <- V(g2)$name
g2_edu <- g2_edu[,2]
colnames(g2_edu)<-c("PersonId")
person_edu <-merge(g2_edu,people,by="PersonId",all.x=TRUE)
person_edu$MBA <-"0"
person_edu[grepl('MBA',person_edu[,Education]),5]<-"1"
set.vertex.attribute(g1, 'education',person_edu$MBA)
q1 <- ergm(g1 ~ edges + triangle+nodematch("education")+nodematch("gender"))
summary(q1)
# Formula:   g1 ~ edges + triangle + nodematch("education") + nodematch("gender")
# 
# Iterations:  20 out of 20 
# 
# Monte Carlo MLE Results:
#   Estimate Std. Error MCMC % p-value    
# edges               -10.15137    0.01905      4  <1e-04 ***
#   triangle              1.42368    0.07530     13  <1e-04 ***
#   nodematch.education  -0.52853    0.01861      3  <1e-04 ***
#   nodematch.gender      0.11047    0.02366      3  <1e-04 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Null Deviance: 1.276e+10  on 9.202e+09  degrees of freedom
# Residual Deviance: 1.191e+10  on 9.202e+09  degrees of freedom
# 
# AIC: 1.191e+10    BIC: 1.191e+10    (Smaller is better.) 
# The result suggests that the edge, the triangle, the e???ect of having the same gender 
#and of the investor and the executive both having an MBA. all have siginifiant influence 
#over investment decision.

# Question 3 --------------------------------------------------------------
invest <- investor[,length(unique(InvestorId)),by=investor]
set.vertex.attribute(g1, 'invest',invest$V1)
# relation1<-relation1[,-11]
# colnames(relation1)[2]<-c("CompanyId")
# merge <- merge(relation1,investor_detail,by="InvestorId",allow.cartesian=TRUE)
# merge[,.N:=length(InvestorId),by=PersonId]
# sum <- merge[,sum(successful_investments),by=PersonId]
# set.vertex.attribute(g1, "success",sum$V1)
executive_suc<-executive_suc%>%group_by(PersonId)%>%summarise(suc=sum(successful_investments))

q1 <- ergm(g1 ~ edges + triangle+nodecov("invest")+nodematch("education")+nodematch("gender"))
summary(q1)
Formula:   g1 ~ edges + triangle + nodecov("invest") + nodematch("education") + 
  nodematch("gender")
# 
# Iterations:  20 out of 20 
# 
# Monte Carlo MLE Results:
#   Estimate Std. Error MCMC %  p-value    
# edges               -10.42028    0.04567      3  < 1e-04 ***
#   triangle              1.19610    0.06456     13  < 1e-04 ***
#   nodecov.invest        0.07991    0.02161      2 0.000218 ***
#   nodematch.education  -0.56942    0.01770      3  < 1e-04 ***
#   nodematch.gender      0.24025    0.02276      2  < 1e-04 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Null Deviance: 1.276e+10  on 9.202e+09  degrees of freedom
# Residual Deviance: 6.104e+06  on 9.202e+09  degrees of freedom
# 
# AIC: 6103623    BIC: 6103728    (Smaller is better.) 


# The result suggests that the edge, the triangle ,also the total number of di???erent
#companies the startup investor has worked for,as well as the total number of successful deals the
#e???ect of having the same gender and of the investor and the executive both having an MBAn all have siginifiant influence 
#over investment decision.



















