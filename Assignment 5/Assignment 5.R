library(data.table)
library(igraph)
library(stringr)
library(splitstackshape)
library(plotly)
library(network)
investor = fread(file = "C:/Users/akabo/Downloads/social network analysis/Assignment 5/investor_firms.csv", header = TRUE)
investor_deal = fread(file = "C:/Users/akabo/Downloads/social network analysis/Assignment 5/investors_and_deals.csv", header = TRUE)
startup = fread(file = "C:/Users/akabo/Downloads/social network analysis/Assignment 5/startup_companies.csv", header = TRUE)
startup_deal = fread(file = "C:/Users/akabo/Downloads/social network analysis/Assignment 5/startups_and_deals.csv", header = TRUE)
setkey(investor, InvestorId)
setkey(investor_deal,Investor_Id)
colnames(investor_deal)=c("InvestorId","DealId","Lead_investor")
colnames(startup_deal)[2]="CompanyID"
a <- merge(investor,investor_deal,by="InvestorId")
b<- merge(startup,startup_deal,by="CompanyID")
investment <- merge(a,b,by="DealId")
investment<- investment[order(Investor_Name)]
investment_1 =investment[,.N,by=.(Investor_Name,Primary_Industry_Code,successful_investments)]
investment_1=investment_1[,.(sum=sum(N),len=length(N)),by=.(Investor_Name,successful_investments)]
investment_1$diverse <- investment_1$len/investment_1$sum
ggplot(investment_1,aes(investment_1$diverse,investment_1$successful_investments))+geom_point()+ylim(0,200)

# 1b ----------------------------------------------------------------------
model <- glm(successful_investments~diverse,data=investment_1) 
summary(model)
#Higher Diversificaton,lower successful investments.

# 2 -----------------------------------------------------------------------
#in order to calculate the edgelist, i use DealID as the key and merge a with itself, eliminating the self merged company
investment <- investment[order(Company_Name)]
c<-merge(a,a[,c(1,2,4)],by="DealId",allow.cartesian=TRUE)
c<- c[c$Investor_Name.x!=c$Investor_Name.y,]
invest <- c[,sum:=sum(Lead_investor),by=.(Investor_Name.x,Investor_Name.y)]
invest <- c[,total:=.N,by=.(Investor_Name.x,Investor_Name.y)]
#invest <- c[,Deal:=length(DealId),by=.(Investor_Name.x,Investor_Name.y)]
invest$weight <-invest$sum/invest$total
invest<-invest[,c(3,7,10)]
colnames(invest) = c("from","to","weight")
g1<-graph.data.frame(invest,directed = TRUE)
central <-eigen_centrality(g1)
central <- as.data.frame(central[1])
rowname<- row.names(central)
final <-cbind(central,rowname)
colnames(final)<-c("status","Investor_Name")
final <-merge(final,investor,by="Investor_Name")
plot(final$status,final$successful_investments)

# 2b ----------------------------------------------------------------------
model <- glm(successful_investments~status,data=final) 
summary(model)

# 3 -----------------------------------------------------------------------
investment_2 <- merge(investment_1,final, by="Investor_Name")
colnames(investment_2)[2]="successful_investments"
colnames(investment_2)[5]="diversification"
g3 <- glm(successful_investments~diversification*status,data=investment_2)
summary(g3)

# 3b ----------------------------------------------------------------------
library(rgl)
library(scatterplot3d)
# set up scaled grid of (x,y) values 
diversification = seq(0,1,by=0.02) 
status = seq(0,1,by=0.02) 
values = expand.grid(diversification=diversification, status=status)
# prediction from the model 
values$successful_investments = predict(g3,newdata =values)
# regular 3d plot 
scatterplot3d(values$diversification, values$status, values$successful_investments)
# interactive 3d plot you can move around 
plot3d(values$diversification, values$status, values$successful_investments) 

# 4 -----------------------------------------------------------------------
install.packages("nnet")
library(nnet)
investment3 <-investment_2[,c(1,5,6)]
final_1 <-merge(investment3,investment,by="Investor_Name")
final_2 <-final_1[final_1$Business_Status=="Generating Revenue"|final_1$Business_Status=="Startup"|final_1$Business_Status=="Out of Business"|final_1$Business_Status=="Generating Revenue/Not Profitable"|final_1$Business_Status=="Profitable"]
g4 = multinom(Business_Status ~ diversification + status + diversification*status, final_2) 
z =summary(g4)$coefficients/summary(g4)$standard.errors
(1 - pnorm(abs(z), 0, 1)) * 2 