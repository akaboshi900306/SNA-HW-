data<- read.csv("C:/Users/akabo/Downloads/social network analysis/Assignment 3/district_information.csv",head=TRUE)
#data[data$district=="Amritsar",]
rain_inf <- read.csv("C:/Users/akabo/Downloads/social network analysis/Assignment 3/rain_information.csv",head=TRUE)
#library(dplyr)
#summary(rain$district)
border <- read.csv("C:/Users/akabo/Downloads/social network analysis/Assignment 3/border_information.csv",head=TRUE)

rain_inf <- rain_inf[-(which(rain_inf$year==1946& rain_inf$rain<100)),]
#b <-aggregate(rain_inf$rain, by=list(Category=rain_inf$district), FUN=sum)
rain_inf$sum <-NA
rain_inf$spi_mean <-NA

#year= as.array(unique(data$year))
year <-c("1946","1951","1957","1962","1967","1971","1977","1980","1984","1985","1989","1991","1996","1998","1999")

name = as.array(unique(rain_inf$district))
for (i in 1:332){
  for (j in 2:15){
  rain_inf[which(rain_inf$district==name[i]& rain_inf$year==year[j]),5]<-
    sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=year[j]& rain_inf$year > year[j-1]),3])
  rain_inf[which(rain_inf$district==name[i]& rain_inf$year==year[j]),6]<-
    mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=year[j]& rain_inf$year > year[j-1]),4])
  }
}

  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1957),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1957&rain_inf$year>1951),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1957),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1957&rain_inf$year>1951),4])
  # 
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1962),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1962&rain_inf$year>1957),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1962),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1962&rain_inf$year>1957),4])
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1967),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1967&rain_inf$year>1962),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1967),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1967&rain_inf$year>1962),4])
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1971),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1971&rain_inf$year>1967),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1971),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1971&rain_inf$year>1967),4])
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1977),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1977&rain_inf$year>1971),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1977),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=19771&rain_inf$year>1971),4])
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1980),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1980&rain_inf$year>1977),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1980),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1980&rain_inf$year>1977),4])
  # 
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1984),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1984&rain_inf$year>1980),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1984),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1984&rain_inf$year>1980),4])
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1985),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1985&rain_inf$year>1984),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1985),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1985&rain_inf$year>1984),4])
  # 
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1989),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1989&rain_inf$year>1985),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1989),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1989&rain_inf$year>1985),4])
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1991),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1991&rain_inf$year>1989),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1991),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1991&rain_inf$year>1989),4])
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1996),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1996&rain_inf$year>1991),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1996),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1996&rain_inf$year>1991),4])
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1998),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1998&rain_inf$year>1996),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1998),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1998&rain_inf$year>1996),4])
  # 
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1999),5]<-
  #   sum(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1999&rain_inf$year>1998),3])
  # rain_inf[which(rain_inf$district==name[i]& rain_inf$year==1999),6]<-
  #   mean(rain_inf[which(rain_inf$district==name[i]& rain_inf$year<=1999&rain_inf$year>1998),4])


merge_data <- merge(rain_inf,data,by=c("district","year"))
plot(merge_data$rain,merge_data$new_parties)

# 1b ----------------------------------------------------------------------
library(igraph)
border_neigh <- graph.data.frame(border,directed = FALSE)

length(unique(neighbors(border_neigh,v=name[12])))
length(unique(border[border$focal_district==name[1],"district"]))

rain_neigh<- rain_inf
rain_inf$rain_neighbor <- NA
rain_inf$rain_p <- NA
rain_inf$year_length <- NA

for(i in 2:15){
  for (j in 1:332){
    rain_inf[(rain_inf$year<=year[i]) & (rain_inf$year > year[i-1])&(rain_inf$district==name[j]),"rain_p"]<-
               sum(rain_inf[(rain_inf$year<=year[i-1])&(rain_inf$year>year[i-2])&(rain_inf$district==name[j]),"rain"])/(year[i-1]-year[i-2])
    
    rain_inf[(rain_inf$year==year[i]) &(rain_inf$district==name[j]),"rain"]<-
      sum(rain_inf[(rain_inf$year<=year[i])&(rain$year>year[i-1])&(rain_inf$district==name[j]),"rain"])/(year[i]-year[i-1])
    
    rain_inf[(rain_inf$year==year[i]) &(rain_inf$district==name[j]),"rain"]<-
      sum(rain_inf[(rain_inf$year<=year[i])&(rain$year>year[i-1])&(rain_inf$district==name[j]),"spi"])/(year[i]-year[i-1])
    
    rain_inf[(rain_inf$year<=year[i]) & (rain_inf$year > year[i-1])&(rain_inf$district=name[j]),"rain_neighbor"]<-
      sum(rain_inf[(rain_inf$year<=year[i-1])&(rain$year>year[i-2])&(rain_inf$district%in%border[border$focal_district==name[j],"district"]),"rain"])/(length(unique(border[border$focal_district==place[j],"district"]))*(year[i-1]-year[i-2]))
   
    rain_inf[(rain_inf$year<=year[i]) & (rain_inf$year > year[i-1])&(rain_inf$district==name[j]),"year_length"]<-(year[i-1]-year[i-2])
  }
}

rain_inf <-rain_inf[rain_inf$year%in%year[3:15],]
p1<- plm(rain_inf~rain_p+rain_neighbor,data=rain_neighbor,effect= "twoways",mode="within",index= "district")
p2<-plm(spi ~rain_p+rain_neighbor,data=rain_neighbor,effect= "twoways",mode="within",index= "district")

# c -----------------------------------------------------------------------

extreme <- rain_inf[,c("district","year")]  
extreme$extreme_nei <-NA
extreme$extreme_p <-NA
extreme$extreme_neighbor <- NA

for(i in 3:15){
  for (j in 1:332){
    extreme[(extreme$year<=year[i]) & (extreme$year > year[i-1])&(rain_inf$district==place[j]),"extreme_p"]<-
      count(rain_inf[((rain_inf$year<=year[i-1])&(rain_inf$year>year[i-2])&(rain_inf$district==place[j])&(abs(rain_inf$spi)>1),])
   
    extreme[(extreme$year==year[i]) &(extreme$district==place[j]),"extreme_nei"]<-
    count(rain_inf[(rain_inf$year<=year[i])&(rain$year>year[i-1])&(rain_inf$district==place[j])&(abs(rain_inf$spi)>1,])
    
    
    extreme[(extreme$year==year[i]) & (extrene$district==place[j]),"extreme_neignbor"]<-
    count(rain_inf[(rain_inf$year<=year[i-1])&(rain$year>year[i-2])&(rain_inf$district%in%border[border$focal_district==place[j],"district"])&(abs(rain_inf$spi)>1,])/length(unique(border[border$focal_district==place[j],"district"]))
  }
}

p3<- plm(extreme_nei~extreme_p+extreme_neighbor,data=extreme_neighbor,effect= "twoways",mode="within",index= "district",family="poisson")

# 2 -----------------------------------------------------------------------
setkeyv(district, c("district", "year"))
setkeyv(rain_climate_lagged_new, c("district", "year"))

district_climate = merge(district, rain_climate_lagged_new)

# regression predicting the number of new political parties that are formed as a function
# of the number of years a district experiences droughts or flooding in the interval starting
# from the year following the previous election up until the year of the current election
lm3 <- pglm(total_parties ~ climate + climate_lagged, district_climate, effect = "twoways", model = "within", index = "district", family="poisson")
summary(lm3)
# 3 -----------------------------------------------------------------------

# regressing between number of parties founded and extreme weather and lagged number of extreme weather of neigbors
lm4 <- pglm(total_parties ~ climate + climate_neighbor_lagged, district_climate, effect = "twoways", model = "within", index = "district", family="poisson")
summary(lm4)              

# 4 -----------------------------------------------------------------------

# regressing between political concenration and extreme weather of a district and its neigbor's lagged number of extreme weather
lm5 <- pglm(political_concentration ~ climate + climate_neighbor_lagged, district_climate, effect = "twoways", model = "within", index = "district", family="poisson")
summary(lm5)

# 5 -----------------------------------------------------------------------
# load data
new_party <- fread("new_parties_in_each_district_by_candidate.csv")

# concatenate district in border table 
border_agg <- aggregate(district ~ focal_district, border_cmb, paste, collapse = ",")

# rename column names
colnames(border_agg) <- c("district", "focal_district")

# merge togethwe
new_party_neigbor <- merge(new_party, border_agg, by="district", all.x=TRUE)

# create two empty value to store number of neigbor parties 
num_neighbor_party <- NULL
num_neighbor_party1 <- NULL

# explore whether new political parties being founded in a district, that have contested an election in a neighboring district in any previous election period
lm6 <- pglm(ind ~ climate + climate_neighbor_lagged, district_climate, effect = "twoways", model = "within", index = "district", family="poisson")
summary(lm6)

# explore wether new political parties being founded in a district that have not contested an election in a neighboring district in any previous election period
lm7 <- pglm(ind1 ~ climate + climate_neighbor_lagged, district_climate, effect = "twoways", model = "within", index = "district", family="poisson")
summary(lm7)
                        

  
  
