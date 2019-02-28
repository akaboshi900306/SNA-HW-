library(igraph)
library(data.table)

# # 1 ---------------------------------------------------------------------

data = read.csv("C:/Users/akabo/Downloads/social network analysis/Assignment 1/classroom_social_and_task_network.csv", header=TRUE, as.is = TRUE)
trust <- data[!(data$social_tie==0&data$task_tie==0),]
social_tie =data.frame(matrix(NA,22,22))
for (a in (1:22)){
  for (b in (1:22)){
    social_tie[a,b]=data[(a-1)*22+b,3]
  }
}

social1 <-graph.adjacency(as.matrix(social_tie), "directed", weighted = TRUE)
Task_Tie =data.frame(matrix(NA,22,22))
for (x in (1:22)){
  for (y in (1:22)){
    Task_Tie[x,y]=data[(x-1)*22+y,4]
  }
}

task1 <-graph.adjacency(as.matrix(Task_Tie), "directed", weighted = TRUE)

#social_trust <- data[data$social_tie>0,1:3]
#task_trust <-data[data$task_tie>0,c(1,2,4)]
social_trust <-trust[,1:3]
task_trust <- trust[,c(1,2,4)]
#task_trust[task_trust==0] <-NA
#social_trust[social_trust==0] <-NA
#social1 <-graph_from_data_frame(social_trust,directed = TRUE)
#task1 <- graph.data.frame(task_trust,directed = TRUE)
#plot(social1)
#trust2 <- as_adjacency_matrix(trust1)
#V(social1)

social_indegree <- degree(social1, v = V(social1), mode = c("in"),
       loops = TRUE, normalized = FALSE)
#social_indegree1 <-graph_from_edgelist(social_indegree)
social_outdegree <- degree(social1, v = V(social1), mode = c("out"),
                          loops = TRUE, normalized = FALSE)
task_indegree <((j-1)*22+z)=NA- degree(task1, v = V(task1), mode = c("in"),
                          loops = TRUE, normalized = FALSE)
task_outdegree <- degree(task1, v = V(task1), mode = c("out"),
                           loops = TRUE, normalized = FALSE)


social_closeness <- closeness(social1, vids = V(social1),
          weights = NULL, normalized = FALSE)
task_closeness<- closeness(task1, vids = V(task1),
          weights = NULL, normalized = FALSE)

social_between <- betweenness(social1, v = V(social1))
task_between <- betweenness(task1, v = V(task1))

social_page <- page_rank(social1,vids = V(social1))
task_page <- page_rank(task1,vids = V(task1))

cor(social_indegree,task_indegree) #=0.5578869
cor(social_outdegree,task_outdegree) #=0.6996636
cor(social_closeness,task_closeness) #=0.2318945
#Warning message:
#In closeness(task1, vids = V(task1), weights = NULL, normalized = FALSE) :
  #At centrality.c:2617 :closeness centrality is not well-defined for disconnected graphs
cor(social_between,task_between) #=0.75168
cor(social_page$vector,task_page$vector)#= 0.1730575
# The betweenness of task network are most closely related to those in the 
#socializing network. I could assume that the outdegree and betweenness was quite same
#in both network.22 was in the middle and the other nodes spread out like petals. So the
#betweenness of each node and the center 22 were comparatively same.

# # 2 ---------------------------------------------------------------------

social_trustrow <- social_trust[social_trust$social_tie>0,]
social_mean= sum(data$social_tie)/nrow(social_trustrow )
task_trustrow <- task_trust[task_trust$task_tie>0,]
task_mean= sum(data$task_tie)/nrow(task_trustrow )

trust$Strength <- "w"
trust[trust$social_tie>social_mean|trust$task_tie>task_mean,]$Strength<-"s"
st_trust <-graph.data.frame(trust,directed = TRUE)

data$Strength = '0'
data[!(data$social_tie==0&data$task_tie==0),]$Strength <- 'w'
data[data$social_tie>social_mean|data$task_tie > task_mean,]$Strength<-'s'

c = E(st_trust)$Strength
colors[c  == "s"] = "light blue"
colors[c == "w"] = "red"

# and then 
E(st_trust)$c = colors
# or
plot(st_trust,vertex.label=V(st_trust)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
# graph doesn't appear to exhibit strong triadic closure
trust
#The network doesn't satisfy Strong Triadic Closure since No.22 has strong ties to 1 and 
#21. But there is no link between 1 and 21 according to the graph.
data$Strength = '0'
data[!(data$social_tie==0&data$task_tie==0),]$Strength <- 'w'
data[data$social_tie>social_mean|data$task_tie > task_mean,]$Strength<-'s'

count=0
for (i in (1:22)) {
  for (j in (1:22)) {
    for (z in j:22) {
      if ((data[(i-1)*22+j,5]=="s"|data[(j-1)*22+i,5]=="s")&(data[(i-1)*22+z,5]=="s"|data[(z-1)*22+i,5]=="s")&(data[(j-1)*22+z,5]==0)&(data[(z-1)*22+j,5]==0)&z!=j&z!=i&i!=j){
        print(paste(i,j,z))
        count=count+1
      }
    }
  }
}
data[,5]<- data
sum(data[,5]=='s')
count
#(B)
social_trust1 <- data[data$social_tie>0,3]
task_trust1 <-data[data$task_tie>0,4]
social_median <-median(social_trust1)
task_median <-median(task_trust1)

trust$Strength1 <- "w"
trust[trust$social_tie>social_mean|trust$task_tie>task_mean,]$Strength1<-"s"
st_trust1 <-graph.data.frame(trust,directed = TRUE)
 st_trust1
#IGRAPH 24021fd DN-- 21 79 -- 
#  + attr: name (v/c), social_tie (e/n), task_tie (e/n), Strength (e/c), Strength1 (e/c)
#+ edges from 24021fd (vertex names):
#  [1] 1 ->5  1 ->6  1 ->22 2 ->22 4 ->8  5 ->1  5 ->6  5 ->22 6 ->1  6 ->5  6 ->9  6 ->22 7 ->10
#[14] 7 ->22 8 ->4  9 ->6  9 ->22 10->7  10->12 10->22 11->15 11->22 12->10 12->16 12->18 13->18
#[27] 13->22 14->22 15->11 15->22 16->12 16->17 16->18 16->19 16->22 17->16 17->18 17->19 17->21
#[40] 17->22 18->12 18->13 18->16 18->17 18->19 18->20 18->21 18->22 19->16 19->17 19->18 19->20
#[53] 19->21 19->22 20->18 20->19 20->21 20->22 21->17 21->18 21->20 21->22 22->1  22->2  22->5 
#[66] 22->6  22->7  22->9  22->10 22->11 22->13 22->14 22->15 22->16 22->17 22->18 22->19 22->20
#[79] 22->21

 #This network still can not satisfy the Strong Triadic Closure. In my opinion, it's partly due to 
 #the combined strong ties by two network. Since we assume that whenever there is a strong tie,
 #the row of the combined network is defined as "strong tie". I It is easier to have two strong 
 #ties in a clique with 22 in the central.

# #3 ----------------------------------------------------------------------
ST_tie <-social_tie+Task_Tie
ST <-graph.adjacency(as.matrix(ST_tie), "directed", weighted = TRUE)
ST_edge <- edge_betweenness(ST, e = E(ST), directed = TRUE, weights = NULL)
#[1]  0.0000000 18.0000000  0.0000000 18.0000000  1.0000000  0.0000000 18.0000000  0.0000000
#[9] 18.0000000 18.0000000 48.0000000  0.0000000  3.0000000 15.0000000  1.0000000 48.0000000
#[17] 60.0000000  2.3333333 17.5000000 15.0000000  1.0000000 17.0000000 13.3333333  8.0000000
#[25] 10.0000000  4.6666667 37.6666667 18.0000000  1.0000000 17.0000000  8.3333333 16.1666667
#[33] 11.8333333  0.0000000  0.0000000 22.5000000  0.0000000 16.1666667  5.0000000 39.8333333
#[41]  5.5000000 32.3333333  5.8333333  0.0000000  0.8333333  3.5000000  0.0000000  0.0000000
#[49]  0.0000000  7.5000000  0.5000000  0.0000000 10.0000000  0.0000000 21.0000000  1.0000000
#[57] 19.0000000  0.0000000 29.5000000  0.0000000  4.5000000  0.0000000  0.0000000 18.0000000
#[65]  0.0000000  0.0000000 15.6666667 60.0000000 18.5000000 17.0000000 10.0000000 18.0000000
#[73] 17.0000000  0.0000000 30.3333333  0.0000000  0.0000000 33.0000000  0.0000000
#Does it seem like edges with high betweenness tend to be strong or weak ties, according to our two de???nitions above? 
#Does this result make sense?
#It seems that the high betweenness have no relationship with the attributes of the ties according to
#the correlation between the edge betweenness and the ties in network.

# # 4 ---------------------------------------------------------------------
ST_edge_1= as.matrix(ST_tie)%*%as.matrix(ST_tie)
for (i in (1:10)) {
  ST_edge_1=as.matrix(ST_edge_1)%*%as.matrix(ST_tie)
}
#Judged by the ten times product of the combined matrix, we could see that the only 3 has no links with one
#another. While 4 and 8 only have one link between each other.The multiply of each matrix for one time means
#the node walks around for one time. So I wrote a for loop to calculate the walk for 10 times and focused on
# the 0 in the specific column.

# # 5 ---------------------------------------------------------------------

ring = make_ring(30)
plot(ring)
centr_degree(ring, mode = c("all", "out", "in", "total"), loops = TRUE,
             normalized = TRUE)
#Create a star graph
g1 = graph( edges=c(1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,1,10,1,11,1,12,1,13,1,14,1,15,1,16,1,17,1,18,1,19,1,20,1,21,1,22,1,23,1,24,1,25,1,26,1,27), n=3, directed=FALSE)
plot(g1)
centr_degree(g1, mode = c("all"), loops = TRUE,
             normalized = TRUE)
# The relationship will not hold true for other measures for closeness and betweenness.
