
rm(list = ls(all = TRUE))
library(igraph)
library(data.table)
library(splitstackshape)
library(gridExtra)

setwd("C:/Users/demetrius/Dropbox")
setwd("~/Dropbox")


data = fread(file="C:/Users/akabo/Downloads/social network analysis/Assignment 6/anonymized_mba_classroom_network_with_demographics.csv")

advice = data[,22:26]

namekey = data[,1]

adj = data[,2:21]

setcolorder(adj, sample(colnames(adj), replace = FALSE))

# getting adjacency data into igraph

# make the choice data numeric
scale = cbind(c("Extremely Distrust", "Distrust", "Slightly Distrust", "Neither Distrust Nor Trust", "Slightly Trust","Trust", "Extremely Trust", "I don't know this person.", "This is my own name."), c(-3, -2, -1, 0, 1, 2, 3, 0, 0))

column_wise_replace = function(DT, x, y) {
    for(i in seq_along(x)){
         for (j in seq_len(ncol(DT))){
            set(DT,which(DT[[j]] == x[i]),j,y[i])
        }
    }
}


column_wise_replace(adj, scale[,1], scale[,2])

# make adjacency matrices for trust and distrust

# data are directed, so matrix will not necessarily be symmetric
adj = as.data.frame(adj)
rownames(adj) = namekey$Name

# remove columns of participants who did not receive complete answers about themselves
adj = adj[,colSums(adj=="")==0]

# insert a row of 0s for participants who did not fill out the survey, so they can still be represented in terms of ties received
insert = colnames(adj)[!colnames(adj) %in% intersect(colnames(adj), rownames(adj))]
insert = sapply(1:length(insert), function(i) cbind(insert[i], rep(0, ncol(adj))))
insert_df = as.data.frame(t(insert[-1,]))
colnames(insert_df) = colnames(adj)
rownames(insert_df) = insert[1,]

adj = rbind(adj, insert_df)

adj = adj[sort(rownames(adj)),sort(colnames(adj))]


# remove columns of participants who did not respond to the survey *and* rows of participants who did not receive complete answers (set up by the previous step)
#adj = adj[intersect(colnames(adj), rownames(adj)), intersect(colnames(adj), rownames(adj))]


# igraph ignores blanks, which is okay when there are no isolates, but otherwise need blanks to be represented as 0s for isolates to appear on the plot
trust_matrix = apply(adj, 2, as.numeric)

# subset to just the trust choices first, then sort on the columns and rows to make each entry match up
trust_matrix[trust_matrix < 0] = 0

# trust network
trust = graph.adjacency(as.matrix(trust_matrix), "directed", weighted = TRUE)
trust = simplify(trust, remove.loops = TRUE)


# subset to just distrust choices
distrust_matrix = apply(adj, 2, as.numeric)*-1
distrust_matrix[distrust_matrix < 0] = 0

# distrust network
distrust = graph.adjacency(as.matrix(distrust_matrix), "directed", weighted = TRUE)


# looking at advice network
advice_edge1 = cbind(namekey, advice[,1])
advice_edge2 = cbind(namekey, advice[,2])
advice_edge3 = cbind(namekey, advice[,3])
advice_edge4 = cbind(namekey, advice[,4])
advice_edge5 = cbind(namekey, advice[,5])

advice_edges = rbindlist(list(advice_edge1, advice_edge2, advice_edge3, advice_edge4, advice_edge5))

colnames(advice_edges) = c("advisee", "advisor")
advice_edges = advice_edges[advisor != ""]

advice_seeking = graph.data.frame(advice_edges, directed = TRUE)
advice_seeking = simplify(advice_seeking, remove.loops = TRUE)

advice_matrix = get.adjacency(advice_seeking, sparse = FALSE)
advice_matrix = advice_matrix[sort(rownames(advice_matrix)),sort(colnames(advice_matrix))]

advice_seeking = graph.adjacency(as.matrix(advice_matrix), "directed")

# checking out demographics and homophily/sorting with ergm
demos = data.table(as.character(namekey$Name), data[,(ncol(data) - 15):(ncol(data) - 1)])
colnames(demos) = c("id", "age", "gender", "ethnicity", "married", "degree", "prev_ind", "years_exp", "future_ind", "entrepreneur", "hobby", "car", "pets", "neighborhood", "prev_lived", "childhood_lived")

# issue from class occurs through setkey sorting a numeric id rather than a character one
# could use
#trust_demos[order(sort(as.character(trust_demos$id)))]
# after setting up trust_demos object

# can also convert to character beforehand, and setkey will sort using character rather than numeric sorting
# also saves having to sort 3 separate times
# in the cases below, since we have the same # in each network, we don't need the separate objects--writing out in full for cases where different #s of nodes would be possible 


setkey(demos, id)

trust_demos = data.table(id = V(trust)$name, key = "id")
trust_demos = merge(trust_demos, demos, all.x = TRUE)

distrust_demos = data.table(id = V(distrust)$name, key = "id")
distrust_demos = merge(distrust_demos, demos, all.x = TRUE)

advice_demos = data.table(id = V(advice_seeking)$name, key = "id")
advice_demos = merge(advice_demos, demos, all.x = TRUE)

vertex_attributes = function (graph, D) {
    if (!is.igraph(graph)) 
        stop("Not a graph object")
    if (!is.data.frame(D)) 
        stop("Not a dataframe")
    for (i in colnames(D)) graph = set.vertex.attribute(graph, 
        i, value = D[, i])
    return(graph)
  }

trust = vertex_attributes(trust, as.data.frame(trust_demos))
advice_seeking = vertex_attributes(advice_seeking, as.data.frame(advice_demos))

# gender
colrange = c("white", "light blue", "light green")
V(trust)$color = colrange[as.factor(V(trust)$gender)]

pdf(file = "gender_trust.pdf")
plot.igraph(trust,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

pdf(file = "gender_advice.pdf")
V(advice_seeking)$color = colrange[as.factor(V(advice_seeking)$gender)]
plot.igraph(advice_seeking,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()


# age
colrange = rainbow(max(demos$age, na.rm = TRUE))

V(trust)$color = colrange[V(trust)$age]

pdf(file = "age_trust.pdf")
plot.igraph(trust,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()


V(advice_seeking)$color = colrange[V(advice_seeking)$age]

pdf(file = "age_advice.pdf")
plot.igraph(advice_seeking,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(advice_seeking)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()


# ethnicity
colrange = sample(colors(distinct = TRUE), length(unique(demos$ethnicity)))

pdf(file = "ethnicity_trust.pdf")
V(trust)$color = colrange[as.factor(V(trust)$ethnicity)]
plot.igraph(trust,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

pdf(file = "ethnicity_advice.pdf")
V(advice_seeking)$color = colrange[as.factor(V(advice_seeking)$ethnicity)]
plot.igraph(advice_seeking,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

# previous industry 
colrange = sample(colors(distinct = TRUE), length(unique(demos$prev_ind)))

pdf(file = "prev_ind_trust.pdf")
V(trust)$color = colrange[as.factor(V(trust)$prev_ind)]
plot.igraph(trust,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

pdf(file = "prev_ind_advice.pdf")
V(advice_seeking)$color = colrange[as.factor(V(advice_seeking)$prev_ind)]
plot.igraph(advice_seeking,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

# future industry 
colrange = sample(colors(distinct = TRUE), length(unique(demos$future_ind)))

pdf(file = "future_ind_trust.pdf")
V(trust)$color = colrange[as.factor(V(trust)$future_ind)]
plot.igraph(trust,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

pdf(file = "future_ind_advice.pdf")
V(advice_seeking)$color = colrange[as.factor(V(advice_seeking)$future_ind)]
plot.igraph(advice_seeking,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

# hobby
colrange = sample(colors(distinct = TRUE), length(unique(demos$hobby)))

pdf(file = "hobby_trust.pdf")
V(trust)$color = colrange[as.factor(V(trust)$hobby)]
plot.igraph(trust,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

pdf(file = "hobby_advice.pdf")
V(advice_seeking)$color = colrange[as.factor(V(advice_seeking)$hobby)]
plot.igraph(advice_seeking,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

# neighborhood
colrange = sample(colors(distinct = TRUE), length(unique(demos$neighborhood)))

pdf(file = "neighborhood_trust.pdf")
V(trust)$color = colrange[as.factor(V(trust)$neighborhood)]
plot.igraph(trust,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

pdf(file = "neighborhood_advice.pdf")
V(advice_seeking)$color = colrange[as.factor(V(advice_seeking)$neighborhood)]
plot.igraph(advice_seeking,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

# previous region lived
colrange = sample(colors(distinct = TRUE), length(unique(demos$prev_lived)))

pdf(file = "prev_region_trust.pdf")
V(trust)$color = colrange[as.factor(V(trust)$prev_lived)]
plot.igraph(trust,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

pdf(file = "prev_region_advice.pdf")
V(advice_seeking)$color = colrange[as.factor(V(advice_seeking)$prev_lived)]
plot.igraph(advice_seeking,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

# childhood region lived
colrange = sample(colors(distinct = TRUE), length(unique(demos$childhood_lived)))

pdf(file = "childhood_region_trust.pdf")
V(trust)$color = colrange[as.factor(V(trust)$childhood_lived)]
plot.igraph(trust,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()

pdf(file = "childhood_region_advice.pdf")
V(advice_seeking)$color = colrange[as.factor(V(advice_seeking)$childhood_lived)]
plot.igraph(advice_seeking,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
dev.off()


library(ergm)
# using a random graph model to analyze sorting and relationship formation
trust_net = network(trust_matrix)
distrust_net = network(distrust_matrix)
advice_net = network(advice_matrix)

# set attributes
for(i in seq_along(colnames(trust_demos))[-1]){
	network::set.vertex.attribute(trust_net, colnames(demos)[i], as.data.frame(demos)[,i])
}

for(i in seq_along(colnames(distrust_demos))[-1]){
    network::set.vertex.attribute(distrust_net, colnames(demos)[i], as.data.frame(demos)[,i])
}

for(i in seq_along(colnames(advice_demos))[-1]){
	network::set.vertex.attribute(advice_net, colnames(demos)[i], as.data.frame(demos)[,i])
}

library(sna)

# likelihood of any two ties existing
summary(ergm(trust_net ~ edges))
summary(ergm(distrust_net ~ edges))
summary(ergm(advice_net ~ edges))

# same as the density of the network
exp(0.4385)/(1 + exp(0.4385))
gden(trust_net)

exp(-2.8904)/(1 + exp(-2.8904))
gden(distrust_net)

exp(-1.0986)/(1 + exp(-1.0986))
gden(advice_net)


# effect of reciprocation on likelihood of a tie
summary(ergm(trust_net ~ edges + mutual))
summary(ergm(distrust_net ~ edges + mutual))
summary(ergm(advice_net ~ edges + mutual))

# less ties reciprocated than by chance
# -inf means that no distrust ties are reciprocated

# modeling on trait characteristics
summary(ergm(trust_net ~ edges  + mutual + nodematch("gender") + nodematch("age") + nodematch("ethnicity")))
summary(ergm(distrust_net ~ edges  + mutual + nodematch("gender") + nodematch("age") + nodematch("ethnicity")))
summary(ergm(advice_net ~ edges +  mutual + nodematch("gender") + nodematch("age") + nodematch("ethnicity")))

# industry characteristics
summary(ergm(trust_net ~ edges + mutual + nodematch("prev_ind") + nodematch("future_ind") + nodematch("entrepreneur")))
summary(ergm(distrust_net ~ edges + mutual + nodematch("prev_ind") + nodematch("future_ind") + nodematch("entrepreneur")))
summary(ergm(advice_net ~ edges + mutual + nodematch("prev_ind") + nodematch("future_ind") + nodematch("entrepreneur")))

# personal characteristics
summary(ergm(trust_net ~ edges + mutual + nodematch("hobby") + nodematch("neighborhood") + nodematch("prev_lived")))
summary(ergm(distrust_net ~ edges + mutual + nodematch("hobby") + nodematch("neighborhood") + nodematch("prev_lived")))
summary(ergm(advice_net ~ edges + mutual + nodematch("hobby") + nodematch("neighborhood") + nodematch("prev_lived")))


# overlap between the networks
summary(ergm(trust_net ~ edges + edgecov(network(advice_matrix))))
summary(ergm(distrust_net ~ edges + edgecov(network(advice_matrix))))

# more likely to get tie in trust people you go to for advice 
# -inf means that no one that goes to someone for advice also distrusts that person

# all characteristics
summary(ergm(trust_net ~ edges + mutual + nodematch("gender") + nodematch("age") + nodematch("ethnicity") + nodematch("prev_ind") + nodematch("entrepreneur") + nodematch("future_ind") + nodematch("hobby") + nodematch("neighborhood") + nodematch("prev_lived")))

summary(ergm(distrust_net ~ edges + mutual + nodematch("gender") + nodematch("age") + nodematch("ethnicity") + nodematch("prev_ind") + nodematch("entrepreneur") + nodematch("future_ind") + nodematch("hobby") + nodematch("neighborhood") + nodematch("prev_lived")))

summary(ergm(advice_net ~ edges + mutual + nodematch("gender") + nodematch("age") + nodematch("ethnicity") + nodematch("prev_ind") + nodematch("entrepreneur") + nodematch("future_ind") + nodematch("hobby") + nodematch("neighborhood") + nodematch("prev_lived")))

trust_ergm = ergm(trust_net ~ edges + mutual + nodematch("gender") + nodematch("age") + nodematch("ethnicity") + nodematch("prev_ind") + nodematch("entrepreneur") + nodematch("future_ind") + nodematch("hobby") + nodematch("neighborhood") + nodematch("prev_lived"))

distrust_ergm = ergm(distrust_net ~ edges + mutual + nodematch("gender") + nodematch("age") + nodematch("ethnicity") + nodematch("prev_ind") + nodematch("entrepreneur") + nodematch("future_ind") + nodematch("hobby") + nodematch("neighborhood") + nodematch("prev_lived"))

advice_ergm = ergm(advice_net ~ edges + mutual + nodematch("gender") + nodematch("age") + nodematch("ethnicity") + nodematch("prev_ind") + nodematch("entrepreneur") + nodematch("future_ind") + nodematch("hobby") + nodematch("neighborhood") + nodematch("prev_lived"))

# taken together, results potentially suggest antagonism among the class in terms of competitiveness among students that are applying for limited positions available for the same kinds of jobs

# checking simulation goodness of fit--want mean values produced by the simulation to be close to the observed values and p-values to not be significant

# for plots, want trends in lefthand panel to be relatively stable over time and distributions on righthand side to be normal
# plot functionality may require package "latticeExtra"
gof(trust_ergm, GOF=~model)
plot(trust_ergm$sample)

gof(distrust_ergm, GOF=~model)
plot(distrust_ergm$sample)

gof(advice_ergm, GOF=~model)
plot(advice_ergm$sample)