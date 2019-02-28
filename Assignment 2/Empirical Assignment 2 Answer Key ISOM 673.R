## Preqin venture deals through late July 2014
rm(list = ls(all = TRUE))
setwd("~/Dropbox")
setwd("C:/Users/dplewi3/Dropbox")

library(data.table)
library(igraph)
library(stringr)
library(splitstackshape)
library(plotly)
library(network)

############## Data prep

# key insight -- ignore the startups, investor-investor relationships are given in investor column of affiliation edge list. from here, follow the same procedure as for the advice networks from exercise 1 to make the investor-investor edge list

# making monthly investor network
investments1 = fread(file = "Funding_events_7.14.csv", header = TRUE)
investments2 = fread(file = "Funding_events_7.14_page2.csv", header = TRUE)

investments = rbindlist(list(investments1, investments2))

# getting months
investments[, date := as.numeric(as.Date(investments$"Deal Date", "%m/%d/%y"))]

# center minimum date to = 0
investments[, date := date - min(date)]

# convert to months, counting from 0
investments[, month := floor(date/30)]

# focus on investors column, maintain month to track network over time
investors = data.table(investors = investments$Investors, month = investments$month)

# split out investors into their own separate columns to make an edge list. can use commas to separate, but need to be careful of investors with suffixes like inc, ltd, llc, and others. use splitstackshape's cSplit to make this simpler
investors = cSplit(investors, "investors", ",")

# warnings should be okay but you can avoid by replacing any non UTF-8 by ''

# now no warnings
investors = data.table(investors = investments$Investors, month = investments$month)
Encoding(investors$investors) = "UTF-8"
investors[, investors := iconv(investors, "UTF-8", "UTF-8",sub='')]


# can set up an edge list using a similar approach as the advice network method from the in-class exercise on the classroom network
# key for this method is to combine that setup with the t(combn()) method from exercise 1 question 2 (question on checking for strong/weak ties)

investors = t(cSplit(investors, "investors", ","))

# get all pairs
possible_pairs = lapply(seq_len(ncol(investors)), function(i) investors[,i])

# note the differennce in speed between the above and this:
investors = data.table(investors = investments$Investors, month = investments$month)
investors = cSplit(investors, "investors", ",")
possible_pairs = lapply(seq_len(nrow(investors)), function(i) t(investors[i,]))
# helpful to avoid vectoring on rows with apply functions when possible

# get rid of blanks to make the object smaller
for(i in seq_along(possible_pairs)){
	possible_pairs[[i]] = possible_pairs[[i]][!is.na(possible_pairs[[i]])]
}

# ignore funding events  where there is 
# tryCatch error handling isn't totally necessary here but can be good to see how it works
# other approach is similar to the one from exercise 2, using something similar to the steps below, that got rid of entries where there was only one ego
# seqs641_full_strong[, min_two := .N > 1, by = ego]
# s641_full_strong = s641_full_strong[min_two==TRUE]
# using error handling is a little more direct for this case because we can force R to return an already-cleaned object when it encounters the error we wrote into the command (< 2 investors)
edges = lapply(seq_along(possible_pairs), function(i) tryCatch(cbind(possible_pairs[[i]][1], t(combn(possible_pairs[[i]][-1], 2))), error = function(e) NULL))

# note that this step isn't necessary at all, but is here just so we can refer to the columns by name instead of by index # to make the code easier to read
# data frame would serve the same function
# calling data table on this just so we can refer to column names and make the rest of the code more interpetable
# main issue is that it's not possible to call $ on matrix-type objects, even if they have explicit column names
edges = data.table(do.call(rbind, edges))

colnames(edges) = c("month", "from", "to")

# keeping track of our string splitting from before, we want to account for when commas are included in the name
# we're worried mostly about strings of length 3 or 4
unique(c(edges$from[nchar(edges$from) < 5], edges$to[nchar(edges$to) < 5]))

# after a quick scan, let's say that the problem strings are <NA>, Ltd., Inc., LLC, Inc, LP, LLC., Ltd, L.P., S.A, Corp, a.s., llc, S.A., and LTD

# removing rows where either entry in the edge list doesn't represent an actual company
edges = edges[from != "<NA>" & from != "Ltd." & from != "Inc." & from != "LLC" & from != "Inc" & from != "LP" & from != "LLC." & from != "Ltd" & from != "L.P." & from != "S.A" & from != "Corp" & from != "a.s." & from != "llc" & from != "S.A." & from != "LTD" & to != "<NA>" & to != "Ltd." & to != "Inc." & to != "LLC" & to != "Inc" & to != "LP" & to != "LLC." & to != "Ltd" & to != "L.P." & to != "S.A" & to != "Corp" & to != "a.s." & to != "llc" & to != "S.A." & to != "LTD" ,]

# now let's make a network for each month

# not totally necessary, but just lets us do this without having to actually calculate what the last month is
edges[, month := as.numeric(month)]
july_2014 = max(edges$month)

# note that the months start at 0 and the index sequence starts at 1, so we use strictly <
edges_monthly = lapply(seq_len(july_2014), function(i) edges[edges$month < i])

# feed these into igraph -- will use the full network but simplified is also okay
vcnets = lapply(seq_along(edges_monthly), function(i) graph.data.frame(edges_monthly[[i]][,-1], directed = FALSE))
vcnets_simple = lapply(seq_along(vcnets), function(i) simplify(vcnets[[i]], remove.multiple = TRUE))

############## Question 1
 
####### Part A

# key insight -- setting up our monthly networks in a list as above make for quick calculations on the months we're interested in using the built-in function in igraph, because we avoid having to make matrices or re-generate a network for each step

# center of the network in the same terms as the Hollywood Actor example is VC firm with highest closeness
closeness(vcnets[[july_2014]])[which.max(closeness(vcnets[[july_2014]]))]

####### Part B

# can also do part A with the "bacon" numbers, that is, lowest average shortest path distance
# all shortest paths 
distances = distances(vcnets[[july_2014]])

# replace unreachable firms with just the size of the network to get a real number solution--infinity also an acceptable answer for the avearage distance
distances[distances == Inf] = nrow(distances)

# average shortest path length for each firm -- the "bacon" number
avg_dist_firm = apply(distances, 1, mean)

# firm with the smallest average shortest path length
avg_dist_firm[which.min(avg_dist_firm)]

####### Part C

# average shortest path length for all firms
mean(avg_dist_firm)

# number is so high because many firms are not reachable from one another
distances = distances(vcnets[[july_2014]])
sum(distances(vcnets[[july_2014]]) == Inf)/2

# tells us that there are 6029049 unreachable pairs in the network, so this will inflate this shortest path calculation by a lot even if a lot of the distances are very small

# note that if we included isolates, the distances would grow even larger



############## Question 2
 
####### Part A
# key insight -- same as above, having a list of networks to work from makes the calculations quicker and can be done in igraph
# compute mean coreness for each month
mean_coreness = lapply(seq_along(vcnets), function(i) mean(coreness(vcnets[[i]])))

# can check this against simplified version
mean_coreness_simple = lapply(seq_along(vcnets), function(i) mean(coreness(vcnets_simple[[i]])))

data = as.data.frame(list(x = seq_along(mean_coreness), y = do.call(rbind, mean_coreness)))

# plot
p = plot_ly(data, x = ~x, y = ~y, color = I("gray50"), type = "scatter", mode = "lines") %>%
	layout(yaxis = list(title = "Average coreness")) %>%
	layout(xaxis = list(title = "Months"))
export(p, file = "coreness_over_time.pdf")

####### Part B

# key insight -- similar to exercise 1, edge lists are easier to work with than matrices when we need to keep track of attributes of the edges

# modify edges_monthly to remove ties if they are at least 120 months old and have not been renewed
# subtracting 1 from i lines up the index sequence to months' starting at 0
edges_monthly_decay = lapply(seq_along(edges_monthly), function(i) edges_monthly[[i]][, tie_age := i - 1 - month])
edges_monthly_decay = lapply(seq_along(edges_monthly), function(i) edges_monthly_decay[[i]][tie_age < 121,])

vcnets_decay = lapply(seq_along(edges_monthly), function(i) graph.data.frame(edges_monthly_decay[[i]][,-(3:4)], directed = FALSE))

# same plot as in part A
mean_coreness_decay = lapply(seq_along(vcnets_decay), function(i) mean(coreness(vcnets_decay[[i]])))
data_decay = as.data.frame(list(x = seq_along(mean_coreness_decay), y = do.call(rbind, mean_coreness_decay)))

# plot
p = plot_ly(data_decay, x = ~x, y = ~y, color = I("gray50"), type = "scatter", mode = "lines") %>%
	layout(yaxis = list(title = "Average coreness")) %>%
	layout(xaxis = list(title = "Months"))
export(p, file = "coreness_over_time_decay.pdf")
# looks similar 

# not a huge difference, suggests that working relationships might tend to persist
t.test(data[,2], data_decay[,2])

############## Question 3

# key insight -- can use variety of measures to talk about core/periphery structure, including 

# 1. show development of core/periphery visually using a network plot
# can compare early plots to late plots using coreness as a color, e.g.,
june_1982 = 12*1
june_1986 = 12*5
june_1991 = 12*10
june_1996 = 12*15
june_2001 = 12*20
june_2006 = 12*25


par(mfrow=c(2,2), mar = c(0,0,0,0)) 
plot(vcnets[[12*1]],vertex.label = NA, vertex.size = 4, vertex.color = rainbow(max(coreness(vcnets[[june_1982]])))[coreness(vcnets[[12*1]])], layout = layout.fruchterman.reingold)
plot(vcnets[[june_1986]],vertex.label = NA, vertex.size = 4, vertex.color = rainbow(max(coreness(vcnets[[june_1986]])))[coreness(vcnets[[june_1986]])], layout = layout.fruchterman.reingold)
plot(vcnets[[12*10]],vertex.label = NA, vertex.size = 4, vertex.color = rainbow(max(coreness(vcnets[[june_1991]])))[coreness(vcnets[[12*10]])], layout = layout.fruchterman.reingold)
plot(vcnets[[june_1996]],vertex.label = NA, vertex.size = 4, vertex.color = rainbow(max(coreness(vcnets[[june_1996]])))[coreness(vcnets[[june_1996]])], layout = layout.fruchterman.reingold)
dev.off()
# can see development of dense core over time
# plots will get slow after 15 years but will still plot if given time

# 2. develop a plot or descriptive statistics of how many nodes are included in different levels of k-cores
# can look at distribution of coreness to see if it is evenly dispersed or not
table(coreness(vcnets[[june_1982]]))
table(coreness(vcnets[[june_1986]]))
table(coreness(vcnets[[june_1991]]))
table(coreness(vcnets[[june_1996]]))
table(coreness(vcnets[[june_2001]]))
table(coreness(vcnets[[june_2006]]))

# firms become members of higher-degree k-cores over time -- 62 firms in k-core with degree 77 in 2006


# 3. can look at the distribution of closness, in the sense that if many firms have very high coreness and some very few firms have very low coreness then that would also suggest that many firms are in the core and then a few are scattered in the periphery
par(mfrow=c(2,3), mar = c(0,0,0,0)) 
hist(closeness(vcnets[[june_1982]]))
hist(closeness(vcnets[[june_1986]]))
hist(closeness(vcnets[[june_1991]]))
hist(closeness(vcnets[[june_1996]]))
hist(closeness(vcnets[[june_2001]]))
hist(closeness(vcnets[[june_2006]]))
dev.off()

# can see split of closeness statistics emerge over time

# 4. perform a multidimensional scaling on the distance between to see if many firms appear in the center of the plot and there are some other firms scattered, maybe in a ring, etc. outside of this
dist = distances(vcnets[[june_1982]])
dist[dist == Inf] = nrow(dist)
mdscale1 = cmdscale(dist, k = 2)
dist = distances(vcnets[[june_1986]])
dist[dist == Inf] = nrow(dist)
mdscale5 = cmdscale(dist, k = 2)
dist = distances(vcnets[[june_1991]])
dist[dist == Inf] = nrow(dist)
mdscale10 = cmdscale(dist, k = 2)
dist = distances(vcnets[[june_1996]])
dist[dist == Inf] = nrow(dist)
mdscale15 = cmdscale(dist, k = 2)

par(mfrow=c(2,2), mar = c(0,0,0,0)) 
plot(mdscale1)
plot(mdscale5)
plot(mdscale10)
plot(mdscale15)
dev.off()

# actually hard to see on the plot, but look at the coordinates from the last scaling
mdscale15

# many firms located on top of each other in same location, and a few firms scattered outside of this region

# 5. decompose the network into its largest connected component--the “giant component”--and see if this component contains most of the firms in the network
# get components and compare size of largest component to number of firms in the network
components = clusters(vcnets[[june_1991]])
max(components$csize)/vcount(vcnets[[june_1991]])

components = clusters(vcnets[[june_1996]])
max(components$csize)/vcount(vcnets[[june_1996]])
# percentage jumps over 2x from 1991 to 1996

# from the above, some of these may be more or less memory intensive, and may make sense on larger or smaller networks

############## Question 4
 
####### Data prep

# successful investments 
outcomes = fread("Venture_capital_firm_outcomes.csv", head = TRUE)

# get network info into the outcomes table

# can use this function again
getNetStats=function(net)
{
  deg = degree(net, mode = "total")
  close= closeness(net, mode = "total")
  betw = betweenness(net)
  prank = page_rank(net)$vector # page_rank creates a list object with some other items in it, we just want the actual scores, which are contained in the object called "vector"
  id=V(net)$name
  stats= as.data.table(list(firm_name = id, deg = deg, close = close, betw = betw, prank = prank))
  return(stats)
}

# note that only need yearly stats, so can just take one stat per year from 1981-2014
investments[which.min(date)]
# network begins in June 1981

index = seq(min(edges$month), max(edges$month), 12) + 1 # again offsetting since months start at 0
year_nets = vcnets[index]
vc_centralities = lapply(seq_along(year_nets), function(i) getNetStats(year_nets[[i]]))

# add the years
for(i in seq_along(vc_centralities)){
	vc_centralities[[i]][, year := seq(1981,2014)[i]]
}
vc_centralities = rbindlist(vc_centralities)

# merge together
setkeyv(outcomes, c("firm_name", "year"))
setkeyv(vc_centralities, c("firm_name", "year"))

outcomes = merge(outcomes, vc_centralities)

####### Part A

# successful investments is somewhat right-skewed, so can use quantile regression adopted for counts to estimate the median of successful investments, rather than the mean as in a normal regression
hist(outcomes$successful_investments)

library(Qtools)

rq.counts(successful_investments ~ close + year, data = outcomes, tau = .5, M = 100)
rq.counts(successful_investments ~ deg + year, data = outcomes, tau = .5, M = 100)
rq.counts(successful_investments ~ betw + year, data = outcomes, tau = .5, M = 100)
rq.counts(successful_investments ~ prank + year, data = outcomes, tau = .5, M = 100)
# if the models return errors about singular designs, can re-run until they converge

# closeness not related to more succesful investments, but other types of centrality seem to be

####### Part B

# a firm can only go out of business once, and its likelihood of going out of business is also related to how long it has been in operation
# can use the set of "survival", or "hazard" models to estimate these kinds of failure events effectively

# a simple survival model is a discrete-time model where we estimate a logit on going out of business, controlling for the tenure-specific rate of failure for any firm that has been in operation for a tenure of x years
outcomes[, tenure := year - first_investment_year]

# can also include some controls for industry and location
summary(glm(out_of_business ~ close + year + factor(venture_firm_industry) + factor(venture_firm_location) + factor(tenure), data = outcomes, family = "binomial"))
summary(glm(out_of_business ~ deg + year + factor(venture_firm_industry) + factor(venture_firm_location) + factor(tenure), data = outcomes, family = "binomial"))
summary(glm(out_of_business ~ betw + year + factor(venture_firm_industry) + factor(venture_firm_location) + factor(tenure), data = outcomes, family = "binomial"))
summary(glm(out_of_business ~ prank + year + factor(venture_firm_industry) + factor(venture_firm_location) + factor(tenure), data = outcomes, family = "binomial"))

# all types of centrality are related to being less likely to go out of business, in varying degrees