# http://www.faculty.ucr.edu/~hanneman/nettext/ - online book on sna
# http://sachaepskamp.com/files/Cookbook.html
# http://statmath.wu.ac.at/research/friday/resources_WS0708_SS08/igraph.pdf
# https://www.google.nl/url?sa=t&rct=j&q=&esrc=s&source=web&cd=15&ved=0ahUKEwid-uzbtsjVAhURbVAKHbv6Cug4ChAWCEQwBA&url=https%3A%2F%2Fwww.jstatsoft.org%2Farticle%2Fview%2Fv024i06%2Fv24i06.pdf&usg=AFQjCNFd8gj8WpyNJ6ECzEUSh54v9bi0Bg
# http://www.bojanorama.pl/snar:start

# https://www.r-bloggers.com/network-analysis-part-1-exercises/
# http://www.r-exercises.com/2016/09/15/network-analysis-part-1-solutions/
# https://www.r-bloggers.com/network-analysis-part-2-exercises/
# http://www.r-exercises.com/2016/10/02/network-analysis-part-2-solutions/
# https://www.r-bloggers.com/network-analysis-part-3-exercises/
# http://www.r-exercises.com/2016/10/20/network-analysis-part-3-solutions/

rm(list = ls(all=T))

# ?Rprofile
.libPaths('D:/R/library')
.libPaths() # Defined in Rprofile

setwd("D:/Google Drive/PRIDE/R")
getwd()
load("D:/Google Drive/PRIDE/R/sna.alex.RData")
# save(list=ls(all=TRUE), file="D:/Google Drive/PRIDE/R/sna.alex.RData") # save RDATA for later use

library(igraph) # Make sure that the Windows environment settings are set correctly for Java!
# library(sna)
# library(network)
# detach("package:sna", unload=TRUE)
# detach("package:network", unload=TRUE)

### PART 1 ####

# Exercise 1. Create an empty directed graph with 5 nodes. Set color of all nodes to yellow and shape to sphere

g <- make_empty_graph(n=5, directed=TRUE)
g
str(g)
V(g)
V(g)$color = "yellow" # vertices/nodes of a graph
V(g)$shape = "sphere" # shape of nodes
str(g)
 
# Exercise 2. Add the following edges to the graph: 1->2, 1->3, 2->4, 3->4, 4->5

g <- add.edges(g, c(1,2, 1,3, 2,4, 3,4, 4,5))
plot(g)
 
# Exercise 3. Add a vertex to the graph, color it to red and add edges: 3->6, 6->5. Set vertex shape to sphere.

g <- add.vertices(g, 1, color="red", shape="sphere")
g <- add.edges(g, c(3,6, 6,5))
plot(g)
 
# Exercise 4. Replace edge 1->3 with the edge 3->1.

e <- E(g)
str(e)
e
g <- delete.edges(g, c(2)) # delete second edge
plot(g)
g <- add.edges(g, c(3,1))
plot(g)

# Exercise 5. Name vertices with letters A-F. List all vertices and edges.

LETTERS[1:6]
letters[1:6]
str(g)
summary(g)

V(g)$name <- LETTERS[1:6]
V(g)
plot(g)

E(g)
 
# Exercise 6. What is the shortest path between vertices A and E.

shortest_paths(g, "A", "E", output="epath")$epath[1]
e <- shortest_paths(g, "A", "E", output="epath")$epath[1]
e

# Exercise 7. Plot the graph. The size of a vertex should depend on the number of incoming edges.

plot(g)
plot(g, layout=layout_nicely, vertex.size=degree(g, V(g), "in")*15+15, vertex.label.dist=0.5, edge.arrow.size=0.5) # degree = number of its adjacent edges, mode = "in" --> receiving relationships
 
# Exercise 8. Plot the distribution of vertices degrees.

degree(g, V(g), "in")
degree(g, V(g), "total")
plot(degree_distribution(g), main="Degree distribution", xlab="Degree", ylab="Frequency")
 
# Exercise 9. Create a heatmap from the data.

pal <- colorRampPalette(c("lightblue", "blue"))
pal
plot(g, layout=layout_nicely, vertex.size=degree(g, V(g), "in")*15+15, vertex.label.dist=0.5, edge.arrow.size=0.5)
get.adjacency(g)
a <- as.matrix(get.adjacency(g))
a
heatmap(a, Rowv=NA, Colv="Rowv", col=pal(100))

# Exercise 10. Create graph from subset of vertices with 1 or more incoming edges and plot it. Set the vertice shape to green box. The size of a vertice should depend on the number of outcoming edges.

sg <- induced_subgraph(g, V(g)[degree(g, V(g), "in") >= 1])
sg
plot(sg, layout=layout_nicely, vertex.size=degree(sg, V(sg), "out")*10+15, vertex.color="green", vertex.shape="square", vertex.label.dist=0.5, edge.arrow.size=0.5)

### PART 2 ####

# A number of employees in a factory was interviewed on a question:
# "Do you like to work with your co-worker?".
# Possible answers are 1 for yes and 0 for no. Each employee gave an answer for each other employee thus creating an adjecancy matrix. You can download the dataset from here https://www.r-bloggers.com/network-analysis-part-2-exercises/

# Exercise 1. Load the data and create an unweighted directed graph from the adjecancy matrix.
# Name the nodes as letters A to Y. Set node color to yellow and shape to sphere.
# Set the edge's color to gray and arrow size to 0.2.

d <- read.csv("sociogram-employees-un.csv", header=FALSE)
d <- read.csv("D:/Google Drive/PRIDE/R/sociogram-employees-un.csv", header=FALSE)
head(d); tail(d); dim(d) # 25 25
str(d)
g <- graph.adjacency(as.matrix(d), mode="directed")
g
V(g)$name <- LETTERS[1:NCOL(d)]
V(g)$color <- "yellow"
V(g)$shape <- "sphere"
E(g)$color <- "gray"
E(g)$arrow.size <- 0.2
plot(g)
 
# Exercise 2. Plot the graph.

plot(g)

# Exercise 3. Calculate network diameter and average closeness.

diameter(g)

closeness(g)
mean(closeness(g))

# Exercise 4. Calculate average network betweenness.

betweenness(g)
plot(betweenness(g))
mean(betweenness(g))
 
# Exercise 5. Calculate network density and average degree.

plot(g)
graph.density(g) # 0.4616667

degree(g, mode="all")
degree(g, mode="in")
degree(g, mode="out")
mean(degree(g, mode="all")) # 22.16

# Exercise 6. Calculate network reciprocity and average transitivity.

reciprocity(g) # Calculates the reciprocity of a directed graph.
transitivity(g) # Transitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.
mean(transitivity(g))

# Exercise 7. Calculate average eccentricity of the vertices. What is the average distance between two nodes?

eccentricity(g) # The eccentricity of a vertex is its shortest path distance from the farthest other node in the graph.
mean(eccentricity(g))

distance_table(g, directed = TRUE)
mean_distance(g)

# Exercise 8. Find the hubs and plot graph with node's size according to their hubs index. Which employee is the biggest hub?

hub.score(g)
hs <- hub.score(g)$vector
hs
plot(g, layout=layout.fruchterman.reingold, vertex.size=hs*25)

which.max(hs)
degree(g, mode="out")
 
# Exercise 9. Find the authorities and plot graph with node's size according to their authority index. Which employee is the biggest authority?

authority.score(g) # The authority scores of the vertices are defined as the principal eigenvector of t(A)*A, where A is the adjacency matrix of the graph.
as <- authority.score(g)$vector
as
plot(g, layout=layout_nicely, vertex.size=as*20)

which.max(as)
degree(g, mode="in")
 
# Exercise 10. Show the nodes that make diameter. Plot these nodes larger and in red. Plot edges on this path thicker in red.

degree(g, mode="all")
diameter.nodes <- get_diameter(g)
diameter.nodes <- get.diameter(g)
diameter.nodes

str(V(g))
V(g)$size <- 20 # define size as 20 to all nodes
V(g)[diameter.nodes]$color <- "red"
V(g)[diameter.nodes]$size <- V(g)[diameter.nodes]$size+10
E(g)$width <- 1
E(g, path=diameter.nodes)$color <- "red"
E(g, path=diameter.nodes)$width <- 10
plot.igraph(g, layout=layout.fruchterman.reingold)
plot.igraph(g, layout=layout_nicely)

### PART 3 - Less useful ####

# A number of employees in a factory was interview on question: "Do you like to work with your co-worker?". Possible answers are 1 for yes and 0 for no. Each employee gave answer for each other employee thus creating adjacency matrix. You can download data set from here.

# Exercise 1. Load the data and create un-directed graph from adjacency matrix. Name nodes as letters A to Y. Set node color to orange and shape to square. Set edge's color to blue and arrow size to 0.2. Plot the graph.

# d <- read.csv("sociogram-employees-un.csv", header=FALSE)
d <- read.csv("http://www.r-exercises.com/wp-content/uploads/2016/10/sociogram-employees-un.csv", header=FALSE)
d
g <- graph.adjacency(as.matrix(d), mode="undirected")
g
V(g)$name <- LETTERS[1:NCOL(d)]
V(g)$color <- "orange"
V(g)$shape <- "square"
E(g)$color <- "blue"
E(g)$arrow.size <- 0.2
plot(g)
 
# Exercise 2. Find the largest cliques in the group.

largest_cliques(g)
 
# Exercise 3. How many maximal cliques are there?

maximal.cliques.count(g)

# Exercise 4. Calculate the network cohesion.

cohesion(g)
 
# Exercise 5. Find the clusters based on betweenness.

cluster_edge_betweenness(g) 
 
# Exercise 6. Find the components of a graph.

components(g)
 
# Exercise 7. Find the loop edges.

E(g)[which_loop(g)==TRUE]
 
# Exercise 8. How many triangles are there in the graph? In how many of them is vertex S included?

sum(count_triangles(g))
count_triangles(g, V(g)['S'])
 
# Exercise 9. What is the global clustering coefficient of this network? Can we say that clustering is statistically significant for this network. Tip: in order to determine if clustering coefficient is significant, it should be much larger than the random network with the same number of vertices and edges.

transitivity(g, type="global") # 0.7145809
transitivity(sample_gnm(vcount(g), ecount(g)), type="global")

# No, the clustering is not significant, since clastering coefficient of network is just little greater than clustering coefficient of corresponding random network
 
# Exercise 10. Create and draw the following types of networks:   
# Random with 10 nodes and probability of an edge of 0.4 (Erdos-Renyi random graph)
plot(sample_gnp(10, 0.4))
# Full un-directed graph with 15 nodes
plot(make_full_graph(15))
# Star network with 20 nodes
plot(graph.star(20))
# Directed ring network with 15 nodes and mutual edges
g.ring <- make_ring(15, directed=TRUE, mutual=TRUE)
E(g.ring)$arrow.size <- 0.2
plot(g.ring)