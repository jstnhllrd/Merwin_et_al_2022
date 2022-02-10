#Create butterfly phylogeny
library(phytools)

#List of names for the terminal nodes.
tip.labels <- c("A. mormo", "F. tarquinius", "S. walbum", "C. irus", "C. gryneus", "C. niphon", "G. piasus", "G. lygdamus", "A. glandon")

#Matrix to specify tree topology.
edge <- matrix(c(10,1,
                 10,11,
                 11,2,
                 11,12,
                 12,13,
                 13,3,
                 13,14,
                 14,4,
                 14,15,
                 15,5,
                 15,6,
                 12,16,
                 16,17,
                 17,7,
                 17,8,
                 16,9), byrow=TRUE, ncol=2)

#List of edge/branch lengths for the tree created above. The edge lengths correspond directly to the node layout above.
edge.length <-c(89.41,
                19.41,
                70,
                70-49.24,
                49.24-46.86,
                46.86,
                46.86-15.47,
                15.47,
                15.47-14.37,
                14.37,
                14.37,
                49.24-37.27,
                37.27-13.88,
                13.88,
                13.88,
                37.27)

#Specify the number of internal nodes.
Nnode <- 8

#Create the tree
bfly.tree <- list(edge=edge, Nnode=Nnode, tip.label=tip.labels, edge.length=edge.length)

#Specify the tree as a 'phylo' class object.
class(bfly.tree) <- 'phylo'

#Plot the tree and label features.
plot(bfly.tree)
tiplabels()
nodelabels()
edgelabels(bfly.tree$edge.length, bg="black", col="white", font=2)


#Export the tree to your local directory in Newick format.
bfly.tree.nwk <- write.tree(bfly.tree, file = "./bfly.tree.nwk")
