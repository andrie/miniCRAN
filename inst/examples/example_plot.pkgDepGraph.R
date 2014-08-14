tags <- "chron"

# Plot using defaults
dg <- makeDepGraph(tags, includeBasePkgs=FALSE, suggests=TRUE, enhances=TRUE)

set.seed(42); 
plot(dg)

# Move edge legend to top left
set.seed(42); 
plot(dg, legendPosEdge=c(-1, 1))

# Change font size and shape size
set.seed(42); 
plot(dg, legendPosEdge=c(-1, 1), vertex.size=20,  cex=0.5)


# Move vertex legend to top right
set.seed(42); 
plot(dg, legendPosEdge=c(-1, 1), legendPosVertex=c(1, 1), vertex.size=20,  cex=0.5)

