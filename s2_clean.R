options(stringsAsFactors = FALSE)
crandata <- read.csv("cran_data.csv")



require(stringr)
require(plyr)


genRel <- function(target, home) {
    return(ldply(target, function(x) { data.frame(home = c(home), target = x)}))
}


createRelationship <- function(row) {
    row <- unlist(row)
    rel <- lapply(strsplit(row[c("Depends", "Imports", "Suggests")], ","), function(x) { str_trim(str_replace(x, "\\(.+\\)" , "")) })
    return(ldply(rel, genRel, home = row['package_name']))
}

edgelist <- do.call("rbind", apply(crandata, 1, createRelationship))
edgelist <- edgelist[edgelist$target != "R",]
edgelist <- edgelist[edgelist$target != edgelist$home,] ##Removal of loop

basePkg <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk")

nrow(edgelist)

edgelist <- edgelist[(!edgelist$home %in% basePkg) & (!edgelist$target %in% basePkg),]

edgelist <- edgelist[,c(2,3,1)]
colnames(edgelist)[3] <- "RelType"

saveRDS(edgelist, "edgelist.RDS")

require(igraph)
crannetwork <- graph.data.frame(edgelist, directed = TRUE)
edgeWeight <- E(crannetwork)$RelType
edgeWeight[edgeWeight == "Depends" | edgeWeight == "Imports"] <- 3
edgeWeight[edgeWeight == "Suggests"] <- 1
edgeWeight <- as.numeric(edgeWeight)
prCran <- page.rank(crannetwork, weights = edgeWeight)$vector

cranevc <- evcent(crannetwork)$vector
plottopkgraph <- function(crannetwork, k, cranevc, sizingFactor, ...) {
    vname <- get.vertex.attribute(crannetwork, name = "name")
    top50 <- names(cranevc[order(cranevc, decreasing = TRUE)][1:k])
    eigen <- cranevc[order(cranevc, decreasing = TRUE)]
    top50graph <- induced.subgraph(crannetwork, match(top50, vname))
    V(top50graph)$label.cex <- 0.6
    plot(top50graph, vertex.size = eigen[match(V(top50graph)$name, names(eigen))] * sizingFactor , edge.arrow.size = 0.2, ...)
}

plottopkgraph(crannetwork, 100, prCran, 50, layout = layout.kamada.kawai)

