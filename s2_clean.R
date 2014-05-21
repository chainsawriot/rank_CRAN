options(stringsAsFactors = FALSE)
crandata <- read.csv("cran_data.csv")



require(stringr)
require(plyr)


genRel <- function(target, home) {
    return(ldply(target, function(x) { data.frame(home = c(home), target = x)}))
}


createRelationship <- function(row) {
    row <- unlist(row)
    rel <- lapply(strsplit(row[c("Depends", "Imports", "Suggests", "Enhances")], ","), function(x) { str_trim(str_replace(x, "\\(.+\\)" , "")) })
    return(ldply(rel, genRel, home = row['package_name']))
}

edgelist <- do.call("rbind", apply(crandata, 1, createRelationship))
edgelist <- edgelist[edgelist$target != "R",]
edgelist <- edgelist[edgelist$target != edgelist$home,] ##Removal of loop

basePkg <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")

nrow(edgelist)

edgelist <- edgelist[(!edgelist$home %in% basePkg) & (!edgelist$target %in% basePkg),]

edgelist <- edgelist[,c(2,3,1)]
colnames(edgelist)[3] <- "RelType"

saveRDS(edgelist, "edgelist.RDS")

require(igraph)
crannetwork <- graph.data.frame(edgelist, directed = TRUE)
edgeWeight <- E(crannetwork)$RelType
edgeWeight[edgeWeight == "Depends" | edgeWeight == "Imports"] <- 3
edgeWeight[edgeWeight == "Suggests"| edgeWeight == "Enhances"] <- 1
edgeWeight <- as.numeric(edgeWeight)
prCran <- page.rank(crannetwork, weights = edgeWeight)$vector

names(head(prCran[order(prCran, decreasing = TRUE)], n = 15))

cranevc <- evcent(crannetwork, direct=TRUE, weights = edgeWeight)$vector
plottopkgraph <- function(crannetwork, k, cranevc, sizingFactor, labelk = NA, coloring = NA, ...) {
    vname <- get.vertex.attribute(crannetwork, name = "name")
    top50 <- names(cranevc[order(cranevc, decreasing = TRUE)][1:k])
    eigen <- cranevc[order(cranevc, decreasing = TRUE)]
    top50graph <- induced.subgraph(crannetwork, match(top50, vname))
    V(top50graph)$label.cex <- 0.6
    if (!is.na(labelk)) {
      toplabelk <- names(cranevc[order(cranevc, decreasing = TRUE)][1:labelk])
      curV <- V(top50graph)$name
      curV[!(curV %in% toplabelk)] <- NA
      print(curV)
    } else {
      curV <- V(top50graph)$name
    }
    if (!is.na(coloring)) {
      V(top50graph)$color <- rep("SkyBlue2", length(V(top50graph)$name))
      V(top50graph)$color[V(top50graph)$name %in% coloring] <- "red"
    }
    plot(top50graph, vertex.size = eigen[match(V(top50graph)$name, names(eigen))] * sizingFactor , edge.arrow.size = 0.2, vertex.label =  curV, ...)
}

pdf("network20.pdf")
plottopkgraph(crannetwork, 20, cranevc, 20, layout = layout.kamada.kawai)
dev.off()


pdf("network20col.pdf")
plottopkgraph(crannetwork, 20, cranevc, 20, coloring = c("KernSmooth", "Matrix","MASS", "boot", "cluster", "codetools", "foreign", "lattice", "mgcv", "nlme", "rpart", "survival", "class", "nnet", "spatial"), layout = layout.kamada.kawai)
dev.off()


pdf("network100.pdf")
plottopkgraph(crannetwork, 100, cranevc, 20, layout = layout.kamada.kawai)
dev.off()

pdf("network500.pdf")
plottopkgraph(crannetwork, 500, cranevc, 20, layout = layout.kamada.kawai)
dev.off()


pdf("network500.pdf")
plottopkgraph(crannetwork, 500, cranevc, 20, layout = layout.kamada.kawai)
dev.off()


pdf("network500_labelk.pdf")
plottopkgraph(crannetwork, 500, cranevc, 5, labelk = 5, layout = layout.fruchterman.reingold)
dev.off()


head(cranevc[order(cranevc, decreasing = TRUE)], n = 20) -> top20
top20 <- data.frame(x = names(top20), y = top20, stringsAsFactors = FALSE)
rankplot <- ggplot(top20, aes(x=reorder(x, y), y=y)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) + xlab("Package") + ylab("Eigenvector Centrality")
ggsave(rankplot, filename = "rankplot.jpg")
### TM


require(tm)
cranCorpus <- Corpus(VectorSource(crandata$long_desc))
inspect(cranCorpus[1:2])

cranCorpus <- tm_map(cranCorpus, stripWhitespace)
cranCorpus <- tm_map(cranCorpus, tolower)
inspect(cranCorpus[1:2])

cranCorpus <- tm_map(cranCorpus, removeWords, stopwords("english"))
cranCorpus <- tm_map(cranCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)

inspect(cranCorpus[1:2])

cranCorpus <- tm_map(cranCorpus, stemDocument)

inspect(cranCorpus[1:2])

dtm <- DocumentTermMatrix(cranCorpus)

dtm

dtm_reduced <- removeSparseTerms(dtm, 0.995) # remove Sparse terms
findAssocs(dtm_reduced, "model", corlimit = 0.1)
findAssocs(dtm_reduced, "plot", corlimit = 0.1)
findAssocs(dtm_reduced, "ggplot2", corlimit = 0.1)
findAssocs(dtm_reduced, "rcpp", corlimit = 0.1)

inspect(dtm_reduced[1:100, 1500:1510])
ncol(dtm_reduced)

dtm.matrix <- as.matrix(dtm_reduced)
rm(dtm, cranCorpus)

crandata$package_name

names(prCran)

evcentPkg <- cranevc[match(crandata$package_name, names(cranevc))]

evcentPkg[is.na(evcentPkg)] <- 0

rm(dtm_reduced)
rm(crandata)
saveRDS(dtm.matrix, file="dtm.matrix.RDS")

#require(randomForest)
#rfRank <- randomForest(xpagerank ~ . , data = dtm.matrix, importance = TRUE, ntree = 20000)

#rfRank <- randomForest(y = pagerank, x = dtm.matrix, importance = TRUE, ntree = 500)
require(plyr)
corMat <- adply(dtm.matrix, 2, function(x) { cor(x, evcentPkg) }, .progress = "text")
corMat$Terms[order(corMat$V1, decreasing = TRUE)]

posTerm <- head(corMat[order(corMat$V1, decreasing = TRUE),], 50)
posrankplot <- ggplot(posTerm, aes(x=reorder(Terms, V1), y=V1)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) + xlab("Keywords") + ylab("Correlation")
ggsave(posrankplot, filename = "posrankplot.jpg")

pdf("ddist.pdf")
ddist <- degree.distribution(crannetwork)
ddist[ddist == 0] <- NA
ndegree <- 2:length(ddist)
plot(y=ddist[2:length(ddist)], x = ndegree, type = "p", xlab="Degree", ylab="Fraction of nodes", log = "xy")
dev.off()

### detection of community using edge betweenness community detection algo. Very slow and not very effective!

#ebcomm <- edge.betweenness.community(crannetwork, weights = edgeWeight)
