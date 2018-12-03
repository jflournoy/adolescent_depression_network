library(mgm)
library(qgraph)
library(bootnet)

PHQ <- read.table("PHQ_data.txt", header = TRUE)

# remove missing values
table(rowSums(is.na(PHQ)))
PHQ_NA <- PHQ[rowSums(is.na(PHQ)) == 0,]


# remove answer categories DK/R
apply(PHQ_NA, 2, table)
PHQ_FULL <- PHQ_NA[rowSums(PHQ_NA > 80) == 0,]

# response categories
level <- c(3, 5, 2, 2, 2, 5, 4, 3, 6)
type <- rep("c", 9)


## mgm cannot handle ordinal data!!
# mgm network
# mgmfit <- mgm(data = PHQ_FULL[,-1],
#               type = type,
#               level = level,
#               k = 2,
#               lambdaSel = "EBIC",
#               lambdaGam = 0.25)
# 
# Q_mgm <- qgraph(mgmfit$pairwise$wadj, layout = "circle", labels = colnames(PHQ_FULL[,-1]), minimum = 0, maximum = 1, cut = 0.2)

# glasso network

PHQcor <- cor_auto(PHQ_FULL[,-1])

Q_glasso <- qgraph(PHQcor, graph = "glasso", sampleSize = nrow(PHQ_FULL), minimum = 0, maximum = 1, cut = 0.2, layout = "circle")

# partial correlation network
Q_pcor <- qgraph(PHQcor, graph = "pcor", sampleSize = nrow(PHQ_FULL), minimum = 0, maximum = 1, cut = 0.2, layout = "spring")

getWmat(Q_glasso)
getWmat(Q_pcor)


# centrality analyses

CT_glasso = centralityTable(Q_glasso, standardized = TRUE)
CT_pcor = centralityTable(Q_pcor, standardized = TRUE)

centralityPlot(Q_glasso, include = c("Strength", "Closeness"))

pdf("centrality_phq_glasso.pdf", height = 7.5, width = 15)
plot(CT_glasso[CT_glasso$measure == "Strength",5], type = "l", las = 1, axes = FALSE, xlab = "Variables", ylab = "Value", ylim = c(-3, 3), cex = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2)
axis(1, at = 1:9, labels = row.names(PHQcor), cex.axis = 1.5)
axis(2, at = seq(-3, 3, length.out = 5), labels = seq(-3, 3, length.out = 5), las = 1, cex.axis = 1.5)
lines(CT_glasso[CT_glasso$measure == "Betweenness",5], lty = 2, lwd = 2)
lines(CT_glasso[CT_glasso$measure == "Closeness",5], lty = 3, lwd = 2)

legend("topleft", c("Strength", "Betweenness", "Closeness"), lty = c(1,2,3), bty = "n", cex = 1.5, lwd = 2)
dev.off()

pdf("centrality_phq_pcor.pdf", height = 7.5, width = 15)
plot(CT_pcor[CT_pcor$measure == "Strength",5], type = "l", las = 1, axes = FALSE, xlab = "Variables", ylab = "Value", ylim = c(-3, 3), cex = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2)
axis(1, at = 1:9, labels = row.names(PHQcor), cex.axis = 1.5)
axis(2, at = seq(-3, 3, length.out = 5), labels = seq(-3, 3, length.out = 5), las = 1, cex.axis = 1.5)
lines(CT_pcor[CT_pcor$measure == "Betweenness",5], lty = 2, lwd = 2)
lines(CT_pcor[CT_pcor$measure == "Closeness",5], lty = 3, lwd = 2)

legend("topleft", c("Strength", "Betweenness", "Closeness"), lty = c(1,2,3), bty = "n", cex = 1.5, lwd = 2)
dev.off()


# Stability analyses ------------------------------------------------------


boots <- 1000

CI_glasso_person <- bootnet(PHQ_FULL[,-1], 
               nBoots = boots, 
               default = "EBICglasso", 
               type = "person", 
               verbose = TRUE)

CI_pcor_person <- bootnet(PHQ_FULL[,-1], 
               nBoots = boots, 
               default = "pcor", 
               type = "person", 
               verbose = TRUE)


CorStTable <- data.frame("graph" = rep(c("glasso", "pcor"), each = 3), "Centrality measure" = rep(c("Betweenness", "Closeness", "Strength"), times = 2),  "CS" = c(corStability(CI_glasso_person, verbose = FALSE), corStability(CI_pcor_person, verbose = FALSE)))

plot(CI_glasso_person, subsetRange = c(100, 40), order = "sample")
plot(CI_pcor_person, subsetRange = c(100, 40), order = "sample")


# stability of edge weights

CI_glasso_edge <- bootnet(PHQ_FULL[,-1], 
                     nBoots = boots, 
                     default = "EBICglasso", 
                     verbose = TRUE)

CI_pcor_edge <- bootnet(PHQ_FULL[,-1], 
                   nBoots = boots, 
                   default = "pcor", 
                   verbose = TRUE)

plot(CI_glasso_edge, order = "sample")
plot(CI_pcor_edge, order = "sample")

plot(CI_glasso_edge, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(CI_pcor_edge, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

plot(CI_glasso_edge, "strength")
plot(CI_glasso_edge, "strength")
