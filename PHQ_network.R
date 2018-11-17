library(mgm)
library(qgraph)

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


# mgm network
mgmfit <- mgm(data = PHQ_FULL[,-1],
              type = type,
              level = level,
              k = 2,
              lambdaSel = "EBIC",
              lambdaGam = 0.25)

Q_mgm <- qgraph(mgmfit$pairwise$wadj, layout = "circle", labels = colnames(PHQ_FULL[,-1]), minimum = 0, maximum = 1, cut = 0.2)

# glasso network

PHQcor <- cor_auto(PHQ_FULL[,-1])

Q_glasso <- qgraph(PHQcor, graph = "glasso", sampleSize = nrow(PHQ_FULL), minimum = 0, maximum = 1, cut = 0.2, layout = "circle")

# partial correlation network
Q_pcor <- qgraph(PHQcor, graph = "pcor", sampleSize = nrow(PHQ_FULL), minimum = 0, maximum = 1, cut = 0.2, layout = "spring")

getWmat(Q_glasso)
getWmat(Q_pcor)


centralityPlot(Q_pcor)

cor(PHQ_FULL[,-1])


Q_pcor <- qgraph(cor(PHQ_FULL[,-1]), sampleSize = nrow(PHQ_FULL), minimum = 0, maximum = 1, cut = 0.2, layout = "spring")

cor(PHQ_FULL[,-1])[c(2,6,9), c(2,6,9)]
