## set working directory
setwd("~/Documents/PhD/Projects/Adolescent CNA/Analysis")

## packages
library(plyr)


## load data
ncs <- read.csv("ncs_psaq_full.csv", sep = ",")
head(ncs)

# select columns with items 24a - 24f and 26a - 26ii
ncsdata <- ncs[,c(grep(colnames(ncs), pattern = "D24")[1:6], grep(colnames(ncs), pattern = "D26")[1:35])]

# change coding for D26c
ncsdata$D26c[ncsdata$D26c == "PREGNANT OR GROWING"] <- "NO"

# check
apply(ncsdata_num, 2, table)

# change letters to numbers
ncsdata_num <- ncsdata

# change factor to numeric values
for(i in 1:ncol(ncsdata))
{
  ncsdata_num[,i] = revalue(ncsdata[,i], c("R" = 99, "D" = 88, "NO" = 0, "YES" = 1))
  ncsdata_num[,i] = as.numeric(levels(ncsdata_num[,i]))[ncsdata_num[,i]]
}

# set missing values to 0
#ncsdata_num[is.na(ncsdata_num)] = 0

# converting to PHQ
PHQ1 <- rowSums(cbind(ncsdata_num$D24e, ncsdata_num$D24f), na.rm = TRUE)
PHQ2 <- rowSums(cbind(ncsdata_num$D24a, ncsdata_num$D24b, ncsdata_num$D24c, ncsdata_num$D24d), na.rm = TRUE)
PHQ3 <- ifelse((ncsdata_num$D26g == 1 | ncsdata_num$D26h == 1 | ncsdata_num$D26i == 1), 1, 0)
PHQ4 <- ncsdata_num$D26j
PHQ5 <-  ifelse((ncsdata_num$D26a == 1 | ncsdata_num$D26b == 1), 1, 0) 
PHQ6 <- rowSums(cbind(ncsdata_num$D26t, ncsdata_num$D26u, ncsdata_num$D26v, ncsdata_num$D26w), na.rm = TRUE)
PHQ7 <- rowSums(cbind(ncsdata_num$D26p, ncsdata_num$D26q, ncsdata_num$D26r, ncsdata_num$D26s), na.rm = TRUE)
PHQ8 <- rowSums(cbind(ncsdata_num$D26l, ncsdata_num$D26m, ncsdata_num$D26n, ncsdata_num$D26o), na.rm = TRUE)
PHQ9 <- rowSums(cbind(ncsdata_num$D26aa, ncsdata_num$D26bb, ncsdata_num$D26cc, ncsdata_num$D26dd, ncsdata_num$D26ee), na.rm = TRUE)

PHQ_data <- cbind("AGE" = ncs$Age, PHQ1, PHQ2, PHQ3, PHQ4, PHQ5, PHQ6, PHQ7, PHQ8, PHQ9)

write.table(PHQ_data, file = "PHQ_data.txt", row.names = FALSE, col.names = colnames(PHQ_data))
