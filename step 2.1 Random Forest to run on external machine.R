##############################################
#
#	LTS 
#
#    Random Forest classification
#   Code to run on external server
#	 
#   13 July 2021
# 
###############################################

install.packages("cowplot")
install.packages("randomForest")
install.packages("ggplot2")
library(ggplot2)
library(randomForest)
library(cowplot)

data_dir <- "D:/CZU/LTS mapping/Random Forest/data"
out_dir <- "D:/CZU/LTS mapping/Random Forest/output"

# Load data----
setwd(data_dir)
db <- load(file="Combined LTS, ref and simulated tree with 15 env and spectral variables.Rdata")
trees.3 <- get(db)

# Remove rows that have NAs 
trees.4 <- na.omit(trees.3)
length(which(is.na(trees.4)))
str(trees.4)

# Random Forest----
set.seed(317)

model <- randomForest(survival ~ ., data=trees.4, 
                      importance = TRUE, proximity=TRUE)

# save model output
setwd(out_dir)
save(m1, file= "Default random forest model output for LTS1.RData")
a <- load(file= "Default random forest model output for LTS1.RData")
mod_out <- get(a)
str(mod_out)
mod_out$confusion
mod_out$err.rate


### # Variable Importance----
# create importance plot and save it as pdf
setwd(out_dir)
pdf(file = "Variable importance plot for LTS 1 data, default model - top 10 variables.pdf",
    width = 7,
    height = 5)

varImpPlot(model,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance for default model")

dev.off()

importance(model)
varUsed(model)



# Errors plot----

# look at the error plot and save it as pdf
setwd(out_dir)
pdf(file = "Error rate plot for LTS 1 data as a function of tree number.pdf",
    width = 7,
    height = 5)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Surviving", "Dead"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"1"], 
          model$err.rate[,"0"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

dev.off()

# we can see that errors stabilize after ... trees


# Calculate min error rate----
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(survival ~ ., data=trees.4, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))


# Create best model----
## create a model for proximities using the best value for mtry
model.1 <- randomForest(survival ~ ., 
                   data=trees.4,
                   ntree=1000, 
                   proximity=TRUE, 
                   mtry=which(oob.values == min(oob.values)))
model.1

# save the best model output
setwd(out_dir)
save(model.1, file = "Best model, random forest output for LTS1.RData")

b <- load(file = "Best model, random forest output for LTS1.RData")
model_best <- get(b)
model_best$confusion


## create an MDS-plot to show how the samples are related to each 
## other.
##
## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-model.1$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)


## make a plot that shows the MDS axes and the variation and save it as pdf:

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=trees.4$survival)

setwd(out_dir)
pdf(file = "MDS axes and variation plot for random forest, best model, LTS1.pdf",
    width = 7,
    height = 5)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")

dev.off()
















