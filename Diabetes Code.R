rm(list=ls()) # this cleans out old stuff
setwd("~/Desktop/dfsf")
source("myfunctions.R")
# import and read the diabetes csv
diabetes <- read.csv("~/Desktop/dfsf/diabetes_data_upload.csv", header=T)
library(tidyverse)

# Take a look at the table
head(diabetes)
names(diabetes)
sapply(diabetes, class)
str(diabetes)
glimpse(diabetes)
summary(diabetes)
dim(diabetes)
glimpse(diabetes)
# Check to see if any of the rows have missing values, so that we can fix them.
clean <- ifelse(complete.cases(diabetes) == TRUE,1,0)
paste("There are ", dim(diabetes)[1]-sum(clean), " rows with missing data.")

# change all of the strings to numerical binary responses

lookup <- c("Female" = 0, "Male" = 1)
diabetes$Gender <- lookup[diabetes$Gender]
lookup <- c("No" = 0, "Yes" = 1)
diabetes$Polyuria <- lookup[diabetes$Polyuria]
diabetes$Polydipsia <- lookup[diabetes$Polydipsia]
diabetes$Polyphagia <- lookup[diabetes$Polyphagia]
diabetes$sudden.weight.loss <- lookup[diabetes$sudden.weight.loss]
diabetes$weakness <- lookup[diabetes$weakness]
diabetes$Genital.thrush <- lookup[diabetes$Genital.thrush]
diabetes$visual.blurring <- lookup[diabetes$visual.blurring]
diabetes$Itching <- lookup[diabetes$Itching]
diabetes$Irritability <- lookup[diabetes$Irritability]
diabetes$delayed.healing <- lookup[diabetes$delayed.healing]
diabetes$partial.paresis <- lookup[diabetes$partial.paresis]
diabetes$muscle.stiffness <- lookup[diabetes$muscle.stiffness]
diabetes$Alopecia  <- lookup[diabetes$Alopecia]
diabetes$Obesity <- lookup[diabetes$Obesity]
lookup <- c("Negative" = 0, "Positive" = 1)
diabetes$class <- lookup[diabetes$class]

hist((diabetes$Age), main ="Histogram of Age of Diabetes Patients", col = NULL, xlab = "Age in Years")
counts <- table(diabetes$Gender)
barplot(counts, main="Gender Distribution",
        xlab="Male(1) or Female(0)")

counts <- table(diabetes$class)
barplot(counts, main="Class of Diabetics Distribution",
        xlab="Yes (1) or No (0)") 

# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(diabetes,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines


pca <- prcomp(diabetes, scale=TRUE)
summary(pca)

pca.variance <- pca$sdev^2
pca.variance.per <- round(pca.variance / sum(pca.variance)*100, 1)
barplot(pca.variance.per, main="Scree Plot of PCs vs Variance", xlab="Principal Components", ylab="Variance", ylim=c(0, 100))

plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", main="PC1 vs. PC2")

pca.data <- data.frame(Sample=rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) + geom_text() + xlab(paste("PC1 - ", pca.variance.per[1], "%", sep="")) + ylab(paste("PC2 - ", pca.variance.per[2], "%", sep="")) + theme_bw() + ggtitle("PC1 vs. PC2")

loading_scores <- pca$rotation[,1]
drug_scores <- abs(loading_scores)
drug_scores_ranked <- sort(drug_scores, decreasing=T)
names(drug_scores_ranked)

BIC(lm(diabetes$class~diabetes$Age))
BIC(lm(diabetes$class~diabetes$Gender))
BIC(lm(diabetes$class~diabetes$Polyuria))
BIC(lm(diabetes$class~diabetes$Polydipsia))
BIC(lm(diabetes$class~diabetes$sudden.weight.loss))
BIC(lm(diabetes$class~diabetes$weakness))
BIC(lm(diabetes$class~diabetes$Polyphagia))
BIC(lm(diabetes$class~diabetes$Genital.thrush))
BIC(lm(diabetes$class~diabetes$visual.blurring))
BIC(lm(diabetes$class~diabetes$Itching))
BIC(lm(diabetes$class~diabetes$Irritability ))
BIC(lm(diabetes$class~diabetes$delayed.healing))
BIC(lm(diabetes$class~diabetes$partial.paresis))
BIC(lm(diabetes$class~diabetes$muscle.stiffness))
BIC(lm(diabetes$class~diabetes$Alopecia))
BIC(lm(diabetes$class~diabetes$Obesity))
BIC(lm(diabetes$class~diabetes$Polyuria +
         diabetes$Polydipsia + diabetes$Gender +
         diabetes$Itching))
glimpse(diabetes)
