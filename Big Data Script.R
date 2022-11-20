#Install packages
install.packages('tidyverse')
install.packages('microbenchmark')
install.packages("doParallel")
install.packages("magrittr")

#Load packages
library(parallel)
library(doParallel)
library(microbenchmark)
library(readr)
library(dplyr)
library(ggplot2)

#Setting the Working directory
setwd("C:/Users/Admin/Documents/GroceriesDataset/")

#2.0 Sequential Processing ====================================================================

#Read multiple csv with data.table in Sequential Method
sequential <- function() {
  
  list_CSV_files <- list.files(path="C:/Users/Admin/Documents/GroceriesDataset/")
  foreach(i=1:2)  %do% {
    dfpar <- readr::read_csv(list_CSV_files, id="file_name")
  }
}

#Test Sequential process with micro benchmark for time taken
library(microbenchmark)
mbmS <- microbenchmark(sequential())

mbmS
#Plot graph Sequential process
library(autoplot)
autoplot(mbmS)

#Sequential Processing ====================================================================


#Parallel Processing ====================================================================

#Read multiple csv with data.table in Parallel Method
parallel <- function() {
  list_CSV_files <- list.files(path="C:/Users/Admin/Documents/GroceriesDataset/")
  foreach(i=1:2)  %dopar% {
    dfpar <- readr::read_csv(list_CSV_files, id="file_name")
  }
}

#Set number of cores and create cluster to be used for Parallel
no_of_cores <- detectCores() - 1
cl <- makeCluster(no_of_cores, type="PSOCK")
registerDoParallel(cl)


#Test Parallel process with micro benchmark for time taken
library(microbenchmark)
mbmP <- microbenchmark(parallel())
stopCluster(cl)

mbmP
#Plot graph Parallel process
autoplot(mbmP)

#2.0 Parallel Processing ====================================================================


#3.0 Data Analysis !=====================================================

#Reading the csv(s)
nutenergy_df <- read.csv("C:/Users/Admin/Documents/HypothesisDataset.csv")
View(nutenergy_df)

#Data set structure----------------------------------------------------------
str(nutenergy_df)

#Descriptive Analysis----------------------------------------------------------

#Descriptive Analysis of the data set
summary(nutenergy_df)

#Visuals for Median
library(ggpubr)
gghistogram(nutenergy_df$energy_carb, title = "Histogram of energy_carb", xlab = "energy_carb",
            X = "energy_carb", bins=5, add="median", add.params = list(linetype="dotted"))

gghistogram(nutenergy_df$energy_protein, title = "Histogram of energy_protein", xlab = "energy_protein",
            X = "energy_protein", bins=5, add="median", add.params = list(linetype="dotted"))

gghistogram(nutenergy_df$energy_fibre, title = "Histogram of energy_fibre", xlab = "energy_fibre",
            X = "energy_fibre", bins=5, add="median", add.params = list(linetype="dotted"))

gghistogram(nutenergy_df$energy_fat, title = "Histogram of energy_fat", xlab = "energy_fat",
            X = "energy_fat", bins=5, add="median", add.params = list(linetype="dotted"))

gghistogram(nutenergy_df$h_nutrients_calories, title = "Histogram of h_nutrients_calories", xlab = "h_nutrients_calories",
            X = "h_nutrients_calories", bins=5, add="median", add.params = list(linetype="dotted"))

#Calculate Standard Deviation for each attributes
stdCarb <- sd(nutenergy_df$energy_carb)
print(stdCarb)

stdProtein <- sd(nutenergy_df$energy_protein)
print(stdProtein)

stdFibre <- sd(nutenergy_df$energy_fibre)
print(stdFibre)

stdFat <- sd(nutenergy_df$energy_fat)
print(stdFat)

stdEnergy <- sd(nutenergy_df$h_nutrients_calories)
print(stdEnergy)


#Visualization for Mean & Standard Deviation
standard_df <- data.frame(meanValues = c(mean(nutenergy_df$energy_carb),
                                        mean(nutenergy_df$energy_protein),
                                        mean(nutenergy_df$energy_fibre),
                                        mean(nutenergy_df$energy_fat),
                                        mean(nutenergy_df$h_nutrients_calories)),
                                        
                          stdValue = c(sd(nutenergy_df$energy_carb),
                                       sd(nutenergy_df$energy_protein),
                                       sd(nutenergy_df$energy_fibre),
                                       sd(nutenergy_df$energy_fat),
                                       sd(nutenergy_df$h_nutrients_calories)),
                                       
                          Category = c("energy_carb", "energy_protein", "energy_fibre", "energy_fat", "h_nutrients_calories"))
                          
#Load ggplot2
library(ggplot2)

ggplot(standard_df, aes(x=Category, y=meanValues)) + 
  geom_bar(position = position_dodge(), stat="identity", colour = 'black') +
  geom_errorbar(aes(ymin=meanValues-stdValue, ymax=meanValues+stdValue), width=.2)


#Correlation =====================================================
library(dplyr)
library(ggplot2)
library(ggpubr)

mosthighlycorrelated <- function(mydataframe, numtoreport){
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("First.variable", "Second.Variable", "Correlation")
  head(fm[order(abs(fm$Correlation), decreasing=T),], n=numtoreport)
}

energy_model <- select(nutenergy_df, 'h_nutrients_calories', 'energy_carb', 'energy_protein', 'energy_fibre', 'energy_fat')
mosthighlycorrelated(energy_model, 10)

#Correlation Visualization ---------------------------------------

library("ggpubr")
ggscatter(nutenergy_df, x = "energy_carb", y = "h_nutrients_calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Energy Carb", ylab = "Nutrients Calories")

ggscatter(nutenergy_df, x = "energy_protein", y = "h_nutrients_calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Energy Protein", ylab = "Nutrients Calories")

ggscatter(nutenergy_df, x = "energy_fibre", y = "h_nutrients_calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Energy Fibre", ylab = "Nutrients Calories")

ggscatter(nutenergy_df, x = "energy_fat", y = "h_nutrients_calories", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Energy Fat", ylab = "Nutrients Calories")

# Correlation ====================================================


#Hypothesis Finding p-value ======================================
model_carb <- lm(h_nutrients_calories ~ energy_carb, data = nutenergy_df)
model_protein <- lm(h_nutrients_calories ~ energy_protein, data = nutenergy_df)
model_fibre <- lm(h_nutrients_calories ~ energy_fibre, data = nutenergy_df)
model_fat <- lm(h_nutrients_calories ~ energy_fat, data = nutenergy_df)

print(summary(model_carb))
print(summary(model_protein))
print(summary(model_fibre))
print(summary(model_fat))

#Hypothesis ======================================================


#Regression ======================================================
# Create the predictor and response variable.
x <- c(nutenergy_df$energy_carb)
x <- c(nutenergy_df$energy_protein)
x <- c(nutenergy_df$energy_fibre)
x <- c(nutenergy_df$energy_fat)

y <- c(nutenergy_df$h_nutrients_calories)
relation <- lm(y~x)

# Plot charts for each attributes
#Energy Carb & H Nutrient Calories
plot(y,x,col = "blue",main = "Energy Carb and H_Nutrients Calories Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "energy_carb",ylab = "h_nutrients_calories")

#Energy Protein & H Nutrient Calories
plot(y,x,col = "blue",main = "Energy Protein and H_Nutrients Calories Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "energy_protein",ylab = "h_nutrients_calories")

#Energy Fibre & H Nutrient Calories
plot(y,x,col = "blue",main = "Energy Fibre and H_Nutrients Calories Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "energy_fibre",ylab = "h_nutrients_calories")

#Energy Fat & H Nutrient Calories
plot(y,x,col = "blue",main = "Energy Fat and H_Nutrients Calories Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "energy_fat",ylab = "h_nutrients_calories")

# Save plot as png (Regression) ------------------------------------
# Give the chart file a name.
png(file = "Enery Carb and H_Nutrients Calories Regression.png")

# Save the file.
dev.off()

#Regression ======================================================

#3.0 Data Analysis =====================================================!

