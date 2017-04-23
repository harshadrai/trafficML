# Setting the Working Directory
setwd("C:/Users/kkrishn3/Desktop/Sowers/predictive_model")
# Load the required package (uncomment line below if running for the first time on a machine)
#install.packages("DMwR")
library(DMwR)

# Load the dataset
df <- read.csv("data_cube.csv")
table(df$fatality)
#    0      1 
# 477992   483 
prop.table(table(df$fatality))
#     0           1 
#0.998990543 0.001009457 

# Run the SMOTE algorithm
df <- as.data.frame(apply(df,2,as.factor))
train <- SMOTE(fatality ~ ., df, perc.over = 1000, perc.under=200)
df$fatality <- as.numeric(df$fatality)

train$fatality <- ifelse(train$fatality==2,1,0)
prop.table(table(train$fatality))
#    0         1 
#0.6451613 0.3548387 
write.csv(train, "balanced_dataset.csv")
