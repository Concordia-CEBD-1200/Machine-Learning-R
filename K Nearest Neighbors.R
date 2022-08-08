#install.packages("ISLR")
#library(ISLR)

#head(Caravan)
#str(Caravan)
#summary(Caravan)
#summary(Caravan$Purchase)
#any(is.na(Caravan))

#var(Caravan[,1])
#var(Caravan[,2])

# Get the data
purchase <- Caravan[, 86]

# Scaling Data
standardized.Caravan <- scale(Caravan[, -86])
#print(var(standardized.Caravan[, 1]))
#print(var(standardized.Caravan[, 2]))

# Test split
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

# Train
train.data <- standardized.Caravan[-test.index, ]
train.purchase <- purchase[-test.index]

####################
######KNN Model#####
####################

library(class)
set.seed(101)

predicated.purchase <-
  knn(train.data, test.data, train.purchase, k = 5)

head(predicated.purchase)

misclass.error <- mean(test.purchase != predicated.purchase)
print(misclass.error)

#Choosing a K Value
predicated.purchase <- NULL
error.rate <- NULL

for (i in 1:20) {
  set.seed(101)
  predicated.purchase <- knn(train.data,
                             test.data,
                             train.purchase,
                             k = i)
  error.rate[i] <-
    misclass.error <- mean(test.purchase != predicated.purchase)
}

print(error.rate)

### Visualize K Elbow
library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)
error.df

ggplot(error.df, aes(k.values, error.rate)) + 
  geom_point() + 
  geom_line(lty = "dotted", color = "red")

# Therefore K = 9 is the best for us










