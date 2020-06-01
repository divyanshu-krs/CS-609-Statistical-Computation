#### Load the Library #######3

library(bnlearn)

#### Setting the Working Director###########
# Make sure you replace your directory here, This directory should contain your csv file. #
setwd("~/32/stats/project/data")

### Read the data ####

filename <- 'processed_cleveland.csv'
dataset <- read.csv(filename)

#### Check how data is looking ###
head(dataset)

### Check the column names ###
names(dataset)

## Since the column names are not present, we will add the them from data description
names(dataset)[1] <- "age"
names(dataset)[2] <- "sex"
names(dataset)[3] <- "cp"
names(dataset)[4] <- "trestbps"
names(dataset)[5] <- "chol"
names(dataset)[6] <- "fbs"
names(dataset)[7] <- "restecg"
names(dataset)[8] <- "thalach"
names(dataset)[9] <- "exang"
names(dataset)[10] <- "oldpeak"
names(dataset)[11] <- "slope"
names(dataset)[12] <- "ca"
names(dataset)[13] <- "thal"
names(dataset)[14] <- "target"

### Find random variable from all variable that we will be using

#sample(1:13, 7, replace=FALSE) # this line of find random number

# my output of the above command was = [1]  9  8  3  6  7 10  4, therefore we will be only taking these column 
v1 <- colnames(dataset[9])
v2 <- colnames(dataset[8])
v3 <- colnames(dataset[3])
v4 <- colnames(dataset[6])
v5 <- colnames(dataset[7])
v6 <- colnames(dataset[10])
v7 <- colnames(dataset[4])
# Now we will get data only from these columns
final_data <- dataset[,c(v1,v2,v3,v4,v5,v6,v7,"target")] # this is the data that we will be using

### Plot some visualization ###
hist(final_data[,c("age")])
hist(dataset[,c("sex")])

# From our whole data The continous variable were : Age(1), trestbps(4), chol(5), thalah(8), oldpeak(10), thal(12)
# So you check if any of v's belong to them if yes, we will discretize them
# In our approach, v2 and v6 are continuous so we will discretize them
v2_new <- discretize(data.frame(dataset[,c(v2)]), breaks = 5)
v6_new <- discretize(data.frame(dataset[,c(v6)]), breaks = 3)

# now Replace this in ur dataset
final_data[,c(v2)]<- v2_new[,1]
final_data[,c(v6)]<- v6_new[,1]


#### Convert the target variable into present or not present where 1 represent present and zero represent absent
final_data$target[final_data$target >= 1]  <- 1
final_data$target[final_data$target <= 0]  <- 0

#### Train test split #####
train_data = dataset[1:290, ]
test_data = dataset[291:302, ]

### Making the Network
bayes_net = hc(train_data)
print(plot(bayes_net))

### Fit the model ##
fit_model = bn.fit(bayes_net,train_data)
pred = predict(fit_model,"target",test_data)

## Find the prediction vs actual class
cbind(pred,test_data[,"target"])

## Find the accuracy of algorithm 
# This is optional you can uncomment and run this
#library(forecast)
#accuracy(f=pred, x=test_data[,"target"])


