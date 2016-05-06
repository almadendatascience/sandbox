library(DMwR)   #scale/unscale
library(ggplot2)#graphics
library(caret)  #test/traim
library(DT)     #datatable

#All data pre-processing is done in excel.  This includes:
#combining all data files into a single file
#removing quantitative columns with many missing values
#removing qualitative columns with missing values
#re-arranging excel columns to bring together numeric and non numeric columns
files = c("AllData_Cleaned.csv")
data.cleaned = read.csv(paste("data/",files[1],sep=""), stringsAsFactors = FALSE)

#Extract subset of numeric columns
data.numeric = data.cleaned[,40:67]

#Scaling
#Data in ALL columns is normalized using DMwR~scale function 
data.numeric.scaled = data.frame(scale(data.numeric, center = TRUE, scale = TRUE))

#Use the same seed to replicate results.  Change the see to generate a different 80/20 combination
set.seed(20)

# define an 80/20 percent train/test split of the dataset
#partition data set into 80% | 20% train/test split
#note: original data set = 858 rows; 80% train = 686 row 
split=0.80
trainIndex <- createDataPartition(data.numeric.scaled$Curr.List.Price, p=split, list=FALSE)

#create train/test using/removing randomly selected indexes
data_train <- data.numeric.scaled[ trainIndex,]
data_test  <- data.numeric.scaled[-trainIndex,]

plot(1:nrow(data_train), data_train$Curr.List.Price)

# train the linear regression model
model <- lm(Curr.List.Price~., data=data_train)

# examine results

summary(model)
plot(model)

# make predictions
x_test <- data_test[,1:27]
y_test <- data_test[,28]
predictions <- predict(model, x_test)

#Unscale to arrive at the original test data and predictions for comparison
data_test_unscaled = lapply(data_test, function(x) {x * sd(data.numeric$Curr.List.Price) + mean(data.numeric$Curr.List.Price)})
predictions.unscaled = lapply(data.frame(predictions), function(x) {x * sd(data.numeric$Curr.List.Price) + mean(data.numeric$Curr.List.Price)})

combined_prediction = cbind(data_test_unscaled, data.frame(predictions.unscaled))
datatable(combined_prediction[,28:29], rownames = FALSE, list(pagelength=1, dom = "t"))