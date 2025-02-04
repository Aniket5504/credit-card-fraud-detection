# Download the file from https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud
install.packages(c("randomForest", "caret", "ROSE", "ggplot2"), dependencies = TRUE)


library(randomForest)
library(caret)
library(ROSE)
library(ggplot2)


df <- read.csv("C:/Users/ANIKET KUMAR/OneDrive/Desktop/creditcard.csv")


df$Class <- as.factor(df$Class)


table(df$Class)


df_balanced <- ROSE(Class ~ ., data = df, seed = 42)$data
table(df_balanced$Class) 


set.seed(42)
trainIndex <- createDataPartition(df_balanced$Class, p = 0.8, list = FALSE)
trainData <- df_balanced[trainIndex, ]
testData  <- df_balanced[-trainIndex, ]


rf_model <- randomForest(Class ~ ., data = trainData, ntree = 50)


rf_pred <- predict(rf_model, testData)


conf_matrix <- confusionMatrix(rf_pred, testData$Class)
print(conf_matrix)


ggplot(df, aes(x = Amount, fill = Class)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(title = "Transaction Amounts Distribution", x = "Transaction Amount", y = "Count")
