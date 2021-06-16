library(readxl)
library(corrplot)
library(dplyr)

dataset <- read_excel("countryagregatedata.xlsx")

dim(dataset)
summary(dataset)

datasetVariables <- dataset[, 4:25]

corrplot(cor(datasetVariables), method="circle")

##############################################################################

total_deaths_new_cases <- dataset %>%
  select(total_deaths, new_cases)

index <- sample(1:nrow(total_deaths_new_cases), as.integer(0.7*nrow(total_deaths_new_cases)))
treino  <- total_deaths_new_cases[index, ]
teste  <- total_deaths_new_cases[-index, ]

slr.model <- lm(total_deaths ~ new_cases, data = total_deaths_new_cases)
slr.model

plot(total_deaths_new_cases$new_cases,total_deaths_new_cases$total_deaths, pch=20)
abline(slr.model$coefficients[1],slr.model$coefficients[2], col='red')
summary(slr.model)

slr.pred <- predict(slr.model,teste)
d<-teste$total_deaths-slr.pred

# MAE (Mean absolute error) 
mae <- mean(abs(d))
print("mae:")
mae

# RMSE (Root Mean Squared Error) 
rmse <- sqrt(mean(d^2))
print(rmse)

############################################################


index <- sample(1:nrow(datasetVariables), as.integer(0.7*nrow(datasetVariables)))
treino  <- datasetVariables[index, ]
teste  <- datasetVariables[-index, ]

mlr.modelo <- lm (life_expectancy ~ ., data = treino)
mlr.modelo

summary(mlr.modelo)


mlr.modelo <- lm (life_expectancy ~ population_density + median_age  + female_smokers +  human_development_index +Tot_dead_pop +  incidence, data = treino)
mlr.modelo

summary(mlr.modelo)

mlr.pred <- predict(mlr.modelo,teste)
mlr.pred

d<-teste$life_expectancy-mlr.pred  

# MAE (Mean absolute error) 
MAE <- mean(abs(d))
print("MAE:")
MAE

# RMSE (Root Mean Squared Error) 
RMSE <- sqrt(mean(d^2))
print("RMSE:")
RMSE

###########################################

library(rpart)
library(rpart.plot)

rpart.modelo <- rpart(life_expectancy ~ ., method="anova", data=treino)
rpart.modelo

rpart.plot(rpart.modelo, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

rpart.pred <- predict(rpart.modelo, teste)
d<-rpart.pred - teste$life_expectancy


# MAE (Mean absolute error) 
MAE <- mean(abs(d))
print("MAE:")
MAE

# RMSE (Root Mean Squared Error) 
RMSE <- sqrt(mean(d^2))
print("RMSE:")
RMSE


##################################################################


normalise = function(y){(y - min(y))/(max(y) - min(y))}
dataset_norm <- as.data.frame(lapply(datasetVariables, normalise)) 
summary(dataset_norm)

library(caTools) 
set.seed(123) 
split = sample.split(dataset_norm, SplitRatio = 0.7) 
train = subset(dataset_norm, split == TRUE) 
test = subset(dataset_norm, split == FALSE)

dim(concrete_train)
dim(concrete_test) 

library(neuralnet)

model <- neuralnet(life_expectancy ~ ., hidden =c(6, 3),data = train) 
plot(model)

model_results <- compute(model, test)

predicted <- model_results$net.result
head(predicted)

cor(predicted, test$life_expectancy)



minvec <- min(datasetVariables$life_expectancy)
maxvec <- max(datasetVariables$life_expectancy)

denormalize <- function(y,minval,maxval) {
  y*(maxval-minval) + minval
}

true_life_expectancy <- as.numeric(Map(denormalize,test$life_expectancy,minvec,maxvec))
true_life_expectancy

predicted_life_expectancy <- as.numeric(Map(denormalize,predicted,minvec,maxvec))
predicted_life_expectancy

d<-true_life_expectancy-predicted_life_expectancy  


# MAE (Mean absolute error) 
MAE <- mean(abs(d))
print("MAE:")
MAE

# RMSE (Root Mean Squared Error) 
RMSE <- sqrt(mean(d^2))
print("RMSE:")
RMSE

