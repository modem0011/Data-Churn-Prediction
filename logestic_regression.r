
Churn_data <- read.csv(file.choose())

str(Churn_data)

Churn_data$Churn <- factor(Churn_data$Churn)
str(Churn_data)

library(caTools)
set.seed(390)
split <- sample.split(Churn_data, SplitRatio = 0.7)
split

train <- subset(Churn_data, split== "TRUE")
test <- subset(Churn_data, split== "FALSE")
str(train)
str(test)

logit_model <- glm(Churn ~ ., data = train, family = "binomial")
logit_model

summary(logit_model)


logit_model <- glm(Churn ~ ContractRenewal + CustServCalls + RoamMins , data = train, family = binomial)
summary(logit_model)

fitted.results <- predict(logit_model, test, type = "response")

fitted.results # Predicted Result
test$Churn   # Actual Result

plot(logit_model)
fitted.results.new <- fitted.results

#fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results.new <- ifelse(fitted.results.new > 0.3,1,0)
#fitted.results <- ifelse(fitted.results > 0.25,1,0)
plot(fitted.results.new)
fitted.results.new # Predicted Result
test$Churn    # Actual Result

plot(fitted.results.new)

table(test$Churn, fitted.results.new)
Error <- mean(fitted.results.new != test$Churn  )
print(paste('Accuracy =',1-Error))


