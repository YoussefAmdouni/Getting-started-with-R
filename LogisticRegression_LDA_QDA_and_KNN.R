### Libraries 

library(tidyverse)
library(corrplot)
library(corrgram)
library(ISLR)
library(caTools)
library(MASS)
library(class)
library(plotly)

### Data Import 

loans <- read.csv("data/loan_data.csv")
head(loans)

### Data Exploration

any(is.na(loans))
str(loans)

summary(loans)

factor_vars <- c('purpose', 'credit.policy', 'not.fully.paid')
loans[factor_vars] <- lapply(loans[factor_vars], function(x) as.factor(x))

p1<- ggplot(loans, aes(fico)) + 
      geom_histogram(aes(fill = not.fully.paid), color = "black", bins = 40, alpha = 0.5) + 
      scale_fill_manual(values = c('red','snow3')) + 
      labs(title = "Loan repayment by fico score", x = "Fico Score") +
      theme_bw() + 
      theme(plot.title = element_text(hjust=0.5, size = 14))
ggplotly(p1)


p2 <- ggplot(loans, aes(factor(purpose))) + 
        geom_bar(aes(fill=not.fully.paid),color = "black", position = 'dodge') +
        labs(title = "loan repayement by loan purpose", x = "Loan purpose") +
        scale_fill_manual(values = c('red','snow3')) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust=0.5, size = 14)) 
ggplotly(p2)

p3 <- ggplot(loans, aes(x = int.rate, y = fico)) +
        geom_point(aes(color = not.fully.paid), alpha = 0.4) +
        labs(title = "loan repayement by interest rate", x = "Interest rate") +
        theme_bw() +
        theme(plot.title = element_text(hjust=0.5, size = 14))
ggplotly(p3)

### Data Correlation 

data.cor <- loans %>% mutate(purpose = as.integer(purpose),
                             not.fully.paid = as.integer(not.fully.paid),
                             credit.policy = as.integer(credit.policy))
corrplot(cor(data.cor), method = 'color', title = 'correlation matrix')

corrgram(data.cor,order=TRUE, lower.panel=panel.pie,
         upper.panel=panel.shade, text.panel=panel.txt)

### Train/Test Split

set.seed(101)
sample <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train <- subset(loans, sample == T)
test <- subset(loans, sample == F)


### Logistic Regression

lgr.model <- glm(not.fully.paid ~ ., data = loans, family = binomial)
summary(lgr.model)
lgr.predictions <- predict(lgr.model, test[1:13], type = 'response')
table(ifelse(lgr.predictions>0.5,1,0), test$not.fully.paid)


### Linear Discriminant Analysis

lda.model <- lda(not.fully.paid ~ . , data = train)
lda.model
plot(lda.model)
lda.predictions <- predict(lda.model , test)$class
table(lda.predictions, test$not.fully.paid)
# change proba
# post <- predict(lda.model , test)$posterior
# table(ifelse(post[,1]>0.4,0,1), test$not.fully.paid)

### Quadratic Discriminant Analysis

qda.model <- qda(not.fully.paid ~ . , data = train)
qda.model
qda.predictions <- predict(qda.model , test)$class
table(qda.predictions, test$not.fully.paid)

### K-Nearest Neighbors 

train <- train %>% mutate(purpose = as.integer(purpose))
test <- test %>% mutate(purpose = as.integer(purpose))
knn.predictions <- knn(train[,-14], test[,-14], cl = train[, 14], k = 1)
table(knn.predictions, test$not.fully.paid)

# Choosing a k value
best.k <- function(iters){
  predictions <- NULL
  error.rate <- NULL 
  for (i in 1:20) {
    predicted <- knn(train[,-14], test[,-14], cl = train[, 14], k = i)
    error.rate[i] <- mean(test$not.fully.paid != predicted)
  }
  k.values <- 1:20
  error.df <- data.frame(error.rate, k.values)
  return(error.df)
}

error.df <- best.k(20)
ggplot(error.df, aes(1:20, error.rate)) + geom_point() +
  geom_line(color='blue') +
  scale_x_continuous(breaks=c(1:20)) + 
  labs(title = "Best k", x = "K values", y = "Error Rate") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face = "bold", size = 14))