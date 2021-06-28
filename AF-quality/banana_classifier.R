library(readr)
library(tidyverse)
library(caret)
df <- read_table2("D:/df.txt")

df <- df %>% slice(-88, -89) %>% select(-D1, -D2, -lengte_inch_decimaal, -Lengte, -Breedte) %>% 
  filter(Gewicht<201)
y<- df$Gewicht
df$Land <- as.factor(df$Land)

df %>% ggplot(aes(volume, Gewicht,  col=Land)) + 
  geom_point() +
  geom_smooth(method = "lm")


#set splitsen
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- df[test_index, ] 
train_set <- df[-test_index, ]  

fit1 <- train(Gewicht ~ Land, method = "lm", data = train_set, 
              metric = "RMSE",
              preProc = c("center", "scale", "nzv"), #good idea to do this with neural nets - your error is due to non scaled data
              trControl = trainControl(
                method = "cv",
                number = 5,
                verboseIter = TRUE)
)
              
summary(fit1)
y_hat <- predict(fit1, test_set)

data=data.frame(gemeten=test_set$Gewicht, voorspeld=y_hat)
data %>% ggplot(aes(gemeten, voorspeld, col=test_set$Land)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)
mean(test_set$Gewicht[test_set$Land=='PE'])

model <- lm(Gewicht ~ Land , data = test_set)
y_hat <- predict(model, test_set)
mean(test_set$Gewicht[test_set$Land=='PE'])


model2 <- train(Land ~ lengte_cm, method = 'glm', 
                data=train_set,
                preProc = c("center", "scale"), #good idea to do this with neural nets - your error is due to non scaled data
                trControl = trainControl(
                  method = "repeatedcv",
                  number = 10,
                  repeats = 10,
                  verboseIter = TRUE)
)

y_hat <- predict(model2, test_set)
confusionMatrix(y_hat, test_set$Land)

t <- test_set %>% mutate(difPE = abs(test_set$lengte_cm - mean(train_set$lengte_cm[train_set$Land=='PE'])),
                    difDR = abs(test_set$lengte_cm - mean(train_set$lengte_cm[train_set$Land=='DR'])))


y_hat <- as.factor(ifelse(t$difPE < t$difDR,'PE','DR'))
length(y_hat)
confusionMatrix(y_hat, test_set$Land)
