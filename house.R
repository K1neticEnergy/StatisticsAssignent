# Import data
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(arsenal)
library(pastecs)
library(ggplot2)
library(modelr)

kc_house_data <- read_csv("kc_house_data.csv")

new_DF <- kc_house_data %>%
  select(price, floors, condition, view, sqft_above, sqft_living, sqft_basement)

#data cleaning

apply(is.na(new_DF), 1, which)

new_DF <- new_DF %>%
  transmute (
    price = (price > 0) ? log(price) : price,
    floors,
    condition,
    view,
    sqft_above = (sqft_above > 0) ? log(sqft_above) : sqft_above,
    sqft_living = (sqft_living > 0) ? log(sqft_living) : sqft_living,
    sqft_basement = (sqft_basement > 0) ? log(sqft_basement) : sqft_basement,
  )


contigous <- new_DF %>%
  select(price, sqft_above, sqft_living, sqft_basement) %>%
  apply(2, function(x) { c(mean(x), median(x), sd(x), min(x), max(x)) }) %>%
  as.data.frame()
rownames(contigous) <- c("mean", "median", "sd", "min", "max")

discrete <- table(new_DF[c(3,4)])
table(new_DF$floors)
table(new_DF$condition)
table(new_DF$view)

ggplot(data = new_DF) + 
  geom_histogram(mapping = aes(x = price), binwidth = 1) + 
  scale_x_continuous(limits=c(11,16)) + 
  ggtitle("price distribution") +
  xlab("Price") + ylab("Frequency")



ggplot(data = new_DF, mapping = aes(x = as.factor(floors), y = price)) +
  geom_boxplot() + xlab("Floors") + ylab("Price") + ggtitle("Distribution of price")

ggplot(data = new_DF, mapping = aes(x = as.factor(condition), y = price)) +
  geom_boxplot() + xlab("Condition") + ylab("Price") + ggtitle("Distribution of price")

ggplot(data = new_DF, mapping = aes(x = as.factor(view), y = price)) +
  geom_boxplot() + xlab("View") + ylab("Price") + ggtitle("Distribution of price")

pairs(~price + sqft_above,new_DF)
pairs(~price + sqft_living,new_DF)
pairs(~price + sqft_basement,new_DF)

#hist(
#  new_DF$price,
#  xlab = "PRICE",
#  ylab = "FREQUENCY",
#  main = "DISTRIBUTION OF PRICE",
#  labels = T,
#)

#boxplot(
#  new_DF$price~new_DF$floors,
#  xlab = "Floor",
#  ylab = "Price",
#  main = "Distribution of price"
#)
#

################# linear regression #################

# Problem 4:
options(na.action = na.warn)
model1 <- lm(formula = price ~ as.factor(condition) + as.factor(floors) + as.factor(view)
              sqft_above + sqft_living + sqft_basement, data = new_DF)
summary(model1)

model2 <- lm(formula = price ~ as.factor(floors) + 
               sqft_above + sqft_living + sqft_basement, data = new_DF)
summary(model2)

model3 <- lm(formula = price ~ as.factor(view) + 
               sqft_above + sqft_living + sqft_basement, data = new_DF)
summary(model3)

anova(model1, model2, model3)

plot(model1, which = 1)

# Problem 5:
data_test <- data.frame(
  sqft_above    <- c(mean(new_DF$sqft_above),    max(new_DF$sqft_above)),
  sqft_living   <- c(mean(new_DF$sqft_living),   max(new_DF$sqft_living)),
  sqft_basement <- c(mean(new_DF$sqft_basement), max(new_DF$sqft_basement)),
  condition     <- c(3,3),
  floors        <- c(2,2),
  view          <- c(1,3))
rownames(data_test) <- c("test1", "test2")
  
pred <- data.frame(predict(model1,data_test,interval="confidence"))
pred$range <- pred$upr - pred$lwr
