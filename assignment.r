# Prerequisite
# Import data
install.packages("dplyr","tidyr","ggplot2","modelr")
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(modelr)


#Question 1
#import
gia_nha <- read.csv("gia_nha.csv")

#Question 2
#get needed variables
new_DF <- gia_nha[,c("price", "floors", "condition", "view", "sqft_above", "sqft_living", "sqft_basement")]
#index_NA to get the position of NA
index_NA <- apply(apply(new_DF, 2, is.na),2,which)
index_NA
#remove rows that have NA
new_DF <- na.omit(new_DF)

#Question 3
#change value (!= 0) to log form
new_DF[, c(1, 5, 6)] <- log(new_DF[, c(1, 5, 6)])

# new_DF$has_basement <- (new_DF$sqft_basement != 0)

new_DF$sqft_basement[which(new_DF$sqft_basement != 0)] <- log(new_DF$sqft_basement[which(new_DF$sqft_basement != 0)])

#Analyze price, sqft_above, sqft_living, sqft_basement
data1 <- as.data.frame(apply(new_DF[,c("price", "sqft_above", "sqft_living", "sqft_basement")], 2,
                             function(x){ c(mean(x), median(x), sd(x), min(x), max(x))}))
rownames(data1) <- c("MEAN", "MEDIAN", "SD", "MIN", "MAX")
data1

#Analyze floors, condition, view, has_basement
data2 <- apply(new_DF[,c("floors", "condition", "view")], 2, table)
data2

#Distribution of price
hist(new_DF$price, xlab = "PRICE", ylab = "NUMBER OF HOUSES", main = "DISTRIBUTION OF PRICE",ylim = c(0,8000), labels = TRUE, col ="pink")

#Distribution of price by floors
price_by_floors <- boxplot( new_DF$price ~ new_DF$floors, ylab = "PRICE", xlab = "FLOORS" ,
                            main = "DISTRIBUTION OF PRICE BY FLOORS", col=rgb(238/255,66/255,102/255))
data_price_by_floors <- price_by_floors$stats
colnames(data_price_by_floors) <- c("1", "1.5", "2", "2.5", "3", "3.5")
rownames(data_price_by_floors) <- c("extreme of the lower whisker", "first quartile", "median", "third quartile", "extreme of the upper whisker")
data_price_by_floors

#Distribution of price by condition
price_by_condition <- boxplot(new_DF$price~new_DF$condition, ylab = "PRICE", xlab = "CONDITION",
                              main = "DISTRIBUTION OF PRICE BY CONDITION", col=rgb(238/255,66/255,102/255))
data_price_by_condition <- price_by_condition$stats
colnames(data_price_by_condition) <- c("1", "2", "3", "4", "5")
rownames(data_price_by_condition) <- c("extreme of the lower whisker", "first quartile", "median", "third quartile", "extreme of the upper whisker")
data_price_by_condition

#Distribution of price by view
price_by_view <- boxplot(new_DF$price~new_DF$view, ylab = "PRICE", xlab = "VIEW",
                         main = "DISTRIBUTION OF PRICE BY VIEW", col=rgb(238/255,66/255,102/255))
data_price_by_view <- price_by_view$stats
colnames(data_price_by_view) <- c("0", "1", "2", "3", "4")
rownames(data_price_by_view) <- c("extreme of the lower whisker", "first quartile", "median", "third quartile", "extreme of the upper whisker")
data_price_by_view

#Distribution of price of houses that dont have basement
# hist(new_DF$price[which(!new_DF$has_basement)], ylim = c(0,2500), xlim = c(11,16), freq = TRUE,
#                             ylab = "NUMBER OF HOUSES", xlab = "PRICE", main = "DISTRIBUTION OF PRICE OF NONE-BASEMENT HOUSES", col = "#316C9F")
# 
# price_none_basement <- list(new_DF$price[which(!new_DF$has_basement)])
# data3 <- as.data.frame(sapply(price_none_basement,
#                              function(x){ c(mean(x), median(x), sd(x), min(x), max(x))}))
# rownames(data3) <- c("MEAN", "MEDIAN", "SD", "MIN", "MAX")
# colnames(data3) <- c("Price")
# data3

#Distribution of price by sqft_above
pairs(new_DF$price ~ new_DF$sqft_above, labels = c("price", "sqft_above"), col= "#2d3166", main = "DISTRIBUTION OF PRICE BY SQFT_ABOVE")

#Distribution of price by sqft_living
pairs(new_DF$price ~ new_DF$sqft_living, labels = c("price", "sqft_living"), col= "#2d3166", main = "DISTRIBUTION OF PRICE BY SQFT_LIVING")

#Distribution of price of houses that have basements by sqft_basement
# pairs(new_DF$price ~ new_DF$sqft_basement[which(new_DF$has_basement)],
#       labels = c("price", "sqft_basement"), col= "#2d3166", main = "DISTRIBUTION OF PRICE BY SQFT_BASEMENT")
pairs(new_DF$price ~ new_DF$sqft_basement,
      labels = c("price", "sqft_basement"), col= "#2d3166", main = "DISTRIBUTION OF PRICE BY SQFT_BASEMENT")

# Question 4 ------------------------------------------------------------------------------
# Model 1
# price: bi???n ph??? thu???c
# condition: bi???n ph??n lo???i
# floors: bi???n ph??n lo???i
# view: bi???n ph??n lo???i
# sqft_above: bi???n ?????c l???p
# sqft_living: bi???n ?????c l???p
# sqft_basement: bi??n ?????c l???p
# -----------------------------------------------------------------------------------------
model1 <- lm(formula = price ~ as.factor(condition) + as.factor(floors) + as.factor(view) +
                sqft_above + sqft_living + sqft_basement, data = new_DF)
# summarize model
summary(model1)
#- H0: ???H??? s??? h???i quy kh??ng c?? ?? ngh??a th???ng k?????.
#- H1: ???H??? s??? h???i quy c?? ?? ngh??a th???ng k?????.
# condition > 0.05 ph?? h???p H0 => lo???i condition
# 
# Model 2: lo???i b??? condition
model2 <- lm(formula = price ~ as.factor(floors) + as.factor(view) +
                    sqft_above + sqft_living + sqft_basement, data = new_DF)
# summarize model
summary(model2)
#- H0: ???H??? s??? h???i quy kh??ng c?? ?? ngh??a th???ng k?????.
#- H1: ???H??? s??? h???i quy c?? ?? ngh??a th???ng k?????.
# T???t c??? bi???n ?????u c?? m???c ?? ngh??a tr??n 5%
anova(model1,model2)
# H0: ???Hai m?? h??nh M1 v?? M2 gi???ng nhau???.
# H1: ???Hai m?? h??nh M1 v?? M2 kh??c nhau???
# v???i m???c ?? ngh??a 5% => M1,M2 kh??c nhau

# V??? ????? th??? sai s??? h???i quy v?? sai s??? d??? b??o cho m?? h??nh Model 1.
plot(model1, which = 1)
# c??c ??i???m t???p trung quanh ???????ng h???i quy (ngo???i tr??? m???t s??? gi?? tr??? l?? ngo???i sai), 
# ch???ng t??? ph????ng sai c???a c??c sai s??? l?? h???ng s??? v?? cho th???y d??? ??o??n theo m?? h??nh n??y l?? h???p l??

# Question 5:
data_test <- data.frame(
  sqft_above    <- c(mean(new_DF$sqft_above),    max(new_DF$sqft_above)),
  sqft_living   <- c(mean(new_DF$sqft_living),   max(new_DF$sqft_living)),
  sqft_basement <- c(mean(new_DF$sqft_basement), max(new_DF$sqft_basement)),
  condition     <- c(median(new_DF$condition),median(new_DF$condition)),
  floors        <- c(median(new_DF$floors),median(new_DF$floors)),
  view          <- c(median(new_DF$view),median(new_DF$view)))
rownames(data_test) <- c("test1", "test2")

pred <- data.frame(predict(model1,data_test,interval="confidence"))
pred$range <- pred$upr - pred$lwr
pred[2, 4] / pred[1, 4]

