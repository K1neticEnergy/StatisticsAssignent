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

new_DF$sqft_basement[new_DF$sqft_basement == 0] = 1

new_DF[,c("price", "sqft_above","sqft_living", "sqft_basement")] <- new_DF %>%
  select(price, sqft_above, sqft_living, sqft_basement) %>%
  log()



contigous <- new_DF %>%
  select(price, sqft_above, sqft_living, sqft_basement) %>%
  apply(2, function(x) { c(mean(x), median(x), sd(x), min(x), max(x)) }) %>%
  as.data.frame()
rownames(contigous) <- c("mean", "median", "sd", "min", "max")

#discrete <- table(select(new_DF, floors, condition, view))
table(new_DF$floors)
table(new_DF$condition)
table(new_DF$view)

ggplot(data = new_DF) + 
  geom_histogram(mapping = aes(x = price), binwidth = 0.5) + 
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

#split dataset into 2 based on sqft basement > 0
a = new_DF %>% group_split(sqft_basement > 0, keep = FALSE) 

res = data.frame(cor(a[[2]]))[1]
# Building histogram
ggplot(data=new_DF, aes(new_DF$price)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

# loading psych package
# install.packages("psych")
library(psych)
psych::describe(new_DF)

################# linear regression #################
# Problem 4:
# Mô hình M1:
# price: biến phụ thuộc
# condition: biến phân loại
# floors: biến phân loại
# view: biến phân loại
# sqft_above: biến độc lập
# sqft_living: biến độc lập
# sqft_basement: bién độc lập
lmModel <- lm(formula = price ~ as.factor(condition) + as.factor(floors) + as.factor(view) +
              sqft_above + sqft_living + sqft_basement, data = new_DF)
summary(lmModel)
######################################################################
# Call:
#   lm(formula = price ~ as.factor(condition) + as.factor(floors) + 
#        as.factor(view) + sqft_above + sqft_living + sqft_basement, 
#      data = new_DF)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.19787 -0.27155  0.01418  0.24573  1.41819 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    ==
# (Intercept)            7.209240   0.084839  84.976  < 2e-16 ***
#   as.factor(condition)2 -0.050420   0.072130  -0.699 0.484553    
# as.factor(condition)3  0.104329   0.066727   1.564 0.117948    
# as.factor(condition)4  0.146065   0.066793   2.187 0.028765 *  
#   as.factor(condition)5  0.257144   0.067220   3.825 0.000131 ***
#   as.factor(floors)1.5   0.118483   0.009328  12.701  < 2e-16 ***
#   as.factor(floors)2     0.053939   0.007275   7.415 1.26e-13 ***
#   as.factor(floors)2.5   0.259204   0.029578   8.763  < 2e-16 ***
#   as.factor(floors)3     0.276455   0.015617  17.702  < 2e-16 ***
#   as.factor(floors)3.5   0.378978   0.128976   2.938 0.003303 ** 
#   as.factor(view)1       0.263154   0.020315  12.954  < 2e-16 ***
#   as.factor(view)2       0.210482   0.012220  17.225  < 2e-16 ***
#   as.factor(view)3       0.294073   0.016621  17.693  < 2e-16 ***
#   as.factor(view)4       0.604313   0.020882  28.940  < 2e-16 ***
#   sqft_above             0.559772   0.029453  19.006  < 2e-16 ***
#   sqft_living            0.185085   0.029156   6.348 2.22e-10 ***
#   sqft_basement          0.041727   0.001973  21.153  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3644 on 21596 degrees of freedom
# Multiple R-squared:  0.5218,	Adjusted R-squared:  0.5214 
# F-statistic:  1473 on 16 and 21596 DF,  p-value: < 2.2e-16
#############################################################################

#- H0: “Hệ số hồi quy không có ý nghĩa thống kê”.
#- H1: “Hệ số hồi quy có ý nghĩa thống kê”.
# Xét mức ý nghĩa 5% ta thấy rằng: condition > 0.05 phù hợp H0 => loại condition

#------------------------------------------------------------------------------
# Mô hình M2:
# price: biến phụ thuộc
# floors: biến phân loại
# view: biến phân loại
# sqft_above: biến độc lập
# sqft_living: biến độc lập
# sqft_basement: bién độc lập

com_lmModel <- lm(formula = price ~ as.factor(floors) + as.factor(view) +
               sqft_above + sqft_living + sqft_basement, data = new_DF)
summary(com_lmModel)
###########################################################################
# Call:
#   lm(formula = price ~ as.factor(floors) + as.factor(view) + sqft_above + 
#        sqft_living + sqft_basement, data = new_DF)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.13866 -0.27372  0.01365  0.24658  1.40610 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          7.314148   0.055398 132.030  < 2e-16 ***
#   as.factor(floors)1.5 0.132438   0.009353  14.160  < 2e-16 ***
#   as.factor(floors)2   0.038299   0.007132   5.370 7.95e-08 ***
#   as.factor(floors)2.5 0.262306   0.029771   8.811  < 2e-16 ***
#   as.factor(floors)3   0.254993   0.015542  16.406  < 2e-16 ***
#   as.factor(floors)3.5 0.373183   0.129854   2.874  0.00406 ** 
#   as.factor(view)1     0.266263   0.020453  13.018  < 2e-16 ***
#   as.factor(view)2     0.214823   0.012302  17.463  < 2e-16 ***
#   as.factor(view)3     0.296031   0.016734  17.691  < 2e-16 ***
#   as.factor(view)4     0.613593   0.021010  29.205  < 2e-16 ***
#   sqft_above           0.511067   0.029521  17.312  < 2e-16 ***
#   sqft_living          0.236886   0.029206   8.111 5.29e-16 ***
#   sqft_basement        0.039743   0.001983  20.044  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3669 on 21600 degrees of freedom
# Multiple R-squared:  0.515,	Adjusted R-squared:  0.5147 
# F-statistic:  1911 on 12 and 21600 DF,  p-value: < 2.2e-16
##############################################################################

#-----------------------------------------------------------------------------
anova(lmModel, com_lmModel)

# Analysis of Variance Table
# 
# Model 1: price ~ as.factor(condition) + as.factor(floors) + as.factor(view) + 
#   sqft_above + sqft_living + sqft_basement
# Model 2: price ~ as.factor(floors) + as.factor(view) + sqft_above + sqft_living + 
#   sqft_basement
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  21596 2867.0                                  
# 2  21600 2907.6 -4    -40.59 76.437 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# H0: “Hai mô hình M1 và M2 giống nhau”.
# H1: “Hai mô hình M1 và M2 khác nhau”

# với mức ý nghĩa 5%  <=> M1,M2 khác nhau

plot(lmModel, which = 1)
plot(lmModel, which = 2)
plot(lmModel, which = 3)
plot(lmModel, which = 4)


# Problem 5:
data_test <- data.frame(
  sqft_above    <- c(mean(new_DF$sqft_above),    max(new_DF$sqft_above)),
  sqft_living   <- c(mean(new_DF$sqft_living),   max(new_DF$sqft_living)),
  sqft_basement <- c(mean(new_DF$sqft_basement), max(new_DF$sqft_basement)),
  condition     <- c(median(new_DF$condition),median(new_DF$condition)),
  floors        <- c(median(new_DF$floors),median(new_DF$floors)),
  view          <- c(median(new_DF$view),median(new_DF$view)))
rownames(data_test) <- c("test1", "test2")

pred <- data.frame(predict(lmModel,data_test,interval="confidence"))
pred$range <- pred$upr - pred$lwr







# other way
library(caret)
index <- createDataPartition(new_DF$price, p = .80, list = FALSE)
train <- new_DF[index, ]
test <- new_DF[-index, ]
# # Checking the dim of train
dim(train)
dim(test)
# Taining model
lmModel <- lm(price ~ as.factor(condition) + as.factor(floors) + as.factor(view) +
                sqft_above + sqft_living + sqft_basement, data = train)
summary(lmModel)
# Call:
#   lm(formula = price ~ as.factor(condition) + as.factor(floors) + 
#        as.factor(view) + sqft_above + sqft_living + sqft_basement, 
#      data = train)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.17324 -0.27241  0.01331  0.24617  1.41888 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            7.289452   0.100463  72.559  < 2e-16 ***
#   as.factor(condition)2 -0.120264   0.087195  -1.379   0.1678    
# as.factor(condition)3  0.028407   0.081695   0.348   0.7281    
# as.factor(condition)4  0.064781   0.081762   0.792   0.4282    
# as.factor(condition)5  0.188164   0.082201   2.289   0.0221 *  
#   as.factor(floors)1.5   0.124012   0.010424  11.896  < 2e-16 ***
#   as.factor(floors)2     0.058345   0.008141   7.167 7.98e-13 ***
#   as.factor(floors)2.5   0.263105   0.034050   7.727 1.16e-14 ***
#   as.factor(floors)3     0.283445   0.017916  15.820  < 2e-16 ***
#   as.factor(floors)3.5   0.405226   0.148874   2.722   0.0065 ** 
#   as.factor(view)1       0.268308   0.022818  11.758  < 2e-16 ***
#   as.factor(view)2       0.208100   0.013745  15.140  < 2e-16 ***
#   as.factor(view)3       0.298075   0.018718  15.925  < 2e-16 ***
#   as.factor(view)4       0.609094   0.023362  26.072  < 2e-16 ***
#   sqft_above             0.563825   0.032878  17.149  < 2e-16 ***
#   sqft_living            0.179961   0.032542   5.530 3.25e-08 ***
#   sqft_basement          0.042362   0.002200  19.251  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3643 on 17275 degrees of freedom
# Multiple R-squared:  0.5221,	Adjusted R-squared:  0.5216 
# F-statistic:  1179 on 16 and 17275 DF,  p-value: < 2.2e-16
############################################################################

com_lmModel <- lm(price ~ as.factor(floors) + as.factor(view) +
                               sqft_above + sqft_living + sqft_basement, data = train)

summary(com_lmModel)
# Call:
#   lm(formula = price ~ as.factor(floors) + as.factor(view) + sqft_above + 
#        sqft_living + sqft_basement, data = train)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.13710 -0.27456  0.01246  0.24757  1.40785 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          7.320947   0.062080 117.927  < 2e-16 ***
#   as.factor(floors)1.5 0.137726   0.010460  13.167  < 2e-16 ***
#   as.factor(floors)2   0.043516   0.007981   5.453 5.03e-08 ***
#   as.factor(floors)2.5 0.262691   0.034271   7.665 1.88e-14 ***
#   as.factor(floors)3   0.263189   0.017844  14.750  < 2e-16 ***
#   as.factor(floors)3.5 0.408817   0.149917   2.727   0.0064 ** 
#   as.factor(view)1     0.272444   0.022976  11.858  < 2e-16 ***
#   as.factor(view)2     0.212670   0.013840  15.367  < 2e-16 ***
#   as.factor(view)3     0.300248   0.018847  15.931  < 2e-16 ***
#   as.factor(view)4     0.621438   0.023500  26.445  < 2e-16 ***
#   sqft_above           0.512455   0.032950  15.552  < 2e-16 ***
#   sqft_living          0.233928   0.032592   7.177 7.39e-13 ***
#   sqft_basement        0.040288   0.002212  18.215  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3669 on 17279 degrees of freedom
# Multiple R-squared:  0.5151,	Adjusted R-squared:  0.5147 
# F-statistic:  1529 on 12 and 17279 DF,  p-value: < 2.2e-16

# Predicting Price in test dataset
anova(lmModel, com_lmModel)

test$PreditedPrice <- predict(lmModel, test)
prediction <- data.frame(predict(lmModel, test, interval="confidence"))
prediction$range <- prediction$upr - prediction$lwr
prediction$actual <- test$price
# Priting top 6 rows of actual and predited price
head(test[ , c("price", "PreditedPrice")])

actual <- test$price
preds <- test$PreditedPrice
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

testing_pred <- data.frame(predict(lmModel,data_test,interval="confidence"))
testing_pred$range <- pred$upr - pred$lwr


# 
# library(reshape2)
# meltData <- melt(new_DF)
# p <- ggplot(meltData, aes(factor(variable), value))
# p + geom_boxplot() + facet_wrap(~variable, scale="free")
# 
# #split dataset into 2 based on sqft basement > 0
# #a = new_DF %>% group_split(sqft_basement > 0)
# meltData <- melt(new_DF)
# p <- ggplot(meltData, aes(factor(variable), value))
# p + geom_boxplot() + facet_wrap(~variable, scale="free")
# 
# require(corrgram)
# corrgram(new_DF, order=TRUE)

# # Checking model statistics
# summary(lmModel)
# # Using AIC function
# AIC(lmModel)
# # Using BIC function
# BIC(lmModel)  
# # Checking model object for actual and predicted values
# names(lmModel)
# library(Metrics)
# rmse(actual = train$price, predicted = lmModel$fitted.values)
# # Histogram to check the distribution of errors
# hist(lmModel$residuals, color = "grey")
# #The above histogram of errors clearly states that errors are normally distributed.
# 
# plot(lmModel)
# library("lmtest")
# dwtest(lmModel)
# 