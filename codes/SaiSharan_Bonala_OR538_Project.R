my_data <- read.csv("D:/Study Material/GMU Textbooks/Sem 3/OR538/Project/dataset/final_dataset.csv")
head(my_data)

#my_data$date <- NULL
str(my_data)

my_data
my_data$During <- as.character(my_data$During)
my_data$Strong <- as.character(my_data$Strong)
my_data$Death <- as.character(my_data$Death)
my_data$temp_rise <- as.character(my_data$temp_rise)
my_data$temp_dec <- as.character(my_data$temp_dec)
my_data$maxTemp <- NULL
my_data$minTemp <- NULL

str(my_data)

my_model <- lm(adj_closed_price ~ .-Return - Trade.Volume - date, data = my_data)

summary(my_model)

plot(my_data$adj_closed_price, my_data$avgTemp)

install.packages('corrplot')
library(corrplot)

correlation <- cor(my_data)
corrplot(correlation, method="circle")

my_ret <- lm(Return ~ .-adj_closed_price - Trade.Volume - date, data = my_data)
summary(my_ret)

my_trade <- lm(Trade.Volume ~ . - Return - adj_closed_price - date, data = my_data)
summary(my_trade)

install.packages("eventstudies")
library(eventstudies)
split_data <- read.csv("D:/Study Material/GMU Textbooks/Sem 3/OR538/Project/dataset/SplitDate.csv")
library(zoo)
split_data$name <- c("return")
split_data$when <- as.Date(split_data$when)
head(split_data)
str(split_data)

return <- cbind(my_data$Return)
colnames(return)<- c("return")
stock_return <- zoo(return,as.Date(my_data$date))
head(stock_return)
str(stock_return)

evnt_stdy <- eventstudy(
     firm.returns = stock_return,
     event.list = split_data,
     event.window = 10,
     type = "None",
     to.remap = TRUE,
     remap = "cumsum",
     inference = TRUE,
     inference.strategy = "bootstrap")
