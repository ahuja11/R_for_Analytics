---
title: "Final Project"
author: "Rated R"
date: "October 2, 2016"
output: html_document
---
rm(list=ls())
if (!("plyr" %in% names(installed.packages()[,"Package"]))) {install.packages("plyr")}
suppressMessages(library(plyr, quietly = TRUE))
if (!("sqldf" %in% names(installed.packages()[,"Package"]))) {install.packages("sqldf")}
suppressMessages(library(sqldf, quietly = TRUE))
if (!("lubridate" %in% names(installed.packages()[,"Package"]))) {install.packages("lubridate")}
suppressMessages(library(lubridate, quietly = TRUE))
if (!("caret" %in% names(installed.packages()[,"Package"]))) {install.packages("caret")}
suppressMessages(library(caret, quietly = TRUE))
if (!("GGally" %in% names(installed.packages()[,"Package"]))) {install.packages("GGally")}
suppressMessages(library(GGally, quietly = TRUE))
if (!("ggplot2" %in% names(installed.packages()[,"Package"]))) {install.packages("ggplot2")}
suppressMessages(library(ggplot2, quietly = TRUE))
if (!("IDPmisc" %in% names(installed.packages()[,"Package"]))) {install.packages("IDPmisc")}
suppressMessages(library(IDPmisc, quietly = TRUE))
colors<-colors() # save vector of colors for custom plots
if (!("faraway" %in% names(installed.packages()[,"Package"]))) {install.packages("faraway")}
suppressMessages(library(faraway, quietly = TRUE))
if (!("lmtest" %in% names(installed.packages()[,"Package"]))) {install.packages("lmtest")}
suppressMessages(library(lmtest, quietly = TRUE))
if (!("lawstat" %in% names(installed.packages()[,"Package"]))) {install.packages("lawstat")}
suppressMessages(library(lawstat, quietly = TRUE))
if (!("nortest" %in% names(installed.packages()[,"Package"]))) {install.packages("nortest")}
suppressMessages(library(nortest, quietly = TRUE))
if (!("MASS" %in% names(installed.packages()[,"Package"]))) {install.packages("MASS")}
suppressMessages(library(MASS, quietly = TRUE))

## Loading the Dataset using urls
house_data = read.csv("https://www.dropbox.com/s/m3pvj69x125e5tn/kc_house_data.csv?dl=1")
zipcity_mapping <- read.csv("https://www.dropbox.com/s/gxy3tk2tfnm1o86/Zip_mapping.csv?dl=1")
 
## Checks to see if the data has loaded correctly
str(house_data)
head(house_data)
dim(house_data)
 
## Checking for NAs
sapply(house_data, function(x) sum(is.na(x)))
 
## Checking for duplicates
sqldf("Select * from house_data limit 10")
sqldf("Select count(*) as All_IDs, count(distinct id) as Distinct_IDs from house_data")
 
## Extracting date from the date column
house_data$year=substr(house_data$date,1,4)
house_data$month=substr(house_data$date,5,6)
house_data$day=substr(house_data$date,7,8)
house_data$date = ISOdate(house_data$year,house_data$month,house_data$day)

## Removing duplicates
house_data = arrange(house_data,id,desc(date))
house_data_clean = house_data[!duplicated(house_data$id), ]
dim(house_data_clean)
 
#bedrooms
count(house_data_clean,'bedrooms')
house_data_clean$bedrooms[house_data_clean$bedrooms > 7] <- 8
## Removing rows with bedrooms = 0 since we are not considering these
house_data_clean = house_data_clean[house_data_clean$bedrooms != 0,]
boxplot(house_data_clean$bedrooms)
count(house_data_clean,'bedrooms')
 
#bathrooms
count(house_data_clean$bathrooms)

 
#waterfront
count(house_data_clean$waterfront)
house_data_clean$waterfront=factor(house_data_clean$waterfront)

#delete view
house_data_clean=subset(house_data_clean,select=-view)
 
#grade
count(house_data_clean$grade)

#yr_built/age
house_data_clean$age=2016-house_data_clean$yr_built
# house_data_clean$age[house_data_clean$yr_built==1991]
 
#renovated or not?
unique(house_data_clean$yr_renovated)
house_data_clean$yr_renov_flag = cut(house_data_clean$yr_renovated, breaks=c(-1,1999,2016), labels=c(0,1))
#house_data_clean$yr_renov_flag[house_data_clean$yr_renovated==2000]
unique(house_data_clean$yr_renov_flag)
house_data_clean$yr_renov_flag=factor(house_data_clean$yr_renov_flag)
count(house_data_clean$yr_renov_flag)

house_data_temp=house_data_clean
#house_data_clean=house_data_temp 
str(house_data_clean)

# BUCKETING ZIPCODES INTO CITIES
str(zipcity_mapping)
zipcity_mapping=subset(zipcity_mapping,select=-X)
count(zipcity_mapping,'zipcode')
house_data_final <- merge(house_data_clean, zipcity_mapping,by="zipcode")
head(house_data_final)
sapply(house_data_final, function(x) sum(is.na(x)))
count(house_data_final,'city')
 
## Exploratory Data Analysis
 
colnames(house_data_final)

## Getting corr between price and continuous features
## Also checking for collinearity among features
house_data_corr = subset(house_data_final, select = c(price, bedrooms, bathrooms, sqft_living, sqft_lot, floors, condition, grade, sqft_above,sqft_basement, sqft_living15, sqft_lot15,age))
str(house_data_corr)
cor(house_data_corr)

corr_mat <-  cor(house_data_corr)
if (!("corrplot" %in% names(installed.packages()[,"Package"]))) {install.packages("corrplot")}
suppressMessages(library(corrplot, quietly = TRUE))

corrplot(corr_mat, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

 
### Scatter Plot of Price for Categorical Variables
# Waterfront ( We can use his for other variables as well)
sps <- ggplot(house_data_clean, aes(x<-sqft_living, y<-price, colour = factor(waterfront))) +
    geom_point(aes(colour = factor(waterfront))) +
    guides(fill = FALSE) +
    scale_colour_brewer(palette = "Set1")
sps + geom_smooth(method=lm, se=FALSE,fullrange=TRUE)
 
qplot(sqft_living, price,data=house_data_clean)
qplot(sqft_living, price,data=house_data_clean, geom = c("point", "smooth"))
 
# Checking the distribution of price
qplot(price,data=house_data_clean, geom = "histogram")
qplot(log(price),data=house_data_clean, geom = "histogram")

# Plotting relevant boxplots
#waterfront
plot1=ggplot(data = house_data_final) +
geom_boxplot(aes(x = as.factor(waterfront), y = price, fill = factor(waterfront))) +
labs(title = "Price vs Waterfront",x="Waterfront",y="Price") + guides(fill = FALSE)
#yr_renov_flag
plot2=ggplot(data = house_data_final) +
geom_boxplot(aes(x = as.factor(yr_renov_flag), y = price, fill = factor(yr_renov_flag))) +
labs(title = "Price vs Renovated Flag",x="Renovated Flag",y="Price") + guides(fill = FALSE) 
#grade
plot3=ggplot(data = house_data_final) +
geom_boxplot(aes(x = as.factor(grade), y = price, fill = factor(grade))) +
labs(title = "Price vs Grade",x="Grade",y="Price") + guides(fill = FALSE)
#condition
plot4=ggplot(data = house_data_final) +
geom_boxplot(aes(x = as.factor(condition), y = price, fill = factor(condition))) +
labs(title = "Price vs Condition",x="Condition",y="Price") + guides(fill = FALSE)

## Plotting boxplot of cities
plot5=ggplot(data = house_data_final) +
geom_boxplot(aes(x = as.factor(city), y = price, fill = factor(city))) +
labs(title = "Price vs Cities",x="City",y="Price") + guides(fill = FALSE)

grid.arrange(arrangeGrob(plot1,plot2,plot3,ncol=3,widths=c(1.25/5,1.25/5,2.5/5)),plot5,heights=c(2/5,3/5),ncol=1)
 
names(house_data_final)
hist(data <- log(house_data_final$price), freq = F)
 
## HERE START THE REGRESSIONS
 
# Multi colinear variables were removed
 
house_data_model = subset(house_data_final, select = c(price, bedrooms, sqft_living, sqft_lot, floors, waterfront, condition, sqft_basement, sqft_lot15, age, city, yr_renov_flag))

# CONVERTING PRICE TO LOG 
house_data_model$price_log <- log(house_data_model$price)
head(house_data_model$price_log)
names(house_data_model)

# Dropping Price from the data
house_data_model = subset(house_data_model, select = -c(price))
names(house_data_model)

# PARTITIONING DATASET INTO TEST AND TRAINING 
set.seed(3456)
dim(house_data_model)
trainIndex <- createDataPartition(house_data_model$price_log, p = 0.75, list = FALSE,times = 1)
house_train <- house_data_model[ trainIndex,]
house_test  <- house_data_model[-trainIndex,]
dim(house_test)
dim(house_train)
 
# Validating the distribution
require(gridExtra)
a = qplot(price_log,data=house_train, geom = "histogram", xlab= "Log of Price", ylab="Frequency", main=" Distribution for Training Data")
 
b = qplot(price_log,data=house_test, geom = "histogram", xlab= "Log of Price", ylab="Frequency", main=" Distribution for Test Data")
 
grid.arrange(a, b, ncol=2)

# DEFINING A DIAGNOSTICS FUNCTION

Diagnostic_plots <- function(dataToPlot) {
  
par(mfcol=c(2,3), fg=colors[24], bg=colors[2],col.lab="black")

# cooks distance - check for influential points
cook<-cooks.distance(dataToPlot)
halfnorm(cook,3,ylab="Cooks distance", main="Influences",col="skyblue3" ,cex.axis=1.3, cex.lab=1.3, cex.main=1.5)

boxplot(cook, col="skyblue3", ylab="Cooks distance", main="Boxplot Cooks Distances",cex.axis=1.3, cex.lab=1.3, cex.main=1.5)
 
# constant variance
plot(fitted(dataToPlot),residuals(dataToPlot),xlab="Fitted",ylab="Residuals", col="skyblue3", pch=19,type='p', main="Residual vs Fitted",cex.axis=1.3, cex.lab=1.3, cex.main=1.5)
 
abline(h=0)
 
plot(fitted(dataToPlot),abs(residuals(dataToPlot)),xlab="Fitted",ylab="Abs(Residuals)", main="Abs(Resid) vs. Fitted", col="skyblue3", pch=19,cex.axis=1.3, cex.lab=1.3, cex.main=1.5)
 
# normality
qqnorm(residuals(dataToPlot),ylab="Residuals", pch=19, col="skyblue4",cex.axis=1.3, cex.lab=1.3, cex.main=1.5)
 
qqline(residuals(dataToPlot))
 
hist(residuals(dataToPlot), col="skyblue3",xlab="Residuals", main="Histogram of Residuals",cex.axis=1.3, cex.lab=1.3, cex.main=1.5)
}
 
# REGRESSION - FIRST ITERATION
x = lm(price_log~.,data=house_train)
summary(x)
rm(x) 
 
# SECOND ITERATION - DROPPING sqft_lot15
house_train_v1 <- subset(house_train, select = -c(sqft_lot15))
names(house_train_v1)
x1 <- lm(price_log ~.,data=house_train_v1 )
summary(x1)
str(x1)

#diagnostic plots
Diagnostic_plots(x1)


# THIRD ITERATION - REMOVING INFLUENTIAL POINT #6421
x1$model[6421,]
subset(house_train_v1,house_train_v1$sqft_lot==307752)
house_train_v2 <- subset(house_train_v1,house_train_v1$sqft_lot!=307752)
dim(house_train_v2)

x2 <- lm(price_log ~.,data=house_train_v2 )
summary(x2)
anova(x2)
 
#diagnostic plots
Diagnostic_plots(x2)


# FOURTH ITERATION - Removing influential points 1872 and 5145
x2$model[c(1872,5145),]
house_train_v3 <- subset(house_train_v2,house_train_v2$sqft_lot!=871200 & house_train_v2$sqft_living!=5545)
house_train_v3 <- subset(house_train_v3,house_train_v3$sqft_lot!=31374 & house_train_v3$sqft_living!=9890)
dim(house_train_v3)

x3 <- lm(price_log ~.,data=house_train_v3 )
summary(x3)
anova(x3)

write.csv(house_train_v3, file = "House_Data.csv") 

#diagnostic plots
Diagnostic_plots(x3)

## Running the model on the test dataset
names(house_test_v1)

house_test_v1 <- subset(house_test, select = -c(sqft_lot15))
pred <- predict(x3, house_test_v1)
head(pred)

house_test_v1$pred <- pred 
head(house_test_v1)

SS_total <- sum((house_test_v1$price_log - mean(house_test_v1$price_log))^2)
SS_total
SS_residual   <- sum((house_test_v1$price_log - house_test_v1$pred)^2)
SS_residual
SS_regression <- sum((house_test_v1$pred - mean(house_test_v1$price_log))^2)
SS_regression
SS_total - (SS_regression+SS_residual)


# fraction of variability EXPLAINED by the model
test_rsq = SS_regression/SS_total
test_rsq

## Prediction and confidence intervals
class(anova(x3))
# Sample prediction
predicted <- predict(x3, newdata = data.frame(bedrooms=3,sqft_living=1000,
                                                               floors=2,
                                                               sqft_lot=5000,
                                                               waterfront=factor(1),
                                                               condition=3,
                                                               sqft_basement=400,
                                                               age=2016-2010,
                                                               city="Auburn",
                                                               yr_renov_flag=factor(1)))
predicted

## 95% Confidence Interval
predict(x3, newdata = data.frame(bedrooms=3, sqft_living=1000, floors=2, sqft_lot=5000, waterfront=factor(1), condition=3, sqft_basement=400, age=2016-2010, city="Auburn", yr_renov_flag=factor(1)), interval="confidence") 

## 95% Prediction Interval
predict(x3, newdata = data.frame(bedrooms=3, sqft_living=1000, floors=2, sqft_lot=5000, waterfront=factor(1), condition=3, sqft_basement=400, age=2016-2010, city="Auburn", yr_renov_flag=factor(1)), interval="prediction")  

library(shiny)
ui <- fluidPage(
        titlePanel(h1("Predicting House Prices in King County")),
        fluidRow(
          column(3, wellPanel(  
            h2("User Inputs"),
            selectInput("city",h5("Which city are you looking for a house in?"), c("Auburn"="Auburn","Bellevue"="Bellevue","Black Diamond"="Black Diamond","Bothell"="Bothell","Carnation"="Carnation","Des Moines"="Des Moines","Duvall"="Duvall","Enumclaw"="Enumclaw","Fall City"="Fall City","Federal Way"="Federal Way","Issaquah"="Issaquah","Kenmore"="Kenmore","Kent"="Kent","Kirkland"="Kirkland","Maple Valley"="Maple Valley","Medina"="Medina","Mercer Island"="Mercer Island","North Bend"="North Bend","Redmond"="Redmond","Renton"="Renton","Sammamish"="Sammamish","Seatac"="Seatac","Seattle"="Seattle","Snoqualmie"="Snoqualmie","Tukwila"="Tukwila","Vashon"="Vashon","Woodinville"="Woodinville")),
            sliderInput("sqft_living",h5("Size of the house:"),value=1910,min=350,max=10000,step=20),
            sliderInput("sqft_lot",h5("Size of the lot:"),value=7500,min=670,max=1164794,step=20),
            sliderInput("sqft_basement",h5("Size of the basement:"),value=300,min=0,max=4850,step=20),
            sliderInput("year_built",h5("When was the house built:"),value=2000,min=1900,max=2015,step=1)
          )),
        column(3, wellPanel(
          selectInput("floors",h5("Number of floors:"),c("1"=1,"1.5"=1.5, "2"=2,"2.5"=2.5,"3"=3,"3.5"=3.5)),
          selectInput("bedrooms",h5("Number of bedrooms:"),c("1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8)),
          selectInput("condition",h5("Condition of the house:"),c("Poor"=1,"Fair"=2,"Average"=3,"Good"=4,"Very Good"=5)),
          
          # selectInput("grade",h5(":"),c("1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8)),
          radioButtons("yr_renov_flag",h5("Is it a new house?"),choices=list("yes" = 1,"no" = 0)),
          radioButtons("waterfront",h5("Do you want to be by the waterfront?"),choices=list("yes" = 1,"no" = 0)),
          actionButton("go", "Check it out!"),
          hr(),
          helpText("Data from Kaggle | Released Under CC0: Public Domain License")
        )),
        column(5,
          h2("Prediction"),
          p("This model will be predict the price of a house given several criteria:"),
          #h3("Result:"),
          h3("The predicted house price is:"),
          verbatimTextOutput("oid1"),
          h3("The model being used is:"),
          verbatimTextOutput("summary")
        )
))
#install.packages("UsingR")
#library(UsingR)

server <- function(input, output) { 
         output$oid1 <- renderText({
           if (input$go > 0) {
             predicted <- predict(x3, newdata = data.frame(bedrooms=as.numeric(input$bedrooms),
                                                           sqft_living=input$sqft_living,
                                                           sqft_lot=input$sqft_lot,
                                                           floors=as.numeric(input$floors),
                                                           waterfront=factor(input$waterfront),
                                                           condition=as.numeric(input$condition),
                                                           sqft_basement=input$sqft_basement,
                                                           age=2016-input$year_built,
                                                           city=input$city,
                                                           yr_renov_flag=factor(input$yr_renov_flag)),
                                  interval="predict")
             a = dollar(exp(predicted))
             paste("The predicted value of the house is",a[1],".","The prediction interval for the price is",a[2],"to",a[3])
             }
           })
         output$summary <- renderPrint({
           anova(x3)
           })
}  

shinyApp(ui = ui, server = server)


