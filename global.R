library(lubridate)
library(officer)
library(tidyr)
library(dplyr)
library(zoo)
library(plotly)
library(leaflet)
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(rmapshaper)
library(mapview)
library(webshot)
library(htmltools)
library(quantmod)
library(gapminder)


dat <- gapminder
ds <- read.csv('ds.csv')


x <- c('Product<br>Revenue', 'Services<br>Revenue', 'Total<br>Revenue', 'Fixed<br>Costs', 'Variable<br>Costs', 'Total<br>Costs', 'Total')
y <- c(400, 660, 660, 590, 400, 400, 340)
base <- c(0, 430, 0, 570, 370, 370, 0)
revenue <- c(430, 260, 690, 0, 0, 0, 0)
costs <- c(0, 0, 0, 120, 200, 320, 0)
profit <- c(0, 0, 0, 0, 0, 0, 370)
text <- c('$430K', '$260K', '$690K', '$-120K', '$-200K', '$-320K', '$370K')
data <- data.frame(x, base, revenue, costs, profit, text)

#The default order will be alphabetized unless specified as below:
data$x <- factor(data$x, levels = data[["x"]])




y <- c('The course was effectively<br>organized',
       'The course developed my<br>abilities and skills for<br>the subject',
       'The course developed my<br>ability to think critically about<br>the subject',
       'I would recommend this<br>course to a friend')
x1 <- c(21, 24, 27, 29)
x2 <-c(30, 31, 26, 24)
x3 <- c(21, 19, 23, 15)
x4 <- c(16, 15, 11, 18)
x5 <- c(12, 11, 13, 14)

data_2 <- data.frame(y, x1, x2, x3, x4, x5)

top_labels <- c('Strongly<br>agree', 'Agree', 'Neutral', 'Disagree', 'Strongly<br>disagree')

