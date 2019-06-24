#Attendence tracker
library(dplyr)
library(lubridate)
library(ggplot2)
setwd('C:/Users/achyuthuni.harsha/Desktop/personal')
data <- read.csv("Attendance_Detail.csv")
data <- data %>% 
  filter(Was.at.Work == 'True')
# 11-Jun-2018 was when timings changed
data$Attendance.Date <- dmy(data$Attendance.Date)
data.req <- data %>% filter(Attendance.Date > dmy("11-Jun-2018"))
plot(data.req$In.Time)
ggplot(data.req, aes(factor(data.req$In.Time))) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

a <- readRDS('model_KNN_2018-09-20-20-11-09.RDS')
for (i in 1:5){
  glm.fit=glm(mpg???poly(horsepower ,i),data=Auto)
  cv.error[i]=cv.glm (Auto ,glm .fit)$delta [1]
}