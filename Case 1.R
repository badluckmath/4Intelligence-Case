library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(purrr)
library(cluster)
library(forecast)
library(fpp2)
#Limpando todas as variáveis do sistema
rm(list= ls())


df <- read.csv('TFP.csv')
df$isocode <- factor(df$isocode)

#Time series for each country

df %>% group_by(isocode) %>% ggplot(aes(year,rtfpna,color= isocode)) + geom_line()

#Make a separete df por each isocode
mex <- df %>% filter(isocode=='MEX')# %>% select(-isocode)
can <- df %>% filter(isocode=='CAN') #%>% select(-isocode)
usa <- df %>% filter(isocode=='USA') #%>% select(-isocode)

#Now, let set the dataframe as a time series
x <- ts(mex[,3],start= 1950)
y <- ts(can[,3],start= 1950)
z <- ts(usa[,3],start= 1950)

#Now, lets make a forecast  with  an 95% interval of confidence
f.x <- forecast(x, level = c(95), h = 10)
f.y <- forecast(y, level = c(95), h = 10)
f.z <- forecast(z, level = c(95), h = 10)

#Plots for each isocode
mex.plot <- autoplot(f.x,alpha=0.5,xlab = "Year", ylab ="rtfpna",main="Mexico TFP Projection", colour = T)
can.plot <- autoplot(f.y,alpha=0.5,xlab = "Year", ylab ="rtfpna",main="Canada TFP Projection", colour = T)
usa.plot <- autoplot(f.z,alpha=0.5,xlab = "Year", ylab ="rtfpna",main="USA TFP Projection", colour = T)

#Plotando os gráficos lado a lado para comparação
par(mfrow= c(1,3))
plot(f.x, main = 'Mexico  10 Years Forecast')
plot(f.y, main = 'Canada  10 Years Forecast')
plot(f.z, main = 'USA  10 Years Forecast')
