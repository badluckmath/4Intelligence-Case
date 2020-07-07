library(ggplot2)
library(lattice)
library(dplyr)
library(readxl)
library(plotly)
library(purrr)
library(cluster)
library(forecast)
library(fpp2)

comex <- read.csv('data_comexstat.csv')
covar <- read_xlsx('covariates.xlsx')



#Primeiramente, vamos categorizar algumas variáveis para tornar a análise mais fácil
comex$state <- factor(comex$state)
comex$product <- factor(comex$product)
comex$country <- factor(comex$country)
comex$route <- factor(comex$route)
comex$type <- factor(comex$type)
############################################

######## Convertendo a coluna "date" para o formato "Date"
comex$date <- as.Date(comex$date)
#################################


######################
#Questão 1 -
#Primeiramente, precisamos criar duas novas colunas : mês e ano
comex <- comex %>% mutate(month = format(date,"%m"), year = format(date,"%Y")) #Esse código pega os elemetnos de date e mostra qual é o mês e o ano da abservação


df <- comex %>% filter(type== "Export",) %>% group_by(year,product) %>%
  filter(product %in% c('soybeans','soybean_oil','soybean_meal'))  %>% summarize(total = sum(tons))
df$year <- as.numeric(df$year)

annual_exports <- df %>% group_by(product) %>% ggplot(aes(year,total,color=product))+geom_line()
annual_exports
###########################################################################

#Questão 2
df2 <- comex %>% filter(type == "Export") %>%  
  filter(date >= "2014-01-01") %>% group_by(product) %>% summarize(total = sum(usd))
top5 <- ggplot(df2,aes(x=factor(product),y= total))+ geom_col(fill = "red")+xlab("Product") + ylab("Total in USD") 
top5
###############################



#Questão 3
df3 <- comex %>% filter(type == "Export") %>%filter(date >= "2014-01-01" & product == "corn")
corn_routes <-barplot(table(df3$route), main = "Corn Routes",ylab = "Frequency",col = 'darkblue')
routes <- with(comex,histogram(~route |product ), )

#Questão 4
df4 <- comex  %>%filter(date >= "2017-01-01" & product %in% c("corn",'sugar'))
top6parceiros <- df4 %>% count(country,sort = TRUE)  %>% top_n(6)
top6parceiros_barplot <- ggplot(top6parceiros,aes(country,n))+ geom_col(fill="darkred") + xlab("Country")+ ylab("Trades") +labs(title = "Top 6 partners in  the last 3 years")

top6parceiros_barplot

#Questão 5
df5 <-  comex %>% group_by(product) %>% count(state) %>% top_n(5)
ggplot(df5,aes(n,product,fill = state))+ geom_col() + ylab('Product') + xlab("Trades")


################
#Questão 6

# Selecionando as colunas importantes
df6 <- comex %>% filter(type== "Export",) %>% group_by(year,product) %>%
  filter(product %in% c('soybeans','corn','soybean_meal'))  %>% summarize(total = sum(tons))
df6$year <- as.numeric(df$year)




#Mostrando a evolução temporal desses elementos
annual_exports2 <- df6 %>% group_by(product) %>% ggplot(aes(year,total,color=product))+geom_line()
annual_exports2


# Transformando em time series e subsetting


soybean <- df6%>% filter(product=="soybeans")
soybean_meal <- df6 %>% filter(product=="soybean_meal")
corn <- df6 %>% filter(product=="corn")

#Transformando em ts
corn <- ts(corn[,3],start= 1997)
soybean <- ts(soybean[,3],start = 1997)
soybean_meal <- ts(soybean_meal[,3],start = 1997)

#Forecasting
f.corn <- forecast(corn, h = 11)
f.soybean <- forecast(soybean, h = 11)
f.soybean_meal <- forecast(soybean_meal, h = 11)



#Plotando os gráficos lado a lado para comparação
par(mfrow= c(1,1))
plot(f.corn, main = 'Corn  11 Years Forecast')
plot(f.soybean, main = 'Soybean  11 Years Forecast')
plot(f.soybean_meal, main = 'Soybean Meal  11 Years Forecast')

#############################################################

#Analisando os preços dos produtos

cov1 <- covar %>% filter(year >= 1997)
par(mfrow= c(1,3))
with(cov1,plot(year,price_soybeans,type = 'l',xlab = 'Year',ylab= 'Price'))
title(main = "Soybean Price over the Years")
with(cov1,plot(year,price_soybean_meal,type = 'l',xlab = 'Year',ylab= 'Price'))
title(main = "Soybean Meal Price over the Years")
with(cov1,plot(year,price_corn,type = 'l',xlab = 'Year',ylab= 'Price'))
title(main = "Corn Price over the Years")

####### Juntando as tabela
soybean <- df6%>% filter(product=="soybeans")
soybean_meal <- df6 %>% filter(product=="soybean_meal")
corn <- df6 %>% filter(product=="corn")
covcorn <-  corn  %>% inner_join(cov1, by = "year", suffix = c("_comex","_cov"))
covsoybean <- soybean %>% inner_join(cov1, by = "year", suffix = c("_comex","_cov"))
covsoybean_meal <- covsoybean <- soybean_meal %>% inner_join(cov1, by = "year", suffix = c("_comex","_cov"))
################


#### Analisando o impacto do preço do produto no total de de exportações

#Corn
with(covcorn,plot(price_corn,total, xlab ="Corn Price", ylab = "Total in Tons"))
abline(lm(covcorn$total ~ covcorn$price_corn))
summary(lm(covcorn$total ~ covcorn$price_corn))

#Soybeans
with(covsoybean,plot(price_soybeans,total, xlab ="Soybean Price", ylab = "Total in Tons"))
abline(lm(covsoybean$total ~ covsoybean$price_corn))
summary(lm(covsoybean$total ~ covsoybean$price_corn))

#Soybean Meal
with(covsoybean_meal,plot(price_soybean_meal,total))
abline(lm(covsoybean_meal$total ~ covsoybean_meal$price_corn, xlab ="Soybean Meal Price", ylab = "Total in Tons"))
summary(lm(covsoybean_meal$total ~ covsoybean_meal$price_corn))


#### Anaçisando o impacto do gdp global
#Corn
with(covcorn,plot(gdp_world,total, xlab= "World GDP",ylab = "Total in Tons"))
abline(lm(covcorn$total ~ covcorn$gdp_world))
summary(lm(covcorn$total ~ covcorn$gdp_world))

#Soybeans
with(covsoybean,plot(price_soybeans,total, xlab= "World GDP",ylab = "Total in Tons"))
abline(lm(covsoybean$total ~ covsoybean$gdp_world))
summary(lm(covsoybean$total ~ covsoybean$gdp_world))

#Soybean Meal
with(covsoybean_meal,plot(gdp_world,total, xlab= "World GDP",ylab = "Total in Tons"))
abline(lm(covsoybean_meal$total ~ covsoybean_meal$gdp_world))
summary(lm(covsoybean_meal$total ~ covsoybean_meal$gdp_world))
