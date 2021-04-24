library(dplyr) # работа с табличками
library(tidyverse) # работа с табличками
library(readxl) # чтение данных из эксель-файла
library(lubridate) # работа с датами
library(ggplot2) # графики
library(urca) # работа с временными рядами
library(forecast) # работа с временными рядами
library(rugarch) # одномерные гарчи
library(rmgarch) # многомерные гарчи

# loading data
dfpath <- str_replace(getwd(), '/scripts', '/data/returns.csv')
df <- read.csv(dfpath)
# конвертируем даты в формат дат
df$TRADEDATE <- as.Date(df$TRADEDATE, format = '%Y-%m-%d')
glimpse(df)
# отлично, все данные открываются, причём цифры в нужном формате

#####################
# Univariate models #
#####################

# посмотрим на ряд доходностей М.Видео
tsdisplay(df$MVID, main = 'MVID returns')

# тест на стационарность проведён в Питоне, но сделаем его и здесь
mvid_adf <- ur.df(type = 'none', df$MVID, selectlags = 'AIC')
summary(mvid_adf)

# можно предложить модель ARIMA(1, 0, 1) по графику, проверим, какую модель предложить нам auto.arima
mvid_arima <- auto.arima(df$MVID)
summary(mvid_arima)
# предлагает ARIMA(3, 0, 1), но третий лаг AR не значим - оставим модель среднего ARIMA(2, 0, 1)

# проверим визуально на наличие условной гетероскедастичности
tsdisplay((df$MVID^2), main = 'MVID squared returns')
# выраженная гетероскедастичность
# наверно нужны какие-то тесты на г/ск в остатках, но пока это не важно

# наконец запишем спецификацию GARCH-модели для MVID
# в модели среднего мы предполагаем, что доходность M.Video около нуля и константа не нужна
mvid_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(1, 1), include.mean = FALSE))
mvid_model <- ugarchfit(spec = mvid_spec, data = df$MVID)
# посмотрим на результаты оценивания
mvid_model
# информационные критерии
infocriteria(mvid_model)
# построим кучу графиков и кайфанём что R сделал всё за нас
plot(mvid_model)
# особенно нас интересует, победили ли мы автокорреляцию и гетероскедастичность
plot(mvid_model, which = 10)
plot(mvid_model, which = 11)

#### и так для остальных эмитентов, а также тест Купика, скользящие прогнозы и VaR


#######################
# Multivariate models #
#######################

# GARCH-BEKK

# DCC-GARCH