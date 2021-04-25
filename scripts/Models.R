library(dplyr) # работа с табличками
library(tidyverse) # работа с табличками
library(lubridate) # работа с датами
library(broom) # обработка табличек с результатами
library(ggplot2) # графики
library(gridExtra) # несколько графиков в одном
library(urca) # работа с временными рядами
library(forecast) # работа с временными рядами
library(rugarch) # одномерные гарчи
library(rmgarch) # многомерные гарчи


# загрузка данных
dfpath <- str_replace(getwd(), '/scripts', '/data/returns.csv')
df <- read.csv(dfpath)
# конвертируем даты в формат дат
df$TRADEDATE <- as.Date(df$TRADEDATE, format = '%Y-%m-%d')
glimpse(df)
# отлично, все данные открываются, причём цифры в нужном формате

# посмотрим на распределения доходностей и сравним их с нормальными
compare_distribution <-
  function(x, name) {
    plot <- qplot(x, geom = 'density',
                  main = paste0('Распределение ', toString(name), ' (синее) и нормальное')) +
      geom_density(fill = 'blue', alpha = 0.4) +
      geom_density(aes(rnorm(200000, mean(x), sd(x))), fill = 'red', alpha = 0.25) +
      labs(x = '') +
      theme(plot.title = element_text(hjust = 0.5))

    return(plot)
  }

grid.arrange(compare_distribution(df[!is.na(df$FIVE), 'FIVE'], 'FIVE'),
             compare_distribution(df$'MGNT', 'MGNT'),
             compare_distribution(df$'LNTA', 'LNTA'),
             compare_distribution(df$'MVID', 'MVID'),
             ncol = 2)


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
# документация здесь:
# https://www.rdocumentation.org/packages/rugarch/versions/1.4-4/topics/ugarchspec-methods
# в модели среднего мы предполагаем, что доходность M.Video около нуля и константа не нужна
mvid_spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(3, 1), include.mean = FALSE))
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

# теперь попробуем учесть в модели не-Гауссово распределение доходностей
# у нас есть альтернатива - в R реализовано, например, распределение Стьюдента для случайной ошибки в GARCH
mvid_spec_t <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(3, 1), include.mean = FALSE),
                          distribution.model = 'std')
mvid_model_t <- ugarchfit(spec = mvid_spec_t, data = df$MVID)
# посмотрим на результаты оценивания
mvid_model_t
# сравним информационные критерии
infocrit_df <- cbind(infocriteria(mvid_model), infocriteria(mvid_model_t))
colnames(infocrit_df) <- c('GARCH(1, 1)', 'GARCH(1, 1) with T-errors')
infocrit_df
# модель с шоками из распределения Стьюдента лучше по всем информационным критериям


#### и так для остальных эмитентов, а также тест Купика, скользящие прогнозы и VaR


#######################
# Multivariate models #
#######################

# GARCH-BEKK

# DCC-GARCH