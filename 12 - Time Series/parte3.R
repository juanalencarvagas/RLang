# Instalação e Carregamento de Todos os Pacotes --------------------------------

pacotes <- c("forecast","tidyverse","prophet")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################################################
###### HOLT-WINTERS VS ARIMA ###################################################
################################################################################

# Lembrete
# Método aditivo - sazonalidade for constante (default)
# Método multiplicativo - sazonalidade for crescente

# Criando a base de dados
values <- c(92.1,  92.6,  89.5,  80.9,  95.6,  72.5,  71.2,  78.8,  73.8,  83.5,  
           97.9, 93.4,  98.0,  90.2,  96.7, 100.0, 103.6,  74.6,  78.9,  92.0,  
           83.4,  98.1, 109.9, 102.2, 102.1,  96.2, 106.9,  95.1, 113.4,  84.0, 
           88.6,  94.9,  94.7, 105.7, 108.6, 101.9,  113.9, 100.9, 100.2,  91.9,
           99.6,  87.2,  92.1, 104.9, 103.4, 103.3, 103.9, 108.5)

# Formatando como Série Temporal
time_series <- ts(values, start = 2015, frequency =12)

# Decomposição
autoplot(decompose(time_series)) + ggtitle("Decomposition of the series") + 
  theme(plot.title = element_text(size=8))

# Modelando: ARIMA e Holt-Winter, auto.arima()

# ARIMA
forecast_arima <- auto.arima(time_series, seasonal=TRUE, stepwise = FALSE, approximation = FALSE) 

# Caso a tenha problema com a linha 513, reinstale o pacote "forecast"
# E claro, ative o pacote novamente para seguir.
# O, eventual, erro se refere a dependência com pacotes similares de previsão
forecast_arima = forecast(forecast_arima, h=60)
plot(forecast_arima)

# Holt-Winters
forecast_hw <- hw(time_series, seasonal="multiplicative", h=60)
summary(forecast_hw) # avaliando os resultados 
plot(forecast_hw) # plotando os resultados

# Forecasting

# ARIMA
autoplot(time_series, series=" Historical data") +
  autolayer(forecast_arima, series=" ARIMA Forecast") +
  ggtitle(" ARIMA forecasting") +
  theme(plot.title = element_text(size=8))

# Holt-Winters
autoplot(time_series, series=" Historical data") + 
  autolayer(forecast_hw, series="Holt-Winter forecast") +
  ggtitle("HW Exponential Smoothing") +
  theme(plot.title = element_text(size=8))

# Avaliação dos modelos

# Modelo de previsão
forecast_arima['model'] #ARIMA
forecast_hw['model'] #Holt Winter

# Acurácia do Modelo
accuracy(forecast_arima) #ARIMA
accuracy(forecast_hw) #Holt Winter

# Diferenças no RMSE, porém, em MAE não é significativa. Em termos de AIC, ARIMA
# parece ser um modelo melhor. Não é recomendável comparar o AIC entre o ARIMA
# e o Holt-Winter.

################################################################################
# PROPHET ######################################################################
# Detalhes podem ser acessados em: https://facebook.github.io/prophet/ #########
################################################################################

# Base - quantidade de visualizações no perfil do Wikipedia do Lebron James
# Fonte: https://pageviews.toolforge.org/

# Fases: 
# 1. Explorando os dados
# 2. Predições Básicas
# 3. Inspecionando Componentes do Modelo
# 4. Personalizando feriados e eventos

# 1. Explorando os dados
load("lebron.RData") # carregando a base
colnames(lebron) <- c("ds", "y")  # renomeando as colunas
head(lebron) # visualizando a base
lebron$y <- log10(lebron$y) # aplicando logaritmo base 10 na variável y
View(summary(lebron)) # explorando os dados
plot(y ~ ds, lebron, type = "l") # gráfico da série

# 2. Predições Básicas
m <- prophet(lebron)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
plot(m, forecast) # visualização simples para compreensão

# Avaliação por valores brutos com o valor previsto por dia e intervalos de incerteza
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(forecast) # previsão por componentes

# 3. Inspecionando Componentes do Modelo
prophet_plot_components(m, forecast)

################################################################################
###### ARIMA e PROPHET #########################################################
################################################################################

data(AirPassengers) 
AirPassengers
plot(AirPassengers, ylab="Passengers", type="o", pch =20) # visualizando a série

# Separação entre treino e teste (período de dois anos)
df_train<- window(AirPassengers, end = c(1958, 12))
df_test <- window(AirPassengers, start = c(1959, 01))

# A partir do fluxo de Box e Jenkins é possível concluir que a variância não é constante, 
# porque aumenta e muda com o tempo, portanto a transformação do log é necessária. Além disso, 
# esta série temporal não é estacionária em média, considerando a sazonalidade, portanto, a 
# diferença de sazonalidade é necessária.

ggtsdisplay(diff(log(AirPassengers), 12)) # avaliação da autocorrelação

# ACF e PACF sugerem um modelo auto regressivo de ordem 2 e um modelo MA de ordem 1. 
# Assim, o modelo ARIMA (2,0,0) (0,1,1) é selecionado e é treinado com o conjunto de treinamento. 
# Dois parâmetros são definidos: include.constant e lambda. O primeiro adiciona ao modelo a interceptação. 
# O outro, em vez disso, define a transformação do log.

arima_1 <- Arima(df_train, c(2, 0, 0), c(0, 1, 1), include.constant = TRUE, lambda = 0)

ggtsdisplay(arima_1$residuals)

# Não há uma autocorrelação automática significativa entre as defasagens. O modelo 
# pode prever os últimos dois anos.

arima_f <- forecast(arima_1, 24)
forecast(arima_1, 24) %>% autoplot()

# Vamos avaliar o modelo com o RMSE, MAE e MAPE.

# RMSE - raiz do erro quadrático da média
## Usado para avaliar a medida das diferenças entre os valores (amostra ou população) previstos
## por mum modelo ou um estimador e os valores observados

# MAPE - erro absoluto do percentual da média
## Medida de precisão. Mede a precisão como uma porcentagem e pode ser calculado como o 
## erro percentual absoluto médio para cada período de tempo menos os valores reais
# divididos pelos valores reais.

# MAE - erro absoluto da média
## Medida de erros entre observações emparelhadas que expressam o mesmo fenômeno

err = df_test - arima_f$mean
mape <- mean(abs(err) / (arima_f$mean+ err)) * 100
rmse <- sqrt(mean(err^2, na.rm = TRUE)) 
mae <- mean(abs(err), na.rm = TRUE) 
cbind(mape, rmse, mae)