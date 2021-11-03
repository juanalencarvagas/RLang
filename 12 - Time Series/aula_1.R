#################################################################################
#                               CRIANDO A SÉRIE                                 #
#################################################################################

# start = quando a série começa
# end = quando a série termina
# frequency = número de unidades por período

# Exemplo 1

vendas <- c(25, 35, 32, 39, 37, 40)
class(vendas) # tipo númerico não é usada em séreis temporais, é preciso ser "ts"
vendas <- ts (vendas)
vendas

vendas2 <- ts(vendas, start = 2021, frequency = 12) # definindo a frequência
vendas2
autoplot(vendas2, main = "Vendas", xlab = "Mês", ylab = "Unidades Vendidas")

# Exemplo 2 
serie = ts(c(100:231), start = c(2000,1), end = c(2010,12), frequency = 12)
serie
class(serie)
autoplot(serie)

plot.ts(serie, main = "Exemplo1")
AirPassengers
plot.ts(AirPassengers, main="Exemplo2")

ts (AirPassengers, frequency = 4, start = c(1959, 2)) # frequency 4 => Quarterly Data
ts (1:10, frequency = 12, start = 1990) # freq 12 => Monthly data. 
ts (AirPassengers, start=c(2009), end=c(2014), frequency=1) # Yearly Data

###############################################################################
### LENDO DADOS DE SÉRIES TEMPORAIS
###############################################################################

# Carregando a base Kings - idade da morte de sucessivos reis da Inglaterra
# Fonte: Hipel and Mcleod, 1994

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3) # ignora as 3 primeiras linhas da base
# save(kings, file = "kings.RData")
kings # avaliando a base
kingstimeseries <- ts(kings) # salvando os dados no formato de séries temporais (ST)
kingstimeseries # visualizando a série criada

# Carregando a base Births - número de récem nascidos em NY entre 1946 a 1959

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") # carregando a base
#save(births, file = "births.RData")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1)) # salvando o período de início, mês 01 de 1946
birthstimeseries # visualizando a base

# Carregando a base Souvenir - venda de souvernirs entre Janeiro de 1987 a Dezembro de 1993
# Fonte: Wheelwright and Hyndman, 1998

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") # carregando a base
#save(souvenir, file = "souvenir.RData")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))  # salvando o período de início, mês 01 de 1987
souvenirtimeseries # visualizando a base

###############################################################################
# Para avaliar quando iniciar a série temporal
start(USAccDeaths)
end(USAccDeaths)

#Avaliar graficamente várias séries
plot.ts(USAccDeaths)

###############################################################################
### PLOTANDO OS DADOS
###############################################################################

# LEMBRETE CONCEITUAL
# método aditivo - usar quando sazonalidade for constante (default)
# método multiplicativo - usar quando sazonalidade for crescente

plot.ts(kingstimeseries) # possibilidade de modelo aditivo
plot.ts(birthstimeseries) # possibilidade de modelo aditivo
plot.ts(souvenirtimeseries) # não parece ter a possibilidade de modelo aditivo
# A base souvernir tem as flutuações sazonais e aleatórias maiores ao longo do tempo

# Uma alterantiva para transforamr o dados seria calcular o LOG natural
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries) # modelo com LOG gera a possibilidade de modelo aditivo

#################################################################################
#                               CONCEITOS                                      #
#################################################################################

# Autocorrelação
# É a correlação da série com ela mesma. Em séries temporais, será a correlação
# entre a série e uma LAG (valores anteriores)

# Discussão sobre LAG

AirPassengers
lag(AirPassengers, -1)
lag(AirPassengers, -2)

# gerando lags para a série
tsData <- ts(AirPassengers)
tsData
laggedTS <- lag(tsData, 3)
myDf <- as.data.frame(tsData)
myDf <- slide(myDf, "x", NewVar = "xLag1", slideBy = -1)  # create lag1 variable
myDf <- slide(myDf, "x", NewVar = "xLead1", slideBy = 1)  # create lead1 variable
head(myDf)

# Séries com autocorrelação demonstra valores associados, ou seja, anteriores vão prever os próximos

fpp2::a10 # série disponível
acf(a10) # autocorrelação
pacf(a10) # autocorrelação parcial
ggtsdisplay(a10) # avaliação em visualização única

# Estacionariedade
# Série cujas características não mudam ao longo do tempo

# Ruído - série totalmente aleatória
# Característica: média e variância constante, sem tendência e sazonalidade
rb <- ts(rnorm(500))
autoplot(rb)

# Normalidade
# Distribuição normal tem forma de sino, com os dados distribuídos simetricamente
# ao redor da média

hist(rnorm(5000)) # expectativa e simétrica ao redor da média
hist(a10)
qqnorm(a10)
qqline(a10, col = "red")

################################################################################
#                               VISUALIZACAO GRÁFICA                            #
#################################################################################

# Formatando o gráfico
plot(USAccDeaths, xlab = 'Anos', ylab = 'Número de Mortes')
plot (USAccDeaths, type = 'o')

#Avaliando, e combinando, os gráficos em diferentes períodos
plot.ts(cbind(USAccDeaths,AirPassengers), main = 'Mortes x Transporte Aéreo', xlab = 'Anos')
plot.ts(cbind(USAccDeaths,AirPassengers), main = 'Mortes x Transporte Aéreo', xlab = 'Anos', nc=2) # lado a lado

# Agregando períodos
aggregate(USAccDeaths, nfrequency = 4, FUN = sum) # somas trimestrais
aggregate(USAccDeaths, nfreq = 1, FUN=mean) # médias anuais
plot(aggregate(USAccDeaths))
plot(aggregate(USAccDeaths, nfrequency = 4, FUN = sum))
monthplot(USAccDeaths, col.base =2, labels = month.abb)

# Visualizando uma janela temporal
janela = window(USAccDeaths, start=c(1973, 5), end = c(1975,7))
janela

diff(USAccDeaths) # diferença entre os meses
log(USAccDeaths) # logaritmo da série

# Análise da Autocorrelação (FAC) e Autocorrelação Parcial
# FCAp com defasagem 25:

a = acf(USAccDeaths, lag.max = 25)
a

p = pacf(USAccDeaths, lag.max = 25)
p

da = acf(diff(USAccDeaths), lag.max = 25) # diferença 
da

dp = pacf(diff(USAccDeaths), lag.max = 25)
dp

# Avaliar a sazonalidade
plot(stl(log(USAccDeaths), "periodic"))

###############################################################################
### DECOMPOSIÇÃO DE SÉRIES TEMPORAIS
###############################################################################

# Objetivo: separar os componentes, em geral, tendências e um componente irregular
# se for uma sére temporal sazonal, um componente sazonal

# Conceitos
# Soma da Tendência + Sazonalidade + Aleatório

# Tendência - padrão de crescimento ou decrescimento da série/média móvel centrada, coimputa variações cíclicas
# Sazonalidade - padrão de repetição em intervalos regulares
# Aleatório - não é tendência, nem sazonalidade, outros aspectos

# Modelo Aditivo
# Série = Tendência + Sazonalidade + Aleatório

# Modelo Multiplicativo
# Série = Tendência x Sazonalidade x Aleatório

# DECOMPOSIÇÃO DE SÉRIES NÃO SAZONAIS

# Componente de tendência e componente irregular. Envolve tentar separar a série temporal e estimar o componente de 
# tendência e o componente irregular

# Para estimar é comum usar o método de suaviazação, como o cáculo da média móvel simples da série temporal

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3) # estimar com uma média móvel simples de ordem 3
plot.ts(kingstimeseriesSMA3)

# Ainda existem muitas flutuações, assim, vamos estimar com uma média superior

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

# DECOMPOSIÇÃO DE SÉRIES SAZONAIS

# Componentes de tendência, sazonalidade e componente irregular
# Objetivo será separar os componentes

# variações sazonais e aleatórias parecem ser constante ao longo do texto
plot.ts(birthstimeseries) # dados originais, picos no verão e no inverno
birthstimeseriescomponents <- decompose(birthstimeseries) # estimando os componentes
birthstimeseriescomponents$seasonal # obter os valores estimados do componentes sazonal
plot(birthstimeseriescomponents)

# AJUSTAMENTO SAZONAL
# É preciso que a série possa ser descrita como um modelo adicional
# É possível ajustar estimando o componente sazonal e substraindo o componente sazonal da série orignal

plot.ts(birthstimeseries) 
birthstimeseriescomponents <- decompose(birthstimeseries) # estimar o componente sazonal
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

# A variação sazonal foi removida. Agora a série contém componentes de tendência e um componente irregular

#################################################################################
#                               TRANSFORMAÇÕES                                  #
#################################################################################

# Tornar séries normais e estacionárias

# Diferenciação - remove a tendênca da série, para ser estacionária
# Registro das mudanças das séries, diferença do segundo valor pelo primeiro valor

AirPassengers
diff(AirPassengers, 1) # diferença entre os meses
diff(AirPassengers, 20) # diferença entre os meses

autoplot(a10)
a10.diff <- diff(a10, 1)
autoplot(a10.diff) # Removeu a tendência
a10.diff2 <- diff(a10, 2)
autoplot(a10.diff2)
ndiffs(a10) # avaliar a quantidade de diferenciações necessárias

# Não foi sufuciente, então, 
# Transformação BoxCox (sazonalidade e distribuição normal)

lambda <- BoxCox.lambda(a10)
a10.bc <- BoxCox(a10, lambda = lambda)
hist(a10) # antes
hist(a10.bc) # atual
autoplot(a10.bc)
ap1 <- autoplot(a10)
ap2 <- autoplot(a10.bc)
ap1 + ap2
ap1 / ap2

serie.final <- diff(a10.bc, 1)
autoplot(serie.final)

# Teste de Dickey-Fuller Aumentado # teste de estacionariedade
# Se p valor < 0.05 indica que a série é estacionária
adf.test(serie.final) 
kpss.test(serie.final)

#################################################################################
# Exemplo 2
# Differencing: representa subtrair cada ponto de dados na série de seu sucessor

nsdiffs(AirPassengers)  # número para diferenciação sazonal necessário
AirPassengers_seasdiff <- diff(AirPassengers, lag=frequency(AirPassengers), differences=1)  # diferenciação sazonal
plot(AirPassengers_seasdiff, type="l", main="Diferença Sazonal")  # ainda não estacionária

# Estacionariedade
ndiffs(AirPassengers_seasdiff)  # número de diferenças necessárias para tornar a série estacionária
stationaryTS <- diff(AirPassengers_seasdiff, differences= 1)
plot(stationaryTS, type="l", main="Diferenças e Estacionária")  # aparenta ser estacionária

###############################################################################
### PREVISÃO COM ALISAMENTO EXPONENCIAL (EXPONENTIAL SMOOTHING)
###############################################################################

# Suavização Exponencial Simples

# Sendo possível descrever por meio do modelo aditivo com nível constante e sem sazonalidade.
# A suaviazação ocorre pelo parâmetro alfa entre 0 e 1. Sendo, 0 pouco peso nas observações
# mais recentes ao fazer previsões de valores futuros. 

###############################################################################

# Carregando a base chuva em Londres entre os anos de 1813-1912
# Fonte: Hipel and McLeod, 1994

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
#save(rain, file = "rain.RData")
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

# A média permanece, quase, constante em aproximadamente em 25, o que indica o uso 
# de um modelo aditivo. 

# Vamos usar a função HoltWinters() para isso é preciso definir os parâmetros beta e gamma.
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts

# O valor estimado do parâmetro alfa é de 0.024. Como o valor é próximo a zero a previsão
# está baseada em observações recentes e menos recentes.Por default a previsão é feita apenas
# para o mesmo período avaliado na série temporal. Logo, entre os anos de 1813-1912.

rainseriesforecasts$fitted # avaliandos os valores estimados
plot(rainseriesforecasts)

# Como medida de previsão calculamos o erro da soma dos quadrados para os erros de previsão dentro
# da amostra. 

rainseriesforecasts$SSE # o valor do erro da soma dos quadrados
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56) # utilizando o primeiro valor previsto

# Vamos prever um período além da série original
rainseriesforecasts2 <- forecast(rainseriesforecasts, h=8)
rainseriesforecasts2
plot(rainseriesforecasts2) # plotando o gráfico para verificar a previsão

# Os erros da previsão são calculados entre os valores observados menos os valores previstos
# Caso o modelo preditivo não possa ser aprimoraado, provavelmente não deve haver correlação entre os erros de
# previsão para as previsões sucessivas. Assim, outra técnica seria melhor empregada.

# Para avaliar usaremos o correlograma.
rainseriesforecasts2$residuals
acf(rainseriesforecasts2$residuals, lag.max = 20, na.action = na.pass)

# Vamos avaliar a significância por meio do teste Ljung-Box
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")

# Há pouca evidência de autocorrelação diferentes de zero nos erros de previsão dentro da amostra.
# Para garantir que seria o melhor modelo vamos verificar os erros de previsão são normalmente distribuídos
# ou seja, média e variância constante.

plot.ts(rainseriesforecasts2$residuals)

# Para avaliar vamos gerar um histograma
plotForecastErrors <- function(forecastErrors)
{
  forecastErrorsSd <- sd(x = forecastErrors,
                         na.rm = TRUE)
  forecastErrorsMin <- min(forecastErrors,
                           na.rm = TRUE) - forecastErrorsSd * 5
  forecastErrorsMax <- max(forecastErrors,
                           na.rm = TRUE) + forecastErrorsSd * 3
  forecastErrorsNorm <- rnorm(n = 10000,
                              mean = 0,
                              sd = forecastErrorsSd)
  binMin <- min(forecastErrorsMin, forecastErrorsNorm)
  binMax <- max(forecastErrorsMax, forecastErrorsNorm)
  binBreaks <- IQR(x = forecastErrors,
                   na.rm = TRUE) / 4
  bins <- seq(from = binMin,
              to = binMax,
              by = binBreaks)
  hist(x = forecastErrors,
       col = "#DCE319FF",
       freq = FALSE,
       breaks = bins)
  with(data = hist(x = forecastErrorsNorm,
                   plot = FALSE,
                   breaks = bins),
       expr = lines(x = mids,
                    y = density,
                    col = "#440154FF",
                    lwd = 3))
}

# plotando o histograma dos erros de previsão
plotForecastErrors(rainseriesforecasts2$residuals)

# O gráfico demonstra que a distribuição dos erros está centrada em zero e aproximadamente distribuída. 
# O teste Ljung-Box mostrou que há pouca evidência de autocorrelações diferentes de zero.
# O método de suavização exponencial simples fornece um modelo preditivo adequado

###############################################################################

# Holt's Suavização Exponencial

# Usado quando é possível utilizar um modelo aditivo com acréscimo ou decréscimo na tendência e sazonalidade
# O método estima o nível e a inclinação no ponto de tempo atual e é controlada por dois parâmetros alfa (ponto atual)
# e beta para inclinação do componente da tendência no ponto do tempo atual.
# Alfa e beta terão valores entre 0 e 1, sendo que próximo a zero temos pouco peso nas previsões mais recentes.

###############################################################################

# Carregando a base skirts - diâmetro anual das saias femininas na bainha, de 1866 a 1911
# Fonte: McLeod, 1994

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
# save(skirts, file = "skirts.RData")
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)

# É preciso configurar os parâmetros gama
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts

# O valor do alpha foi de 0.83 e beta 1. Os valores são altos e indicam a estimativa do valor atual do nível,
# quando a inclinação do componente de tendência se baseiam principalmente em observações recentes da série.

skirtsseriesforecasts$SSE

# Assim, o nível e a inclinação mudam ao longo do tempo. O valor da soma dos erros quadrados é 16954.

plot(skirtsseriesforecasts) # atenção para lag antes dos dados observados na previsão.
skirtsseries

# para corrigir o nível do valor inicial, e a diferença entre a segunda e a primeira observação
HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9) 

# Prevendo 19 pontos a mais que a série temporal
skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)

# linha azul representa com intervalos de predição de 80% com uma área sombreada em azul escuro e os 
# intervalos de predição de 95% com a área na cor clara 
plot(skirtsseriesforecasts2)

# skirtsseriesforecasts2 <- ts(skirts, start = 2021, frequency = 12) # definindo a frequência
# skirtsseriesforecasts2
# skirt.fcast <- hw(skirtsseriesforecasts2,h=19)

# Vamos avalair se o modelo preditivo pode ser melhorado ao verificar os erros de previsão na amostra
# mostram autocorrelações diferentes de zero nas defasagens de 1-20. 

acf(skirtsseriesforecasts2$residuals, lag.max=20, na.action = na.pass)

# O correlograma mostrou que a autocorrelação da amostra para os erros de previsão dentro da amostra no 
# defasamento 5 excede os limites de significância. Porém, é esperado que uma em cada 20 das autocorrelações 
# para os primeiros vinte atraso exceda o limite de significância de 95%. 

Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
# O teste retornou um p valor de 0.47, indicando que há pouca evidência de autocorrelações diferentes de zero 
# nos erros de previsão dentro da amostra nas defasagens 1-20.

plot.ts(skirtsseriesforecasts2$residuals) # gerando um time plot

plotForecastErrors <- function(forecastErrors)
{
  forecastErrorsSd <- sd(x = forecastErrors,
                         na.rm = TRUE)
  forecastErrorsMin <- min(forecastErrors,
                           na.rm = TRUE) - forecastErrorsSd * 5
  forecastErrorsMax <- max(forecastErrors,
                           na.rm = TRUE) + forecastErrorsSd * 3
  forecastErrorsNorm <- rnorm(n = 10000,
                              mean = 0,
                              sd = forecastErrorsSd)
  binMin <- min(forecastErrorsMin, forecastErrorsNorm)
  binMax <- max(forecastErrorsMax, forecastErrorsNorm)
  binBreaks <- IQR(x = forecastErrors,
                   na.rm = TRUE) / 4
  bins <- seq(from = binMin,
              to = binMax,
              by = binBreaks)
  hist(x = forecastErrors,
       col = "#DCE319FF",
       freq = FALSE,
       breaks = bins)
  with(data = hist(x = forecastErrorsNorm,
                   plot = FALSE,
                   breaks = bins),
       expr = lines(x = mids,
                    y = density,
                    col = "#440154FF",
                    lwd = 3))
}
plotForecastErrors(skirtsseriesforecasts2$residuals) # gerando um histograma

# O gráfico demonstra que os erros de previsão têm uma variância quase constante ao longo do tempo. O histograma
# de erros de previsão mostra que é plausível que os erros de previsão sejam normalmente distribuídos com méda
# zero e variância constante.

# O Teste de Ljung-Box mostra que há pouca evidência de autocorrelações nos erros de previsão, enquanto que o 
# time plot e o histograma dos erros de previsão mostram que é plausível que os erros de previsão sejam
# normalmente distribuídos com média zero e variância constante. Logo, é possível concluir que a suavização
# exponencial de Holt fornece um modelo preditivo adequado para os parâmetros avaliados, e que provavelmente
# não pode ser melhorado. Além disso, significa que as suposições nas quais os intervalos de predições de 
# 80% e 95% são validas

###############################################################################

# Holt-Winters Suavização Exponencial

# Caso tenha uma série que pode ser descrita por meio de modelos aditivos, tendência crescente
# ou decrescente e sazonalidade, o uso da suavização exponencial de Holt-Winders é indicada 
# para previsões de curto prazo

# Estima o nível, inclinação e componente sazonal no ponto de tempo atual. A suavização é
# controlada por três parâmetros: alfa, beta e gama para estimar o nível, inclinação e o 
# componente de tendência e sazonal a partir do ponto atual. Os parâmetros variam entre 0 e 1.
# Valores próximos a 0 significam que é colocado relativamente pouco peso nas observações mais 
# recentes ao fazer as previsões.

###############################################################################

# Carregando a base Souvenir - venda de souvernirs entre Janeiro de 1987 a Dezembro de 1993
# Fonte: Wheelwright and Hyndman, 1998

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") # carregando a base (caso não esteja carregada)
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))  # salvando o período de início, mês 01 de 1987

logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts

# Os valores estimados de alfa, beta e gama são 0.41, 0.00 e 0.95. O alfa é relativamente baixo
# indicando que a estimativa do nível no momento atual é baseada em observações no passado mais
# distante. O valor de beta indica que a estimativa da inclinação b do componente de tendência não
# é atualizado ao longo da série temporal e, em vez disso, é definida igual ao valor inicial. Assim,
# o nível muda bastante ao longo da série temporal, mas a inclinaçào do componente de tendência
# permanece praticamente a mesma. Já o valor gama é alto, indicandp que a estimativa do componente
# sazonal no momento atual é baseada apenas em observações recentes.

plot(souvenirtimeseriesforecasts)

# A técnica consegue prever os picos sazonais que ocorrem nos meses finais do ano.
# Vamos agora prever períodos que não estão na base, ou seja, de 1994 a 1998 (48 m)

souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts, h=48)
plot(souvenirtimeseriesforecasts2)

# As previsões são mostradas com uma linha azul e as áreas sombreadas em cores claras e escuras
# mostram intervalos de previsão de 80% a 95%. Para avaliar se o modelo melhorado verificando se
# os erros de previsão na amostra mostram autocorrelações diferentes de zero nas defasagens 1-20, 
# vamos realizar o correlograma e o teste de Ljung e Box.

acf(souvenirtimeseriesforecasts2$residuals, lag.max=20, na.action = na.pass)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")

# O correlograma apresenta que as autocorrelações para os erros de previsão dentro da amostra
# não excedem os limites de significância para defasagem 1-20. O valor p para o teste Ljunb-Box
# e 0,6, indicando que há pouca evidência de autocorrelações diferentes de zero nas defasagens. 

# Os erros de previsão têm variância constante ao longo do tempo e são normalmente distribuídos
# com a média zero, fazendo um gráfico de tempo de erros de previsão e um histograma.

plot.ts(souvenirtimeseriesforecasts2$residuals)            # gerando um time plot

plotForecastErrors <- function(forecastErrors)
{
  forecastErrorsSd <- sd(x = forecastErrors,
                         na.rm = TRUE)
  forecastErrorsMin <- min(forecastErrors,
                           na.rm = TRUE) - forecastErrorsSd * 5
  forecastErrorsMax <- max(forecastErrors,
                           na.rm = TRUE) + forecastErrorsSd * 3
  forecastErrorsNorm <- rnorm(n = 10000,
                              mean = 0,
                              sd = forecastErrorsSd)
  binMin <- min(forecastErrorsMin, forecastErrorsNorm)
  binMax <- max(forecastErrorsMax, forecastErrorsNorm)
  binBreaks <- IQR(x = forecastErrors,
                   na.rm = TRUE) / 4
  bins <- seq(from = binMin,
              to = binMax,
              by = binBreaks)
  hist(x = forecastErrors,
       col = "red",
       freq = FALSE,
       breaks = bins)
  with(data = hist(x = forecastErrorsNorm,
                   plot = FALSE,
                   breaks = bins),
       expr = lines(x = mids,
                    y = density,
                    col = "blue",
                    lwd = 2))
}
  
plotForecastErrors(souvenirtimeseriesforecasts2$residuals)  # gerando um histograma

# É compreensível que os erros de previsão tenham variação constante ao longo do tempo. A partir do histograma de erros 
# de previsão, os erros de previsão parecem ser normalmente distribuídos com média zero. Assim, há pouca evidência de 
# autocorrelação nas defasagens 1-20 para os erros de previsão, e os erros de previsão parecem ser normalmente distribuídos 
# com média zero e variância constante ao longo do tempo. Assim, a suavização exponencial de Holt-Winters fornece um modelo 
# preditivo adequado e que provavelmente não pode ser melhorado. Além disso, as suposições nas quais os intervalos de 
# predição foram baseados são provavelmente válidas.










