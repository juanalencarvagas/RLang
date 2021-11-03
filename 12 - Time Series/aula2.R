#################################################################################
#                          MODELAGEM E FORECAST                                 #
#################################################################################

# Transformações ###############################################################

## Antes de avaliarmos os modelos de previsão, vamos lembrar dois conceitos de 
# transformações:

# Diferenciação: usada para a série estacionária. 
## É a mudança da série, a diferença entre o valor e um valor anterior

autoplot(a10)
a10.diff <- diff(a10, 1)
autoplot(a10.diff) # Removeu a tendência
a10.diff2 <- diff(a10, 2)
autoplot(a10.diff2)
ndiffs(a10) # avaliar a quantidade de diferenciações necessárias

# BoxCox: se a diferenciação não for o bastante, pode ser usada a 
## transformação de BoxCox.

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

#skirtsseriesforecasts2 <- ts(skirts, start = 2021, frequency = 12) # definindo a frequência
#skirtsseriesforecasts2
#skirt.fcast <- hw(skirtsseriesforecasts2,h=19)

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
plotForecastErrors(souvenirtimeseriesforecasts2$residuals)  # gerando um histograma

# É compreensível que os erros de previsão tenham variação constante ao longo do tempo. A partir do histograma de erros 
# de previsão, os erros de previsão parecem ser normalmente distribuídos com média zero. Assim, há pouca evidência de 
# autocorrelação nas defasagens 1-20 para os erros de previsão, e os erros de previsão parecem ser normalmente distribuídos 
# com média zero e variância constante ao longo do tempo. Assim, a suavização exponencial de Holt-Winters fornece um modelo 
# preditivo adequado e que provavelmente não pode ser melhorado. Além disso, as suposições nas quais os intervalos de 
# predição foram baseados são provavelmente válidas.

# Métodos Simples ###############################################################

# Método da média: os valores previstos serão a média da série

# Método naive: os valores previstos serão o mesmo que o último valor registrado

# Método naive sazonal: os valores previstos serão o último valor registrado
# no mesmo período sazonal anterior

## Método da média
fpp2::a10 # série disponível
prev.media <- meanf(a10, h = 12)
autoplot(prev.media)

# O que está na linha azul se refere a média da série. O acerto esperado é baixo.
# Sombra mais forte, 80% de intervalo de confiança, 95% na sombra clara

## Método naive
prev.naive <- naive(a10, h = 12) # valores para os período seguintes
autoplot(prev.naive) # repete o último valor

## Método naive sazonal, limitação da avaliação pela sazonalidade
prev.snaive <- snaive(a10, h = 12) 
autoplot(prev.snaive)

# Método mais comum #############################################################

# ARIMA ########################################################################

## Os métodos de suavização exponencial são úteis para fazer previsões e não fazem 
## suposições sobre as correlações entre os valores sucessivos da série temporal. 
## No entanto, se o objetivo seria fazer intervalos de previsão para previsões 
## feitas usando métodos de suavização exponencial, os intervalos de previsão 
## requerem que os erros de previsão não sejam correlacionados e sejam normalmente 
## distribuídos com média zero e variância constante.

## Embora os métodos de suavização exponencial não façam suposições sobre correlações 
## entre valores sucessivos da série temporal, em alguns casos você pode fazer um 
## modelo preditivo melhor levando em consideração as correlações nos dados. 
## Modelos de Média Móvel Integrada Autoregressiva (ARIMA) incluem um modelo 
## estatístico explícito para o componente irregular de uma série temporal, que 
## permite autocorrelações diferentes de zero no componente irregular.

## ARIMA, série deve ser estacionária
# Modelo autorregresivo, integrado e de média móvel: ARIMA (p,d,q)
# p = termo autoregressivo, valores anteriores ou lags
# d = número de diferenciações, número necessário para prever
# q = termo da média movel, erros anteriores do modelo

###### EXEMPLO 1 ###############################################################

# Diferenciando as séries temporais
# A série precisa ser estacionário. Caso não seja é preciso diferenciar a série
# Se você tiver que diferenciar as séries temporais d vezes para obter uma série estacionária, 
# então terá um modelo ARIMA (p, d, q), onde d é a ordem de diferenciação usada.

# A diferença pode ser feito pela função "diff ()". Por exemplo, a série temporal 
# do diâmetro anual de saias femininas na bainha, de 1866 a 1911, não é estacionária em média, 
# pois o nível muda muito hora extra:

# Carregando a base skirts - diâmetro anual das saias femininas na bainha, de 1866 a 1911
# Fonte: McLeod, 1994

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)

skirtsseriesdiff1 <- diff(skirtsseries, differences=1) 
plot.ts(skirtsseriesdiff1)

# A série com as diferenças não parecem ser estacionárias. Portanto, uma alternativa
# seria diferenciar duas vezes

skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)

## A série temporal das segundas diferenças parece ser estacionária em média e variância, pois 
## o nível da série permanece aproximadamente constante ao longo do tempo, e a variância da série 
## parece aproximadamente constante ao longo do tempo. Assim, parece ser preciso 
## diferenciar a série temporal duas vezes para obter uma série estacionária.

## Se for preciso diferenciar os dados de série temporal originais "d" vezes para obter uma série 
## temporal estacionária, isso significa que o modelo ARIMA (p, d, q) é o indicado. Sendo 
# "d" a ordem de diferenciação usada. Por exemplo, para a série acima 
# tivemos que diferenciar a série temporal duas vezes e, portanto, a ordem de diferenciação (d) seria 2. 
# Isso significa que poderia usar um ARIMA (p, 2, q) modelo para sua série temporal. 
# A próxima etapa é descobrir os valores de peq para o modelo ARIMA.

# Outro exemplo é a série temporal kings:

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3) # ignora as 3 primeiras linhas da base
kings # avaliando a base
kingstimeseries <- ts(kings) # salvando os dados no formato de séries temporais (ST)
plot.ts(skirtsseries)

kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1) # aplicando a primeira diferença
plot.ts(kingtimeseriesdiff1)

# A série temporal das primeiras diferenças parece ser estacionária em média e variância e, portanto, 
# um modelo ARIMA (p, 1, q) é provavelmente apropriado para a série temporal kings. A partir da 
# série temporal das primeiras diferenças, removemos o componente de tendência da série temporal
# e ficamos com um componente irregular. O próximo passo será avaliar se existem correlações entre 
# termos sucessivos desse componente irregular; em caso afirmativo, isso poderia ajudar a fazer 
#um modelo preditivo para a série kings.

# Avaliando os modelos ARIMA

# Se a série temporal for estacionária, ou se transformou diferenciando "d" vezes, 
# a próxima etapa é selecionar o modelo ARIMA apropriado, o que significa encontrar os valores 
# mais apropriados de p e q para o modelo ARIMA (p, d, q). Para fazer isso, é preciso examinar o 
# correlograma e o correlograma parcial da série temporal estacionária.

# Para plotar um correlograma e um correlograma parcial usamos as funções "acf ()" e "pacf ()". 
# Para obter os valores reais das autocorrelações e autocorrelações parciais, definimos "plot = FALSE" 
# nas funções "acf ()" e "pacf ()".

acf(kingtimeseriesdiff1, lag.max=20)             # plotando um correlograma
acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # captando os valores da autocorrelação

# Com o correlograma concluímos que a autocorrelação na defasagem 1 (-0.360) excede os limites
# de significância, porém, as demais não excedem os limites.

pacf(kingtimeseriesdiff1, lag.max=20)             # plotando um correlograma
pacf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # captando os valores da autocorrelação parcial

## O correlograma parcial demonstra que as autocorrelações parciais nos atrasos 1, 2 e 3 excedem os limites
## de significância, são negativas e estão diminuindo lentamente em magnitude com o aumento do atraso 
## (atraso 1: -0,360, atraso 2: -0,335, atraso 3: -0,321 ). As autocorrelações parciais diminuem para 
## zero após o lag 3. 

## Uma vez que o correlograma é zero após o atraso 1, e o correlograma parcial cai para zero após o 
## atraso 3, os modelos ARMA (média móvel autorregressiva) são possíveis para a série temporal das 
## primeiras diferenças:
  
# Modelo ARMA (3,0), ou seja, ordem p = 3, uma vez que o autocorrelograma parcial é zero após o atraso 3,
# e o autocorrelograma cai para zero (embora talvez de forma abrupta demais para que este modelo seja 
# apropriado)
# Modelo ARMA (0,1), ou seja, um modelo de média móvel de ordem q = 1, uma vez que o autocorrelograma é 
# zero após o lag 1 e o autocorrelograma parcial cai para zero.
# Modelo ARMA (p, q), ou seja, um modelo misto com p e q maior que 0, uma vez que o autocorrelograma e o 
# correlograma parcial caem para zero (embora o correlograma provavelmente caia para zero muito 
# abruptamente para este modelo ser apropriado ). Assim, usaremos o princípio da parcimônia para decidir 
# qual modelo é o melhor: isto é, supomos que o modelo com o menor número de parâmetros é o melhor. 
# O modelo ARMA (3,0) tem 3 parâmetros, o modelo ARMA (0,1) tem 1 parâmetro e o modelo ARMA (p, q) tem 
# pelo menos 2 parâmetros. Portanto, o modelo ARMA (0,1) é considerado o melhor modelo.

# Modelo ARMA (0,1) é um modelo de média móvel de ordem 1, ou modelo MA (1). Este modelo pode ser 
# escrito como: X_t - mu = Z_t - (theta * Z_t-1), onde X_t é a série temporal estacionária que estamos 
# estudando (a primeira série diferenciada da base kings), mu é a média de série temporal X_t, Z_t é 
# ruído com média zero e variância constante, e theta é um parâmetro que pode ser estimado.

# Modelo MA (média móvel) é geralmente usado para modelar uma série de tempo que mostra dependências de 
# curto prazo entre observações sucessivas. Intuitivamente, faz sentido que um modelo MA possa ser 
# usado para descrever o componente irregular na série temporal da série kings, já que podemos esperar 
# que a idade de morte de um rei inglês em particular tenha algum efeito sobre as idades na morte do 
# próximo rei ou dois, mas não muito efeito nas idades na morte dos reis que reinam muito mais tempo 
# depois disso.

# Uma vez que um modelo ARMA (0,1) (com p = 0, q = 1) é considerado o melhor modelo candidato para a 
# série temporal das primeiras diferenças das idades na morte de reis ingleses, então a série temporal 
# original do idades de morte podem ser modeladas usando um modelo ARIMA (0,1,1) 
# (com p = 0, d = 1, q = 1, onde d é a ordem de diferenciação necessária).
  
###### EXEMPLO 2 ###############################################################

# Carregando a base Volcanodust - poeira sobre a poeira vulcânica entre 1500 e 1969
# Fonte: Hipel and Mcleod, 1994
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)

# Visualmente as flutuações aleatórias na série temporal são aproximadamente constantes em 
# tamanho ao longo do tempo, portanto, um modelo aditivo é provavelmente apropriado para descrever 
# essa série temporal. Além disso, a série temporal parece ser estacionária em média e variância, 
# pois seu nível e variância parecem ser aproximadamente constantes ao longo do tempo. Portanto, 
# não precisamos diferenciar esta série para ajustar um modelo ARIMA, mas podemos ajustar um modelo 
# ARIMA à série original (a ordem de diferenciação necessária, d, é zero aqui).

## Vamos avaliar o correlograma e correlograma parcial para defasagens 1-20 para investigar qual 
# modelo ARIMA usar:

acf(volcanodustseries, lag.max=20)             # plotando um correlograma
acf(volcanodustseries, lag.max=20, plot=FALSE) # captando os valores da autocorrelação

# As autocorrelações para as defasagens 1, 2 e 3 excedem os limites de significância, e que 
# as autocorrelações diminuem para zero após o atraso 3. As autocorrelações para as defasagens 
# 1, 2, 3 são positivas e diminuem em magnitude com o aumento lag (lag 1: 0,666, lag 2: 0,374, 
# lag 3: 0,162).

# A autocorrelação para defasagens 19 e 20 excede os limites de significância também, mas é provável 
# que seja devido ao acaso, uma vez que apenas excedem os limites de significância (especialmente 
# para defasagens 19), as autocorrelações para defasagens 4-18 não excedem os limites de significância, 
# e esperamos que 1 em cada 20 defasagens excedesse os limites de significância de 95% apenas 
# pelo acaso.

pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20, plot=FALSE)

# A autocorrelação parcial no lag 1 é positiva e excede os limites de significância (0,666), enquanto a 
# autocorrelação parcial no lag 2 é negativa e também excede os limites de significância (-0,126). 
# As autocorrelações parciais diminuem para zero após o lag 2. Uma vez que o correlograma cai para zero 
# após o desfasamento 3, e o correlograma parcial é zero após o desfasamento 2, os seguintes modelos ARMA 
# são possíveis para a série temporal:
  
# Modelo ARMA (2,0), uma vez que o autocorrelograma parcial é zero após o lag 2, e o correlograma cai 
# para zero após o lag 3, e o correlograma parcial é zero após o lag 2
# Modelo ARMA (0,3), uma vez que o autocorrelograma é zero após o atraso 3, e o correlograma parcial 
# cai para zero (embora talvez de forma abrupta demais para que este modelo seja apropriado).
# Modelo misto ARMA (p, q), uma vez que o correlograma e o correlograma parcial diminuem para zero 
# (embora o correlograma parcial talvez diminua abruptamente para este modelo ser apropriado).
# Modelo ARMA (2,0) tem 2 parâmetros, o modelo ARMA (0,3) tem 3 parâmetros e o modelo ARMA (p, q) 
# tem pelo menos 2 parâmetros. Portanto, usando o princípio da parcimônia, o modelo ARMA (2,0) e o 
# modelo ARMA (p, q) são modelos candidatos igualmente bons.

# Modelo ARMA (2,0) é um modelo autoregressivo de ordem 2, ou modelo AR (2). Este modelo pode ser 
# escrito como: X_t - mu = (Beta1 * (X_t-1 - mu)) + (Beta2 * (Xt-2 - mu)) + Z_t, onde X_t é a série 
# temporal estacionária (série volcanodust), mu é a média das séries temporais X_t, Beta1 e Beta2 
# são parâmetros a serem estimados e Z_t é o ruído com média zero e variância constante.

# Modelo AR (autoregressivo) é geralmente usado para modelar uma série de tempo que mostra dependências 
# de longo prazo entre observações sucessivas. Intuitivamente, faz sentido que um modelo AR possa 
# ser usado para descrever a série temporal da base, já que esperaríamos que a poeira vulcânica e os 
# respectivos níveis em um ano afetassem aqueles em anos muito posteriores, uma vez que a poeira 
# são improváveis desaparecer rapidamente.

# Se o modelo ARMA (2,0) (com p = 2, q = 0) for usado para modelar a série temporal significaria que 
# um modelo ARIMA (2,0,0) pode ser usado ( com p = 2, d = 0, q = 0, onde d é a ordem de diferenciação
# necessária). Da mesma forma, se um modelo ARMA (p, q) misto é usado, onde p e q são ambos maiores que 
# zero, então um modelo ARIMA (p, 0, q) pode ser usado.

###### EXEMPLO 3 ###############################################################

# Vamos avaliar a previsão da base anterior - "volcanodust"

volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesarima

# O modelo ARIMA (2,0,0) pode ser escrito como: X_t - mu = (Beta1 * (X_t-1 - mu)) + 
# (Beta2 * (Xt-2 - mu)) + Z_t, onde Beta1 e Beta2 são parâmetros a serem estimados. A saída da função 
# arima reporta que Beta1 e Beta2 são estimados como 0,7533 e -0,1268 aqui (dados como ar1 e ar2 na 
# saída).

# Com o ajuste do modelo ARIMA (2,0,0), podemos usar o modelo "forecast ()" para prever os valores 
# futuros do índice vulcânico. Os dados originais incluem os anos 1500-1969. Para fazer previsões 
# para os anos 1970-2000 (mais 31 anos):

volcanodustseriesforecasts <- forecast(volcanodustseriesarima, h=31)
volcanodustseriesforecasts
plot(volcanodustseriesforecasts) # visualização da série

# Atenção: o modelo previu valores negativos para o índice de poeira vulcânica, mas essa variável 
# só pode ter valores positivos! A razão é que as funções utilizadas não sabem que a variável só pode 
# assumir valores positivos. Claramente, o modelo preditivo atual não nos ajuda.

# Vamos investigar se os erros de previsão parecem estar correlacionados e se  são normalmente 
# distribuídos com média zero e variância constante. Para verificar as correlações entre erros de 
# previsão sucessivos, podemos fazer um correlograma e usar o teste Ljung-Box:

acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")

# O correlograma mostra que a autocorrelação no lag 20 excede os limites de significância. No entanto, 
# provavelmente se deve ao acaso, uma vez que esperaríamos que uma em 20 autocorrelações da amostra 
# excedesse os limites de significância de 95%. Além disso, o valor p para o teste Ljung-Box é 0,2, 
# indicando que há pouca evidência para autocorrelações diferentes de zero nos erros de previsão para 
# defasagens 1-20.

# Para verificar se os erros de previsão são normalmente distribuídos com média zero e variância 
# constante, fazemos um gráfico de tempo dos erros de previsão e um histograma:

plot.ts(volcanodustseriesforecasts$residuals)            # gráfico de tempos de erro de previsão
plotForecastErrors(volcanodustseriesforecasts$residuals)

# O gráfico de tempo dos erros de previsão mostra que os erros de previsão parecem ter uma variação 
# quase constante ao longo do tempo. No entanto, a série temporal de erros de previsão parece ter uma 
# média negativa, em vez de uma média zero. Podemos confirmar isso calculando o erro médio de previsão, 
# que acaba sendo cerca de -0,22:

# O histograma de erros de previsão mostra que embora o valor médio dos erros de previsão seja negativo, 
# a distribuição dos erros de previsão é inclinada para a direita em comparação com uma curva normal. 
# Portanto, parece que não podemos concluir que os erros de previsão são normalmente distribuídos 
# com média zero e variância constante! Portanto, é provável que o modelo ARIMA (2,0,0) para a série 
# temporal do índice de poeira vulcânica não seja o melhor modelo que poderíamos fazer e quase 
# definitivamente poderia ser melhorado!

###### EXEMPLO 4 ###############################################################

# Com a análise feita vamos propor um modelo (0,1,1)
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
kingstimeseriesarima

# Com o modelo ARIMA (0,1,1) na série temporal estamos ajustando um modelo ARMA (0,1) à série 
# das primeiras diferenças. Um modelo ARMA (0,1) pode ser escrito X_t - mu = Z_t - (theta * Z_t-1), 
# onde theta é um parâmetro a ser estimado. A partir da saída da função R "arima ()", o valor 
# estimado de theta (dado como 'ma1' na saída R) é -0,7218 no caso do modelo ARIMA (0,1,1) ajustado à 
# série temporal de idades na morte de reis.

# Vamos agora prever as idades de morte dos próximos dez reis ingleses
kingstimeseriesforecasts <- forecast(kingstimeseriesarima, h=5)
kingstimeseriesforecasts

kingstimeseries # para verificar a idade da última observação

# A série inclui as idades de morte de 42 reis ingleses. A função forecast retornou a 
# previsão da idade de morte dos próximos cinco reis ingleses (reis 43-47), bem como 
# intervalos de predição de 80% e 95% para essas predições. A idade de morte do 42º rei inglês foi de 
# 56 anos (o último valor observado em nossa série temporal), e o modelo ARIMA fornece a idade prevista 
# para a morte dos próximos cinco reis como 67,8 anos.

# Para traçar as idades de morte observadas para os primeiros 42 reis, bem como as idades que 
# seriam previstas para esses 42 reis e para os próximos 5 reis usando nosso modelo ARIMA (0,1,1), 
# usamos:

plot(kingstimeseriesforecasts)

# Como no caso dos modelos de suavização exponencial seria interessante investigar se os erros 
# de previsão de um modelo ARIMA são normalmente distribuídos com média zero e variância constante, 
# e se existem correlações entre erros de previsão sucessivos.

acf(kingstimeseriesforecasts$residuals, lag.max=20) # avaliando o correlograma
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box") # teste de Ljung-Box

# Como o correlograma mostra que nenhuma das autocorrelações da amostra para defasagens 1-20 excede 
# os limites de significância, e o valor p para o teste Ljung-Box é 0,9, podemos concluir que há muito 
# pouca evidência para autocorrelações diferentes de zero no erros de previsão nas defasagens 1-20.

# Para investigar se os erros de previsão são normalmente distribuídos com média zero e variância 
# constante, podemos fazer um gráfico de tempo e histograma (com curva normal sobreposta) dos erros 
# de previsão:

plot.ts(kingstimeseriesforecasts$residuals) # avaliação dos resíduos
plotForecastErrors(kingstimeseriesforecasts$residuals)

# O gráfico de tempo dos erros de previsão dentro da amostra mostra que a variação dos erros 
# de previsão parece ser aproximadamente constante ao longo do tempo (embora talvez haja uma variação 
# ligeiramente maior para a segunda metade da série temporal). 
# O histograma da série temporal mostra que os erros de previsão são aproximadamente normalmente 
# distribuídos e a média parece ser próxima de zero. Portanto, é plausível que os erros de previsão 
# sejam normalmente distribuídos com média zero e variância constante. Uma vez que erros de previsão 
# sucessivos não parecem estar correlacionados, e os erros de previsão parecem ser normalmente 
# distribuídos com média zero e variância constante, o ARIMA (0,1,1) parece fornecer um modelo preditivo 
# adequado para as idades de morte de Reis ingleses.

###### EXEMPLO 5 ###############################################################

# Passo a passo, Box e Jenkis (1976)
# 1: Plotar a série e examinar;
# 2: Diferenciar a série até ficar estacionária e fazer transformações, se necessário;
# 3: Usar séries diferenciadas para definir p e q;
# 4: Implementar o Arima nos dados originais;
# 5: Checar se é um bom modelo;
# 6: Usar o modelo para fazer previsões.

# Passo 1
autoplot(a10)
dec <- decompose(a10)
autoplot(dec)

# Passo 2 Uma diferenciação e transformação de BoxCox
autoplot(serie.final) # d = 1 e BC

# Passo 3
pacf(serie.final)  #p=1 e 1, função de autocorrelação parcial
# p = autoregressivo, valores anteriores
acf(serie.final)  #q=1 e 1, função de autocorrelação
ndiffs(a10) # para avaliar as diferenciações
nsdiffs(a10) # para avaliar diferenças sazonais
# d=1 e 1

# Passo 4
mod.arima <- Arima(a10, order = c(1, 1, 1), seasonal = c(1, 1, 1), lambda = lambda)
# usamos o lambda devido a transformação de BoxCox
summary(mod.arima)
autoplot(a10) + autolayer(mod.arima$fitted)

# modelo ficou melhor no início da série

# Passo 5
checkresiduals(mod.arima)

# Passo 6
prev.arima <- forecast(mod.arima, h = 12)
autoplot(prev.arima)

## Auto-Arima - automatiza os melhores parâmetros, p e q do Arima
# Passo a passo
# Passo 1: Executar Auto-Arima para verificar os melhores parâmetros;
# Passo 2: Implementar o Arima nos dados originais;
# Passo 3: Checar se é um bom modelo;
# Passo 4: Usar o modelo para fazer previsões.

# Passo 1
auto.arima(a10, lambda = lambda, trace = TRUE, approximation = FALSE)  #ARIMA(3,0,0)(2,1,1)

# Passo 2
mod.aa <- Arima(a10, order = c(3, 0, 0), seasonal = c(2, 1, 1), lambda = lambda,
                include.drift = TRUE)
# Passo 3
checkresiduals(mod.aa)

# Passo 4
prev.aa <- forecast(mod.aa, h = 12)
autoplot(prev.aa)

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

####### Avaliação por ARIMA e Prophet ##########################################

# ARIMA ########################################################################

data(AirPassengers) 
AirPassengers
plot(AirPassengers, ylab="Passengers", type="o", pch =20) # visualizando a série

# Separação entre treino e teste (período de dois anos)
df_train<- window(AirPassengers, end = c(1958, 12))
df_test <- window(AirPassengers, start = c(1959, 01))

# A partir do fluxio de Box e Jenkins é possível concluir que a variância não é constante, 
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

# MAPE - erro absoluto do percentual da média
## Medida de precisão. Mede a precisão como uma porcentagem e pode ser calculado como o 
## erro percentual absoluto médio para cada périodo de tempo menos os valores reais
# divididos pelos valores reais.

# RMSE - raiz do erro quadrático da média
## Usado para avaliar a medida das diferenças entre os valores (amostra ou população) previstos
## por mum modelo ou um estimador e os valores observados

# MAE - erro absoluto da média
## Medida de erros entre observações emparelhadas que expressam o mesmo fenômeno

err = df_test - arima_f$mean
mape <- mean(abs(err) / (arima_f$mean+ err)) * 100
rmse <- sqrt(mean(err^2, na.rm = TRUE)) 
mae <- mean(abs(err), na.rm = TRUE) 
cbind(mape, rmse, mae)

# PROPHET ######################################################################

df_train = subset(air_passengers, ds < "1959-01-01")
df_test = subset(air_passengers, ds >= "1959-01-01")

# Como foi analisado antes, a sazonalidade não é constante no tempo, mas aumenta 
# com a tendência. Os modelos aditivos não são os melhores para lidar com essas 
# séries temporais. Mas com o Prophet podemos passar da sazonalidade aditiva para 
# a sazonalidade multiplicativa por meio do parâmetro "seasonality_mode".

m <- prophet(df_train,seasonality.mode = 'multiplicative')

# Vamos definir o período da previsão e a frequência (m, s, a)
future <- make_future_dataframe(m, 24, freq = 'm', include_history = F)
forecast <- predict(m, future)
plot(m, forecast)

# Para efeito de comparação, vamos avaliar o modelo com o RMSE, MAE e MAPE.

pred = forecast$yhat
err = df_test$y - forecast$yhat
mape <- mean(abs(err) / (pred+ err)) * 100
rmse <- sqrt(mean(err^2, na.rm = TRUE)) 
mae <- mean(abs(err), na.rm = TRUE) 
cbind(mape, rmse, mae)

# Para facilitar, vamos comparar os dois

## Modelo Arima
# mape     rmse      mae
# 2.356519 14.12564 10.62677

## Modelo Prophet
# mape     rmse      mae
# 5.463905 31.08188 25.89196

