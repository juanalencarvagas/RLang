# Instalação e Carregamento de Todos os Pacotes --------------------------------

pacotes <- c("fpp2","tidyverse","gridExtra","data.table","ggseas","knitr","zoo")


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
###### SUAVIZAÇÃO EXPONENCIAL ##################################################
################################################################################

# Criando a base de treino e teste
# base: ações do Google do pacote fpp2
goog
goog.train <- window(fpp2::goog, 
                     end = 900)
goog.test <- window(fpp2::goog, 
                    start = 901)

# base: AirPassengers do pacote fpp2
qcement
qcement.train <- window(fpp2::qcement, 
                        end = c(2012, 4))
qcement.test <- window(fpp2::qcement, 
                       start = c(2013, 1))

################################################################################
### SUAVIZAÇÃO EXPONENCIAL SIMPLES (SES) #######################################
################################################################################

# EXEMPLO 1 

# Aplicando o SES na base com dados do Google
ses.goog <- ses(goog.train, 
                alpha = .2,
                h = 100)
autoplot(ses.goog)

# Com o gráfico avaliamos que não está captando a tendência atual

# EXEMPLO 2

# Removendo a tendência
goog.dif <- diff(goog.train)
autoplot(goog.dif)

# Reaplicado o SES com os dados filtrados
ses.goog.dif <- ses(goog.dif,
                    alpha = .2, 
                    h = 100)
autoplot(ses.goog.dif)

# A tendência parece satisfatória. Agora precisamos comparar nossa previsão com
# nosso conjunto de dados de validação ou teste. Como os dados de treino foram 
# diferenciados, vamos criar uma validação diferenciada parao o conjunto de teste.

# Vamos criar uma validação de conjunto diferenciada e comparar com a previsão 
# dos dados. O intervalo de alpha será entre 0.01 e -0.99. A ideia será entender
# o nível de minimização do teste de RMSE. 
# Removendo a tendência e avaliando os dados de teste

# EXEMPLO 3

goog.dif.test <- diff(goog.test)
accuracy(ses.goog.dif, goog.dif.test)

# Comparando os modelos
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(goog.dif, alpha = alpha[i],
             h = 100)
  RMSE[i] <- accuracy(fit, 
                      goog.dif.test)[2,2]
}

# Convertendo os dados e identificando o valor de Alfa
alpha.fit <- data.frame(alpha, RMSE)
alpha.min <- filter(alpha.fit, 
                    RMSE == min(RMSE))

# Plotando o RMSE vs Alpha
ggplot(alpha.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min,
             aes(alpha, RMSE), 
             size = 2, color = "red")

# Agora vamos reajustar a previsão SES com alpha = 0.05

# EXEMPLO 4

# Criando treino e validando a base do Google
goog.train <- window(fpp2::goog, 
                     end = 900)
goog.test <- window(fpp2::goog, 
                    start = 901)

# Removendo a tendência
goog.dif <- diff(goog.train)

# Reajustando o alpha = .05
ses.goog.opt <- ses(goog.dif, 
                    alpha = .05,
                    h = 100)

# Avaliando a performance;performance eval
accuracy(ses.goog.opt, goog.dif.test)

# Plotando os resultados
p1 <- autoplot(ses.goog.opt) +
  theme(legend.position = "bottom")
p2 <- autoplot(goog.dif.test) +
  autolayer(ses.goog.opt, alpha = .5) +
  ggtitle("Predicted vs. actuals for 
                 the test data set")

grid.arrange(p1, p2, 
             nrow = 1)

################################################################################
### HOLT'S #####################################################################
################################################################################

# EXEMPLO 1

# Aplicando o método Holt's method on
holt.goog <- holt(goog.train,
                  h = 100)
autoplot(holt.goog) # plotando

# Não fizemos a definição do valor alfa e beta. Ao mencionarmos qualquer valor
# para alfa e beta a função holt() identificará o ideal. Assim, se valor do alfa
# for de 0,9967 irá identificar um aprendizado rápido, e se o beta for 0,0001
# indicará um aprendizado lento

# EXEMPLO 2
# Vamos configurar o alfa e beta

# Método Holt's
holt.goog$model

# Acurácia do modelo
accuracy(holt.goog, goog.test)

# O valor ideal, beta = 0.0001 será usado para remover erros do conjunto de 
# treinamento. Podemos ainda ajustar o valor beta para o ideal

# EXEMPLO 3

# Vamos tentar encontrar o valor ideal de beta por meio do loop variando 
# de 0.0001 a 0,5 para minimizar o teste RMSE. Veremos que 0,0601 será o valor
# beta que diminuirá o RMSE.

# Identificado o parâmetro alpha ideal
beta <- seq(.0001, .5, by = .001)
RMSE <- NA

for(i in seq_along(beta)) {
  fit <- holt(goog.train,
              beta = beta[i], 
              h = 100)
  RMSE[i] <- accuracy(fit, 
                      goog.test)[2,2]
}

# Convertendo os dados e identificando o menor valor de alpha
beta.fit <- data.frame(beta, RMSE)
beta.min <- filter(beta.fit, 
                   RMSE == min(RMSE))

# Plotando RMSE vs. alpha
ggplot(beta.fit, aes(beta, RMSE)) +
  geom_line() +
  geom_point(data = beta.min, 
             aes(beta, RMSE), 
             size = 2, color = "red")

# Agora vamos refinar o modelo para obter o melhor valor de beta

# EXEMPLO 4

holt.goog <- holt(goog.train,
                  h = 100)

# Novo modelo para otimizar o beta
holt.goog.opt <- holt(goog.train,
                      h = 100,
                      beta = 0.0601)

# Acurácia do primeiro modelo
accuracy(holt.goog, goog.test)

# Acurácia do novo modelo ideal
accuracy(holt.goog.opt, goog.test)

# Plotando
p1 <- autoplot(holt.goog) +
  ggtitle("Original Holt's Model") +
  coord_cartesian(ylim = c(400, 1000))

p2 <- autoplot(holt.goog.opt) +
  ggtitle("Optimal Holt's Model") +
  coord_cartesian(ylim = c(400, 1000))

grid.arrange(p1, p2, 
             nrow = 1)

# O modelo ideal em em comparação com o modelo original é  mais conservador. 
# Além disso, o intervalo de confiança do modelo ideal é mais extremo.

################################################################################
###### HOLT-WINTERS ############################################################
################################################################################

# Método sazonal de Holt-Winter é usado para dados com padrões e tendências sazonais. 
# Pode ser implementado usando a estrutura Aditiva ou usando a estrutura Multiplicativa
# dependendo do conjunto de dados. A estrutura ou modelo aditivo é usado quando 
# o padrão sazonal de dados tem a mesma magnitude ou é consistente em toda a extensão,
# enquanto a estrutura ou modelo Multiplicativo é usado se a magnitude do padrão sazonal
# dos dados aumenta ao longo do tempo. 
# São usados três parâmetros de suavização - alfa, beta e gama.

# Vamos usar a base qcement do pacote fpp2

# Exemplo 1

# Carregando a base de dados
qcement.train <- window(qcement, 
                        end = c(2012, 4))
qcement.test <- window(qcement, 
                       start = c(2013, 1))

# Aplicando o Holt-Winters 
autoplot(decompose(qcement))

# Para criarmos um modelo aditivo que lida com erro, tendência e sazonalidade 
# usaremos a função ets () para escolher o melhor modelo aditivo. 

# EXEMPLO 2

# applying ets
qcement.hw <- ets(qcement.train,
                  model = "AAA")

autoplot(forecast(qcement.hw)) # execute normalmente após a instalação

# Agora vamos avaliar a acurácia e resíduos

# EXEMPLO 3

qcement.hw <- ets(qcement.train, model = "AAA")

# Avaliando o modelo
summary(qcement.hw)
checkresiduals(qcement.hw)

# Previsão para os próximos 5 quadrimestres
qcement.f1 <- forecast(qcement.hw,
                       h = 5)
# Avaliando a acurácia
accuracy(qcement.f1, qcement.test)

# Agora vamos avaliar como o modelo multiplicativo funciona usando ets ().
# Para isso, o parâmetro do modelo de ets () será "MAM".

# EXEMPLO 4

# Aplicando ETS
qcement.hw2 <- ets(qcement.train,
                   model = "MAM")
checkresiduals(qcement.hw2)

# Vamos otimizar o parâmetro gama para minimizar a taxa de erro. O valor do gama
# será 0,21, junto como isso vamos identificar a precisão e plotar os valores preditivos

# EXEMPLO 5
qcement.hw <- ets(qcement.train,
                  model = "AAA")

# Previsão para os próximos 5 quadrimestres
qcement.f1 <- forecast(qcement.hw,
                       h = 5)

# Avaliando a acurácia
accuracy(qcement.f1, qcement.test)

gamma <- seq(0.01, 0.85, 0.01) # estipulando o gamma
RMSE <- NA

for(i in seq_along(gamma)) {
  hw.expo <- ets(qcement.train, 
                 "AAA", 
                 gamma = gamma[i])
  future <- forecast(hw.expo, 
                     h = 5)
  RMSE[i] = accuracy(future, 
                     qcement.test)[2,2]
}

error <- data.frame(gamma, RMSE)
minimum <- filter(error, 
                  RMSE == min(RMSE))

ggplot(error, aes(gamma, RMSE)) +
  geom_line() +
  geom_point(data = minimum, 
             color = "blue", size = 2) +
  ggtitle("gamma's impact onforecast errors",
          subtitle = "gamma = 0.21 minimizes RMSE")

# Modelos aditivos anteriores
# Erro, tendência e sazonalidade
accuracy(qcement.f1, qcement.test)

# Novo modelo com parâmetro gama ótimo
qcement.hw6 <- ets(qcement.train,
                   model = "AAA", 
                   gamma = 0.21)

qcement.f6 <- forecast(qcement.hw6, 
                       h = 5)
accuracy(qcement.f6, qcement.test)

# Valores preditos
qcement.f6
autoplot(qcement.f6)

# EXEMPLO 6

# Acessando a base do pacote "ggseas"
nzdata <- data.table(nzbop) 
nzdata <- nzdata[!((Account=="Capital account"&
                    Category=="Balance")|
                   (Account=="Financial account"&
                      Category=="Foreign inv. in NZ; Financial derivative liabilities")|
                   (Category=="Secondary income balance")),]
sample_ts <- nzdata[Account == "Current account" & Category=="Services; Exports total",
                  .(TimePeriod, Value)]
kable(head(sample_ts)) # visualização da base

# Para avaliar se a série é multiplicativa ou aditiva é preciso separar os componentes

# Extraindo a tendência, pode ser feito calculando a média móvel ou mediana.
# Uma mediana móvel é menos sensível a outliers do que uma média móvel
sample_ts[, trend := rollmean(Value, 8, fill=NA, align = "right")]
kable(tail(sample_ts))

# Redução da Tendência
# A redução precisa ser aditiva ou multiplcativa, como ainda não sabemos 
# faremos os dois processos.
sample_ts[,`:=`( detrended_a = Value - trend,  detrended_m = Value / trend )]
kable(tail(sample_ts))

# Sazonalidade
# É preciso avaliar os valores típicos de tendência ao longo do ciclo. 
# A título de aprendizado vamos calcular o valor médio das observações 
# em Q1, Q2, Q3 e Q4.
sample_ts[,`:=`(seasonal_a = mean(detrended_a, na.rm = TRUE),
                seasonal_m = mean(detrended_m, na.rm = TRUE)), 
          by=.(quarter(TimePeriod)) ]
kable(tail(sample_ts))

# Com a tendência e a sazonalidade é possível calcular os resíduos.
sample_ts[,`:=`( residual_a = detrended_a - seasonal_a, 
                 residual_m = detrended_m / seasonal_m )]
kable(tail(sample_ts))

# Avaliando a decomposição

# Modelo Aditivo
ggsdc(sample_ts, aes(x = TimePeriod, y = Value), method = "decompose", 
      frequency = 4, s.window = 8, type = "additive")+ geom_line()+
  ggtitle("Additive")+ theme_minimal()

# Modelo Multiplicativo
ggsdc(sample_ts, aes(x=TimePeriod, y=Value), method = "decompose", 
      frequency=4, s.window=8, type = "multiplicative")+ geom_line()+
  ggtitle("Multiplicative")+ theme_minimal()

# Após a decomposição é preciso comparar os resíduos. Vamos avaliar a quantidade
# de correlação nos resíduos.

ssacf <- function(x) sum(acf(x, na.action = na.omit)$acf^2)
compare_ssacf <-function(add,mult) ifelse(ssacf(add)< ssacf(mult), 
                                         "Additive", "Multiplicative") 
kable(sample_ts[,.(compare_ssacf(residual_a, residual_m ))])