# Instalação e Carregamento de Todos os Pacotes --------------------------------

pacotes <- c("forecast","tidyverse","fpp2","TSA","tseries")


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
# LEMBRETES ####################################################################
################################################################################

# Padrão sazonal existe quando uma série é influenciada por fatores sazonais 
# (ex.: trimestre do ano) e representa um período fixo e conhecido. 
# Consequentemente, as séries temporais sazonais às vezes são chamadas de séries
# temporais periódicas.

# Padrão cíclico existe quando os dados exibem subidas e descidas que não são 
# de período fixo. A duração dessas flutuações é geralmente de pelo menos 2 anos.

# Se as flutuações não são de período fixo, são cíclicas; se o período for 
# imutável e associado a algum aspecto do calendário, o padrão é sazonal. 
# Em geral, a duração média dos ciclos é maior do que a duração de um padrão 
# sazonal.

################################################################################
# Exemplo - Ciclo e Sazonalidade ###############################################
################################################################################

# base: lynx: número de linces capturadas entre 1821 e 1934 no Canadá
# base: hsales: vendas mensais de novas casas nos EUA entre 1973-1995
# base: taylor: demanda de eletricidade por meia hora entre 06.2000 e 08.2000

# Ciclos populacionais aperiódicos de, aproximadamente, 10 anos
# Não são fixos e duram entre 8 ou 9 anos e outros 10 anos
autoplot(lynx) + xlab("Year") + ylab("Number of lynx trapped")

# Forte sazonalidade dentro de cada ano, e comportamento cíclico
# com um período entre 6 e 10 anos
autoplot(hsales) + xlab("Year") + ylab("Monthly housing sales (millions)")

#Sazonalidade, padrão diário e padrão semanal
autoplot(taylor) + xlab("Week") + ylab("Electricity demand (GW)")

################################################################################
# Exemplo - Estacionariedade ###################################################
################################################################################

# Estacionariedade: propriedades estatísticas não mudam ao longo do tempo
# Exemplo: média, variância, autocorrelação

# Ruido: tem uma média constante, uma variância constante e não há estrutura
# de autocorrelação

# Autocorrelação (ADF) é um cálculo da correlação das observações da série temporal 
# com valores da mesma série, mas em tempos anteriores. Os períodos anteriores 
# são chamados de lags.

# Autocorrelação parcial (PACF) resume a relação entre uma observação em uma série 
# de tempo com observações em etapas de tempo anteriores, mas com as relações de
# observações intermediárias removidas.

# Correlograma: no contexto de séries temporais é uma diagrama de autocorrelações
# da amostra

# Teste de Dickey-Fuller: avaliação estacionariedade da série
# Expectativa que o teste seja significativo para rejeitar hipótese 

# Teste Ljung–Box: determina se algum grupo de autocorrelações de uma série 
# temporal é diferente de zero. Em outras palavras, avaliar se as séries de 
# observações ao longo do tempo é aleatória e independente.

# Expectativa que o p valor não seja significativo para não rejeitarmos a 
# hipótese nula e concluir que os resíduos são conjuntamente não correlacionados.

# Avaliando o ruído
set.seed(14)
model.noise <- ts(rnorm(1000))
plot(model.noise)

# Usar o teste ADF para avaliar se a série é estacionária
# Quanto mais negativo for o valor do Teste de Dickey-Fuller menor o p valor
adf.test(model.noise)

# Sazonalidade e tendência # 01
data(beersales) # base disponível no pacote TSA
plot(beersales) # plotando a série
adf.test(beersales) # avaliando a correlação e aparenta ser estacionária
adf.test(beersales, k=12) # série não é estacionária

# Sazonalidade e tendência # 02
data(airpass) # base disponível no pacote TSA
plot(airpass) # plotando a série

# Avalie com uma lag longo para avaliar a sazonalidade
adf.test(airpass, k=12)