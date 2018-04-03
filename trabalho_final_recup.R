# Lib utilizada para filtrar as datas no data.frame
# install.packages("dplyr")
library(dplyr)
# Lib para geração de gráficos
library(ggplot2)

# Carregando os dados, utilizando mesma configuração da tarefa 4
names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
cepagri <- read.csv("cepagri.csv", 
                    header = FALSE, sep = ";", col.names = names)

# Carregando os dados do arquivo de consulta em data frame
query <- read.csv("query.csv", 
                    header = TRUE, sep = ";")

# Convertando a Horario para POSIXlt
class(cepagri$Horario)
cepagri$Horario <- strptime(cepagri$Horario, '%d/%m/%Y-%H:%M')

# Filtrando os dados de 2015 a 2017, pela documentação do R o ano é referente a 1900, 
# então o filtro será de 115 a 117
filtro_analises <- unclass(cepagri$Horario$year) >= 115 & unclass(cepagri$Horario$year) <= 117
cepagri_filtrado <- data.frame(Horario = cepagri$Horario[filtro_analises],
                               Temperatura = cepagri$Temperatura[filtro_analises],
                               Vento = cepagri$Vento[filtro_analises],
                               Umidade = cepagri$Umidade[filtro_analises], 
                               Sensacao = cepagri$Sensacao[filtro_analises])

# Conferindo os dados
head(cepagri_filtrado)
tail(cepagri_filtrado)
# Elminando NA
cepagri_filtrado <- cepagri_filtrado[! is.na(cepagri_filtrado[, 5]), ]
sapply(cepagri_filtrado, class)
# Convertendo temperatura para numerico
cepagri_filtrado$Temperatura <- as.character(cepagri_filtrado$Temperatura)
cepagri_filtrado$Temperatura <- as.numeric(cepagri_filtrado$Temperatura)


# Eliminando dados consecutivos de um dia
consecutive <- function(vector, k = 1) {
  n <- length(vector)
  result <- logical(n)
  for (i in (1+k):n)
    if (all(vector[(i-k):(i-1)] == vector[i]))
      result[i] <- TRUE
  for (i in 1:(n-k))
    if (all(vector[(i+1):(i+k)] == vector[i]))
      result[i] <- TRUE
  return(result)
}

filtro_cons <- !consecutive(cepagri_filtrado$Temperatura, 144)
# Aplicando o filtro 
cepagri_work <- data.frame(Horario = cepagri_filtrado$Horario[filtro_cons],
                           Temperatura = cepagri_filtrado$Temperatura[filtro_cons],
                           Vento = cepagri_filtrado$Vento[filtro_cons],
                           Umidade = cepagri_filtrado$Umidade[filtro_cons], 
                           Sensacao = cepagri_filtrado$Sensacao[filtro_cons])


# O data.frame utilizado para analise será o cepagri_work
# Analisando a precisão do e do equipamento 
total_days_in_frame <- length(unique(format(cepagri_work$Horario, '%d/%m/%Y')))
total_days_period <- as.Date("01/01/2018","%d/%m/%Y") - as.Date("01/01/2015","%d/%m/%Y")
# Tota de dias coletados
(total_days_in_frame/as.numeric(total_days_period)) * 100

days_in_period <- seq(as.Date("01/01/2015","%d/%m/%Y"), as.Date("31/12/2017","%d/%m/%Y"), by = "days")
days_in_period <- format(days_in_period, "%d/%m/%Y")
days_in_frame <- unique(unique(format(cepagri_work$Horario, "%d/%m/%Y")))
days_in_period[!is.element(days_in_period, days_in_frame)]

# Criando um novo data.frame com as informações consolidadas por dia:
day_vector <- c()
max_temp_day <- c()
min_temp_day <- c()
med_temp_day <-c()
dist_temp_day <- c()
sd_temp_day <- c()

max_umidade_day <- c()
min_umidade_day <- c()
med_umidade_day <- c()
dist_umidade_day <- c()
sd_umidade_day <- c()

max_vento_day <- c()
min_vento_day <- c()
med_vento_day <- c()
dist_vento_day <- c()
sd_vento_day <- c()

max_sensacao_day <- c()
min_sensacao_day <- c()
med_sensacao_day <- c()
dist_sensacao_day <- c()
sd_sensacao_day <- c()

pos <- 1
unique_day <- unique(format(cepagri_work$Horario, "%Y%m%d"))

for (i in unique_day){
  # Criando um novo data.frame para o ano
  filtro <- format(cepagri_work$Horario, "%Y%m%d") == i
  day_vector[pos] <- i
  
  temp_vector <- cepagri_work$Temperatura[filtro]
  x <- as.numeric(summary(temp_vector))
  max_temp_day[pos] <- x[6]
  min_temp_day[pos] <- x[1]
  med_temp_day[pos] <- x[4]
  dist_temp_day[pos] <- x[5] - x[2]
  sd_temp_day[pos] <- sd(temp_vector)
  
  umidade_vector <- cepagri_work$Umidade[filtro]
  x <- as.numeric(summary(umidade_vector))
  max_umidade_day[pos] <- x[6]
  min_umidade_day[pos] <- x[1]
  med_umidade_day[pos] <- x[4]
  dist_umidade_day[pos] <- x[5] - x[2]
  sd_umidade_day[pos] <- sd(umidade_vector)
  
  vento_vector <- cepagri_work$Vento[filtro]
  x <- as.numeric(summary(vento_vector))
  max_vento_day[pos] <- x[6]
  min_vento_day[pos] <- x[1]
  med_vento_day[pos] <- x[4]
  dist_vento_day[pos] <- x[5] - x[2]
  sd_vento_day[pos] <- sd(vento_vector)
  
  sensacao_vector <- cepagri_work$Sensacao[filtro]
  x <- as.numeric(summary(sensacao_vector))
  max_sensacao_day[pos] <- x[6]
  min_sensacao_day[pos] <- x[1]
  med_sensacao_day[pos] <- x[4]
  dist_sensacao_day[pos] <- x[5] - x[2]
  sd_sensacao_day[pos] <- sd(sensacao_vector)
  
  pos <- pos + 1
}

cepagri_day_summary <- data.frame(Dia=strptime(day_vector, "%Y%m%d"),
                                  MaxTemp=max_temp_day, MinTemp=min_temp_day,
                                  MedTemp=med_temp_day, DisTemp=dist_temp_day,
                                  DesTemp=sd_temp_day, MaxUmidade=max_umidade_day,
                                  MinUmidade=min_umidade_day, MedUmidade=med_umidade_day,
                                  DisUmidade=dist_umidade_day, MaxVento=max_vento_day,
                                  MinVento=min_vento_day, MedVento=med_vento_day,
                                  DesVento=sd_vento_day, DisVento=dist_vento_day,
                                  MaxSensacao=max_sensacao_day, MinSensacao=min_sensacao_day,
                                  MedSensacao=med_sensacao_day, DisSensacao=dist_sensacao_day,
                                  DesSensacao=sd_sensacao_day)



# Implemtenção de consulta usando a distance L2 e Similiridade de cosseno
df_dia_consulta <- filter(cepagri_day_summary, format(Dia, "%Y%m%d") == "20150130" )
vetor_caracterisca <- c(df_dia_consulta$MaxTem, df_dia_consulta$MinTemp, df_dia_consulta$MedTemp,
                        df_dia_consulta$MaxSensacao, df_dia_consulta$MinSensacao, df_dia_consulta$MedSensacao,
                        df_dia_consulta$MaxUmidade, df_dia_consulta$MinUmidade, df_dia_consulta$MedUmidade)

dia <- c()
cos_sim <- c()
dist_l2 <- c()
for(i in 1:nrow(cepagri_day_summary)){
  linha <- cepagri_day_summary[i,]
  vc <- c(linha$MaxTem, linha$MinTemp, linha$MedTemp, linha$MaxSensacao, 
          linha$MinSensacao, linha$MedSensacao, linha$MaxUmidade, linha$MinUmidade,
          linha$MedUmidade)
  cos_sim[i] <- cosine(vetor_caracterisca, vc)[1]
  dist_l2[i] <- dist(rbind(vetor_caracterisca, vc))
  dia[i] <- as.Date(linha$Dia)
}

as.Date(dia[order(cos_sim, decreasing = T)], origin = "1970-01-01") > df_dia_consulta$Dia + 7
as.Date(dia[order(dist_l2)], origin = "1970-01-01")[1:10]

dia[order(dist_l2)]
datas_ret_consulta <- as.Date(dia[order(dist_l2)], origin = "1970-01-01")
posicao_relevantes <- which(datas_ret_consulta < as.Date(df_dia_consulta$Dia) +7 &
                           datas_ret_consulta < as.Date(df_dia_consulta$Dia) - 7)
posicao_relevantes <- posicao_relevantes[posicao_relevantes < 101]

count_loop <- 1
precisao <- c()
revocacao <- c()

for (i in posicao_relevantes){
  precisao[count_loop] <- count_loop/i
  revocacao[count_loop] <- count_loop/length(posicao_relevantes)
  count_loop <- count_loop + 1
}

1/11
2/37
3/42
precisao
revocacao
library(ggplot2)
ggplot(data.frame(precisao, revocacao), aes(x=revocacao, y=precisao)) + geom_point() + geom_line()
  
  
filter(cepagri_day_summary, format(Dia, "%Y%m%d") == "20150130")
filter(cepagri_day_summary, format(Dia, "%Y%m%d") == "20160215")



df_dia_consulta$Dia + 7