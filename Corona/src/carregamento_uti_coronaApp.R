library(openxlsx)
library(readxl)
library(WriteXLS)
library(ggthemes)
library(RColorBrewer)
library(lubridate)
library(caret)
library(tidyverse)
library(here)
library(usethis)
library(gargle)
library(googlesheets4)
library(ggrepel)

Sys.setenv(TZ="Brazil/East")
options(tz="Brazil/East")
Sys.getenv("TZ")
options(scipen = 999999)

source("~/Dropbox/Coding/R/funs/msrfun.R")



# Carregamento do Banco e ajuste das variáveis ----------------------------

# Variáveis (coletadas 1 ou mais vezes ou dia) - DF uti
# **************************************************************
# dt: Data do preenchimento
# dt_time: Date time do preenchimento
# email
# local: "Nome_do_Hospital - UTI ADULTO/PED/Especializada - Tipo I/II/III"
# leitos: Quantidade de leitos operacionais no instante do preenchimento
# bloqueados: Quantidade de leitos bloqueados no instante do preenchimento
# pacientes: Total de pacientes internados na UTI no instante do preenchimento
# covid_susp: Total de pacientes suspeitos de COVID na UTI no instante do preenchimento
# covid_positivo: Total de pacientes POSITIVOS PARA COVID na UTI no instante do preenchimento
# pac_nao_covid: Total de paciente NÃO COVID (nem suspeito, nem positivo) na UTI no instante do preenchimento
# senha: senha de validação
# hospital: Nome do Hospital
# adulto_ped: UTI adulto ou pediátrica ou especializada (exemplo: queimados)
# tipo_uti: Tipo da UTI (I, II ou III)
# especializada: booleana - distingue uti geral (adulto ou ped) de especializada
# adulto: booleana - distingue de pacientes adultos x pediátricos
# uti_adulto: boolean - distingue se paciente está em UTI GERAL ADULTO





# Importa dados do Dashboard das UTIS
# Um instante de cada UTI por linha
uti <- read_sheet("https://docs.google.com/spreadsheets/d/1Wy8eskZoiI23Qui4Iamr_pOcLEoDaIVICE-Sgqsoi4s/edit#gid=830997102", sheet = 4)

# Salva o XLS para ser usado pelo CoronaApp (fora desse flexdashboard)
write.xlsx(x = uti,file = "~/Dropbox/Coding/R/SMS/CoronaApp/Corona/uti.xlsx")

# Quebra o local informante em 3 colunas (hospital, adulto_ped, tipo_uti)
uti <- bind_cols(uti, as.data.frame(str_split(uti$`Local Informante`, pattern  = " - ", simplify = TRUE)))

# Troca os nomes das variáveis
names(uti) <- c("dt", "email", "local", "leitos", "bloqueados", "pacientes", 
                "covid_susp", "covid_positivo", "senha", "hospital", "adulto_ped", "tipo_uti")

# Cria outras variáveis:
uti <- uti %>% 
  mutate(dt_time = dt, 
         dt = as.Date(dt),
         pac_nao_covid = pacientes - covid_susp - covid_positivo,
         especializada = if_else(adulto_ped %in% c("UTI ADULTO", "UTI PEDIATRICA"), FALSE, TRUE),
         adulto = if_else(adulto_ped == "UTI PEDIATRICA", FALSE, TRUE),
         uti_adulto = if_else(adulto_ped == "UTI ADULTO", TRUE, FALSE)
         )


# Criação dos DF de UTI auxiliares ----------------------------------------

# uti_diaria: Banco da UTI filtrado para a última observação diária (mais recente) - Reduz o número de linhas
uti_diaria <- uti %>%                 
  group_by(dt,local) %>% 
  filter(dt_time == max(dt_time)) %>% 
  ungroup()


# uti_agregado_diario - DF como o somatório das variáveis mais recentes de cada Hospital, por dia
uti_agregado_diario <- uti_diaria %>% 
  group_by(dt, adulto_ped) %>% 
  summarise(leitos = sum(leitos, na.rm = TRUE),
            bloqueados = sum(bloqueados, na.rm = TRUE),
            pacientes = sum(pacientes, na.rm = TRUE),
            covid_susp = sum(covid_susp, na.rm = TRUE),
            covid_positivo = sum(covid_positivo, na.rm = TRUE),
            pac_nao_covid = sum(pac_nao_covid, na.rm = TRUE),
            utis_presentes = n()
            )

# DFs com o somatório das variáveis de cada hospital separado em adulto e pediátrico
uti_agregado_diario_adulto <- uti_agregado_diario %>% filter(adulto_ped == "UTI ADULTO")
uti_agregado_diario_ped <- uti_agregado_diario %>% filter(adulto_ped == "UTI PEDIATRICA")




# DF uti_diaria_imputada - banco anterior organizado por dt e local e imputados os valores NA (direction = "down")
uti_diaria_imputada <- uti_diaria %>% 
  filter(adulto_ped == "UTI ADULTO") %>% 
  complete(dt = seq.Date(min(dt), max(dt), by="day"), local) %>% 
  group_by(local) %>% 
  fill(leitos:especializada, .direction = "down") %>% 
  ungroup()


uti_diaria_imputada %>% 
  group_by(dt, local) %>% 
  summarise(leitos = sum(leitos, na.rm = TRUE)) %>% tail(20)

uti_diaria_imputada_adulto_ped <- uti_diaria %>% 
  complete(dt = seq.Date(min(dt), max(dt), by="day"), local) %>% 
  group_by(local) %>% 
  fill(leitos:especializada, .direction = "down") %>% 
  ungroup()

uti_agregado_diario_imputada <- uti_diaria_imputada_adulto_ped %>% 
  group_by(dt, adulto_ped) %>% 
  summarise(leitos = sum(leitos, na.rm = TRUE),
            bloqueados = sum(bloqueados, na.rm = TRUE),
            pacientes = sum(pacientes, na.rm = TRUE),
            covid_susp = sum(covid_susp, na.rm = TRUE),
            covid_positivo = sum(covid_positivo, na.rm = TRUE),
            pac_nao_covid = sum(pac_nao_covid, na.rm = TRUE),
            utis_presentes = n()
  )




# UTI diária com imputações pra baixo, somente UTI GERAL ADULTO
uti_agregado_diario_imputada_adulto <- uti_diaria_imputada_adulto_ped %>% 
  filter(grepl("ADULTO", adulto_ped)) %>% 
  group_by(dt) %>% 
  summarise(leitos = sum(leitos, na.rm = TRUE),
            bloqueados = sum(bloqueados, na.rm = TRUE),
            pacientes = sum(pacientes, na.rm = TRUE),
            covid_susp = sum(covid_susp, na.rm = TRUE),
            covid_positivo = sum(covid_positivo, na.rm = TRUE),
            pac_nao_covid = sum(pac_nao_covid, na.rm = TRUE),
            utis_presentes = n()
  )

uti_agregado_diario_imputada_adulto <- uti_agregado_diario_imputada_adulto %>% 
  mutate(covid_pos_susp = covid_positivo + covid_susp)


# Definições de constantes ------------------------------------------------

cor = "#C77CFF" # roxo

vermelho = "#f35e5a" # vermelho
verde = "#17b12b" # verde
roxo = "#C77CFF" # roxo


# Funções -----------------------------------------------------------------


geomSeries <- function(base, max) {
  base^(0:floor(log(max, base)))
}


# Gera um gráfico com as progressões de X dias pra trás do dia de hoje. 
# Usa o DF uti_agregado_com_escalas_imputada
simula_progressao <- function(dias) {
  uti_agregado_com_escalas_imputada %>% 
    filter(adulto_ped == "UTI ADULTO") %>% 
    select(dt, covid_positivo, escala3, escala7, escala10) %>% 
    mutate(covid_7dias = ifelse(dt >= today() - dias, covid_positivo, NA)) %>% 
    pivot_longer(cols = -dt, names_to = "leitos", values_to = "n") %>% 
    ggplot(aes(x = dt, y = n, color = leitos, label = n)) + 
    geom_point() +  geom_smooth(method = "lm", se = FALSE, fullrange=TRUE) +
    scale_y_continuous(trans='log2', n.breaks = 8, limits = c(NA,700)) +
    geom_text(nudge_y = 0.5, show.legend = FALSE) +
    annotate(geom="text", label="174 Leitos de UTI extras para Corona", x=as.Date("2020-03-20"), y=174, vjust=-0.5, colour = "blue", hjust = 0) +
    annotate(geom="text", label="255 Plano em fase avançada", x=as.Date("2020-03-20"), y=255, vjust=-0.5, colour = "orange", hjust = 0) +
    annotate(geom="text", label="383 Leitos máximos de UTI com equipamento adicional", x=as.Date("2020-03-20"), y=383, vjust=-0.5, colour = "red", hjust = 0)+
    ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
    xlab("Tempo decorrente") + 
    ylab("Quantidade de pacientes (log2)") +
    scale_color_discrete(name = "Pacientes: ",
                         labels = c("Ultimos", dias, " dias", "Desde o princípio", "Duplicação a cada 10 dias", "Duplicação a cada 3 dias", "Duplicação a cda 7 dias")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_x_date(date_breaks = "1 week",date_labels = "%d-%m",
                 limits = as.Date(c("2020-03-19", "2020-06-01"))) + 
    labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
         caption = "Fonte: Dashboard das UTIs") +
    theme(legend.position = "bottom") +
    geom_hline(yintercept=174, linetype="dashed",  color = "blue", size=0.5) +
    geom_hline(yintercept=255, linetype="dashed",  color = "orange", size=0.5) +
    geom_hline(yintercept=383, linetype="dashed",  color = "red", size=0.5)
}


# Função prefeito_func
prefeito_func <- function(df, cor, dt_inicial, dt_final, data_final_grafico = (today() + 7), fullrange = TRUE) {
  prefeito_positivos <- df %>% 
    filter(leitos == "covid_positivo") 
  dt_inicial <- date(dt_inicial)
  dt_final <- date(dt_final)
  df %>% 
    filter(dt >= dt_inicial & dt <= dt_final) %>% 
    filter(leitos == "covid_positivo") %>% 
    ggplot(aes(x = dt, y = n, color = leitos, label = n)) + 
    geom_point(data = prefeito_positivos, aes(x=dt, y = n)) +
    geom_point(colour = cor) +  geom_smooth(method = "lm", colour = cor, se = FALSE, alpha = 0.5, fullrange = fullrange) +
    scale_y_continuous(trans='log2', n.breaks = 8, limits = c(4,1024)) +
    geom_text(nudge_y = 0.5, show.legend = FALSE, colour =  cor) + theme_light() +
    annotate(geom="text", label="174 Leitos de UTI extras para Corona", x=as.Date("2020-03-20"), y=174, vjust=-0.5, colour = "blue", hjust = 0) +
    annotate(geom="text", label="255 Plano em fase avançada", x=as.Date("2020-03-20"), y=255, vjust=-0.5, colour = "orange", hjust = 0) +
    annotate(geom="text", label="383 Leitos máximos de UTI com equipamento adicional", x=as.Date("2020-03-20"), y=383, vjust=-0.5, colour = "red", hjust = 0)+
    ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
    xlab("Tempo decorrente") + 
    ylab("Quantidade de pacientes (log2)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_x_date(date_breaks = "1 week",date_labels = "%d-%m",
                 limits = as.Date(c("2020-03-19", as.Date(data_final_grafico)))) + 
    labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
         caption = "Fonte: Dashboard das UTIs") +
    theme(legend.position = "none") +
    geom_hline(yintercept=174, linetype="dashed",  color = "blue", size=0.5) +
    geom_hline(yintercept=255, linetype="dashed",  color = "orange", size=0.5) +
    geom_hline(yintercept=383, linetype="dashed",  color = "red", size=0.5) + 
    #geom_vline(xintercept = today()) + theme(legend.position = "none") +
    geom_segment(aes(x = today(), y = 0, xend = today(), yend = 174))
  
}

# Calcula a taxa de crescimento e o tempo de duplicação
double_time <- function(data_inicial, valor_inicial,  data_final, valor_final) {
  tempo <- as.numeric(date(data_final) - date(data_inicial))
  taxa_crescimento <- log(valor_final/valor_inicial) / tempo
  tempo_duplicacao <- log(2)/taxa_crescimento
  cat("\n\nTempo: ", tempo, "\n")
  cat("\nTaxa de Crescimento: ", taxa_crescimento, "\n")
  cat("\nTempo de Duplicação: ", tempo_duplicacao, "\n")
  c(taxa_crescimento = taxa_crescimento, tempo_duplicacao = tempo_duplicacao)
}


data_caos <- function(data_ref = today(), n_inicial, tp_duplicacao, limites = c(174, 255, 383)) {
  retorno <- NULL
  for (i in 1:length(limites)) {
    delta <-  limites[i] - n_inicial  
    dias_limite <- round(delta / n_inicial * tp_duplicacao)
    data_limite <- data_ref + dias_limite
    retorno[i] <- as.Date(data_limite)
  }
  as.Date(retorno, origin = "1970-01-01")
}


retorna_caos <- function(data = positivos_data, 
                         dt_final = today(),           # essa vai ser usada como data_final na função double_time 
                         dias_para_tras = 7,           # esse vai determinar a data_inicial na função double_time
                         data_ref = today(),           # essa vai ser usada como data_inicial na função data_caos
                         limites = c(174, 255, 383))  {
  positivos_vetor <- positivos_data %>% pull(covid_positivo)
  dt_inicial <- today() - dias_para_tras
  n_inicial <- positivos_data %>% filter(dt == dt_inicial) %>% pull(covid_positivo) # Quantidade de casos no primeiro dia - para a função double_time
  n_final <-  positivos_data %>% filter(dt == dt_final) %>% pull(covid_positivo)    # Quantidade de casos no último dia - para a função double time
  n_inicial_data_caos <- positivos_data %>% filter(dt == data_ref) %>% pull(covid_positivo) # Quantidade de casos no dia de referencia para inicio da contagem par ao futuro
  tp_duplicacao <- double_time(data_inicial = dt_inicial, valor_inicial = n_inicial, data_final = dt_final, valor_final = n_final )
  cat("\n\nVetor com os positivos por dia: \n",positivos_vetor, "\n\npositivos_data últimos 7 dias\n")
  print(tail(positivos_data,7))
  cat("\nLimites de leitos ", limites, "\n")
  cat("Data inicial: ", format(dt_inicial, "%d/%m/%Y"))
  cat("\nPacientes na UTI na data inicial: ", n_inicial)
  cat("\nData final: ", format(dt_final, "%d/%m/%Y"))
  cat("\nPacientes na UTI na data final: ", n_final)
  cat("\n\nTempo de duplicação", round(tp_duplicacao[2],2), " dias\n")
  
  
  retorno <-  list(positivos = positivos_vetor, limites = limites, data_inicial = dt_inicial, data_final = dt_final, data_ref = data_ref, tp_duplicacao = tp_duplicacao)
  if (tp_duplicacao[2] >  0) {
    data_estouro <- data_caos(data_ref = data_ref, n_inicial = n_inicial_data_caos, tp_duplicacao = tp_duplicacao[2],limites = limites)
    for (i in 1: length(limites)) {
      cat("\nData para atingir ", limites[i], " pacientes de UTI COVID: ", format(data_estouro[i], "%d/%m/%Y"))
    }
    cat("\n\n")
    retorno$data_estouro = data_estouro
    return(retorno) 
  } else if (tp_duplicacao[2] == 0) {
    cat("Quantidade de pacientes não mudou. Taxa se mantém estável, não há mudança na tendência\n\n")
    retorno$data_estouro = 0
  } else {
    cat("Tempo de duplicação negativo, tendência de desocupação de leitos\n\n")
    retorno$data_estouro = -1
  }
  
}


tempo_duplicacao_dias_para_tras <- function(dias_para_tras = 5, data_final = today()) {
  data_inicial <- today() - dias_para_tras
  valor_inicial <- positivos_data %>% 
    filter(dt == data_inicial) %>% pull(covid_positivo)
  valor_final <- positivos_data %>% 
    filter(dt == data_final) %>% pull(covid_positivo)
  double_time(data_inicial = data_inicial,
              valor_inicial = valor_inicial,
              data_final = data_final,
              valor_final = valor_final)
}




# Calculo para Value Boxes ------------------------------------------------

ultima_lotacao_utis <- uti %>% 
  group_by(dt, local) %>% 
  filter(dt_time == max(dt_time)) %>% 
  select(dt, dt_time,local, leitos, bloqueados, pacientes, covid_susp, 
         covid_positivo, hospital, adulto_ped, tipo_uti, pac_nao_covid, especializada) %>% 
  ungroup() %>% 
  group_by(local) %>% 
  filter(dt_time == max(dt_time)) %>% 
  ungroup()

resumo_utis <- ultima_lotacao_utis %>% 
  group_by(adulto_ped) %>% 
  summarise(total_positivos_uti = sum(covid_positivo),
            total_suspeitos_uti = sum(covid_susp),
            total_leitos = sum(leitos),
            total_bloqueados = sum(bloqueados),
            total_nao_covid = sum(pac_nao_covid),
            n = n()
            )


# Gráfico de Linha com a Totalização das UTIs (todas, adulto, ped)  --------
totalizacao_uti <- uti_diaria_imputada_adulto_ped %>% 
  ungroup() %>% 
  group_by(dt, adulto_ped) %>%
  summarise(total_covid_positivo_uti = sum(covid_positivo)) %>% 
  ggplot(aes(x = dt, y = total_covid_positivo_uti, color = adulto_ped,label = total_covid_positivo_uti)) + geom_line() + 
  geom_point() + geom_text(nudge_y = 1.2, show.legend = FALSE) +
  ggtitle("Total de pacientes POSITIVOS EM UTI (adulto, pediátrica, especializada)") + 
  labs(subtitle = paste0("Data: ", format(Sys.Date(), "%d/%m/%Y")), 
       caption = "Fonte: Dashboard das UTIs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Dias - Série temporal") + ylab("Quantidade de pacientes") +
  scale_x_date(date_breaks = "1 day",date_labels = "%d-%m") +
  scale_color_discrete(name = "Tipo de UTI")

# Positivos em cada UTI --------------------------------------------------------
grafico_corona_positivo <- uti_diaria %>% 
  group_by(dt, local, covid_positivo) %>%
  filter(covid_positivo > 0) %>% 
  ggplot(aes(x = fct_reorder(format(date(dt), "%d/%m"), dt), y = covid_positivo, fill = local, label = covid_positivo)) + geom_col() +
  theme(legend.position = "bottom") + xlab("Data") + ylab("Quantidade de pacientes") +
  ggtitle("Pacientes POSITIVOS em UTI em cada Hospital") + 
  labs(subtitle = paste0("Data: ", format(Sys.Date(), "%d/%m/%Y")), 
       caption = "Fonte: Dashboard das UTIs") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(position = position_stack(vjust = .5)) +
  guides(fill=guide_legend(ncol=2))
corona_positivo <- grafico_corona_positivo

# Suspeitos na UTI --------------------------------------------------------
grafico_corona_suspeito <- uti_diaria %>% 
  group_by(dt = fct_reorder(format(date(dt), "%d/%m"), dt), local, covid_susp) %>%
  filter(covid_positivo > 0) %>% 
  ggplot(aes(x = dt, y = covid_susp, fill = local, label = covid_susp)) + geom_col() +
  theme(legend.position = "bottom") + xlab("Data") + ylab("Quantidade de pacientes") +
  ggtitle("Pacientes SUSPEITOS em UTI em cada Hospital") + 
  labs(subtitle = paste0("Data: ", format(Sys.Date(), "%d/%m/%Y")), 
       caption = "Fonte: Dashboard das UTIs") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(position = position_stack(vjust = .5)) +
  guides(fill=guide_legend(ncol=2))
corona_suspeito <-  grafico_corona_suspeito

# pacientes nas utis ------------------------------------------------------
grafico_uti_agregado_diario_adulto <- uti_agregado_diario_adulto %>% 
  mutate(eixo_x = paste0(format(dt, "%d-%m"), "\n", utis_presentes, " UTIS")) %>% 
  select(dt, eixo_x,leitos, utis_presentes, pacientes, 
         pac_nao_covid, covid_susp, covid_positivo) %>% 
  pivot_longer(cols = pac_nao_covid:covid_positivo) %>% 
  ggplot(aes(x = fct_reorder(eixo_x,dt), y = value, fill = name, label = value)) + 
  geom_col() +  geom_text(position = position_stack(vjust = .5)) +
  ggtitle("UTIs Adulto de Porto Alegre - Casos COVID, Suspeitos e Não-Covid") +
  xlab("Dias e quantidade de utis que informaram sua situação") + 
  ylab("Quantidade de pacientes") +
  scale_fill_discrete(name = "Pacientes: ",
                      labels = c("Positivo COVID", "Suspeito COVID", "Não Suspeito")) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  labs(subtitle = paste0("Data: ", format(Sys.Date(), "%d/%m/%Y")), 
       caption = "Fonte: Dashboard das UTIs") 



# Simulação de progressão -------------------------------------------------

# Criação do DF para a lihas de duplicação em 3, 5, 7 e 10 dias.
escala_3 <- data.frame( escala3 = geomSeries(base=2, max=10000),
                        dt = seq.Date(from = date("2020-03-19"), to = date("2020-03-19") + (length(geomSeries(base=2, max=10000))*3) - 1, by = 3))

escala_5 <- data.frame( escala5 = geomSeries(base=2, max=10000),
                        dt = seq.Date(from = date("2020-03-19"), to = date("2020-03-19") + (length(geomSeries(base=2, max=10000))*5) - 1, by = 5))

escala_7 <- data.frame( escala7 = geomSeries(base=2, max=10000),
                        dt = seq.Date(from = date("2020-03-19"), to = date("2020-03-19") + (length(geomSeries(base=2, max=10000))*7) - 1, by = 7))

escala_10 <- data.frame( escala10 = geomSeries(base=2, max=10000),
                        dt = seq.Date(from = date("2020-03-19"), to = date("2020-03-19") + (length(geomSeries(base=2, max=10000))*10) - 1, by = 10))



# Adiciona X linhas no banco e cria novos dados
quantidade_de_linhas <- 1000
adiciona_linhas = data.frame(dt = seq.Date(from = date("2020-03-19"), to = date("2020-03-19") + quantidade_de_linhas, by = 1 ))




uti_agregado_com_escalas <- full_join(uti_agregado_diario_adulto, adiciona_linhas, by = "dt")
uti_agregado_com_escalas_imputada <- full_join(uti_agregado_diario_imputada, adiciona_linhas, by = "dt")


uti_agregado_com_escalas <- full_join(uti_agregado_com_escalas, escala_3, by = "dt")
uti_agregado_com_escalas <- full_join(uti_agregado_com_escalas, escala_5, by = "dt")
uti_agregado_com_escalas <- full_join(uti_agregado_com_escalas, escala_7, by = "dt")
uti_agregado_com_escalas <- full_join(uti_agregado_com_escalas, escala_10, by = "dt")

uti_agregado_com_escalas_imputada <- full_join(uti_agregado_com_escalas_imputada, escala_3, by = "dt")
uti_agregado_com_escalas_imputada <- full_join(uti_agregado_com_escalas_imputada, escala_5, by = "dt")
uti_agregado_com_escalas_imputada <- full_join(uti_agregado_com_escalas_imputada, escala_7, by = "dt")
uti_agregado_com_escalas_imputada <- full_join(uti_agregado_com_escalas_imputada, escala_10, by = "dt")

prefeito_dados <- uti_agregado_com_escalas_imputada %>% 
  #filter(dt >= today() - 7) %>% 
  filter(adulto_ped == "UTI ADULTO") %>% 
  select(dt, covid_positivo) %>% 
  # mutate(covid_7dias = ifelse(dt >= today() - 8 & !(dt == today()), covid_positivo, NA)) %>% 
  mutate(covid_15dias = ifelse(dt >= today() - 14, covid_positivo, NA),
         covid_inicial = ifelse(dt <= min(dt) + 14, covid_positivo, NA)) %>% 
  mutate(covid_inicial = ifelse(covid_inicial >= 16, NA, covid_inicial)) %>% 
  pivot_longer(cols = -dt, names_to = "leitos", values_to = "n") 

prefeito_positivos <- prefeito_dados %>% 
  filter(leitos == "covid_positivo") %>% 
  #filter(dt >= date("2020-03-16") & dt <= date("2020-03-29"))
  #filter(dt >= date("2020-03-16") & dt <= date("2020-04-08"))
  filter(dt >= date("2020-03-16") & dt <= date("2020-04-17"))


positivos_vetor <- uti_agregado_diario_imputada_adulto %>% 
  pull(covid_positivo)

positivos_data <- uti_agregado_diario_imputada_adulto %>% 
  select(dt, covid_positivo)


uti_agregado_com_escalas_imputada_adulto <- uti_agregado_diario_imputada %>% 
  filter(adulto_ped == "UTI ADULTO" | is.na(adulto_ped)) %>% 
  group_by(dt) %>% 
  summarise(leitos = sum(leitos, na.rm = TRUE), 
            bloqueados = sum(bloqueados, na.rm = TRUE), 
            pacientes = sum(pacientes, na.rm = TRUE), 
            covid_susp = sum(covid_susp, na.rm = TRUE),
            covid_positivo = sum(covid_positivo, na.rm = TRUE),
            covid = covid_positivo + covid_susp
  )


simulacao_progressao <- uti_agregado_com_escalas %>% 
  select(dt, covid_positivo, escala3, escala7, escala10) %>% 
  pivot_longer(cols = -dt, names_to = "leitos", values_to = "n") %>% 
  ggplot(aes(x = dt, y = n, color = leitos, label = n)) + 
  geom_point() +  geom_smooth(method = "lm", se = FALSE, fullrange=TRUE) +
  scale_y_continuous(trans='log2', n.breaks = 8, limits = c(NA,700)) +
  geom_text(nudge_y = 0.5) +
  annotate(geom="text", label="174 Leitos de UTI extras para Corona", x=as.Date("2020-03-20"), y=174, vjust=-1, colour = "red", hjust = 0)+
  ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
  xlab("Tempo decorrente") + 
  ylab("Quantidade de pacientes (log2)") +
  scale_color_discrete(name = "Pacientes: ",
                      labels = c("Pacientes na UTI", "Duplicaçõa a cada 10 dias", "Duplicação a cada 3 dias", "Duplicação a cada 7 dias")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 week",date_labels = "%d-%m",
               limits = as.Date(c("2020-03-19", "2020-04-30"))) + 
  labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
       caption = "Fonte: Dashboard das UTIs") +
  theme(legend.position = "bottom") +
  geom_hline(yintercept=174, linetype="dashed", 
             color = "red", size=0.5)


simulacao_progressao_7dias_imputada <- uti_agregado_com_escalas_imputada %>% 
  #filter(dt >= today() - 7) %>% 
  filter(adulto_ped == "UTI ADULTO") %>% 
  select(dt, covid_positivo, escala3, escala7, escala10) %>% 
  # mutate(covid_7dias = ifelse(dt >= today() - 8 & !(dt == today()), covid_positivo, NA)) %>% 
  mutate(covid_7dias = ifelse(dt >= today() - 7, covid_positivo, NA)) %>% 
  pivot_longer(cols = -dt, names_to = "leitos", values_to = "n") %>% 
  ggplot(aes(x = dt, y = n, color = leitos, label = n)) + 
  geom_point() +  geom_smooth(method = "lm", se = FALSE, fullrange=TRUE) +
  scale_y_continuous(trans='log2', n.breaks = 8, limits = c(NA,700)) +
  geom_text(nudge_y = 0.5, show.legend = FALSE) +
  annotate(geom="text", label="174 Leitos de UTI extras para Corona", x=as.Date("2020-03-20"), y=174, vjust=-0.5, colour = "blue", hjust = 0) +
  annotate(geom="text", label="255 Plano em fase avançada", x=as.Date("2020-03-20"), y=255, vjust=-0.5, colour = "orange", hjust = 0) +
  annotate(geom="text", label="383 Leitos máximos de UTI com equipamento adicional", x=as.Date("2020-03-20"), y=383, vjust=-0.5, colour = "red", hjust = 0)+
  ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
  xlab("Tempo decorrente") + 
  ylab("Quantidade de pacientes (log2)") +
  scale_color_discrete(name = "Pacientes: ",
                       labels = c("Ultimos 7 dias", "Desde o princípio", "Duplicação a cada 10 dias", "Duplicação a cada 3 dias", "Duplicação a cda 7 dias")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 week",date_labels = "%d-%m",
               limits = as.Date(c("2020-03-19", "2020-06-01"))) + 
  labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
       caption = "Fonte: Dashboard das UTIs") +
  theme(legend.position = "bottom") +
  geom_hline(yintercept=174, linetype="dashed",  color = "blue", size=0.5) +
  geom_hline(yintercept=255, linetype="dashed",  color = "orange", size=0.5) +
  geom_hline(yintercept=383, linetype="dashed",  color = "red", size=0.5) 
  



# Simulação para Prefeito -------------------------------------------------

prefeito_dados %>% 
  ggplot(aes(x = dt, y = n, color = leitos, label = n)) + 
  geom_point() +  geom_smooth(data = subset(prefeito_dados, leitos == "covid_15dias" | leitos == "covid_inicial"),method = "lm", se = FALSE, alpha = 0.5, fullrange = TRUE) +
  scale_y_continuous(trans='log2', n.breaks = 8, limits = c(NA,1024)) +
  geom_text(nudge_y = 0.5, show.legend = FALSE) + theme_light() +
  annotate(geom="text", label="174 Leitos de UTI extras para Corona", x=as.Date("2020-03-20"), y=174, vjust=-0.5, colour = "blue", hjust = 0) +
  annotate(geom="text", label="255 Plano em fase avançada", x=as.Date("2020-03-20"), y=255, vjust=-0.5, colour = "orange", hjust = 0) +
  annotate(geom="text", label="383 Leitos máximos de UTI com equipamento adicional", x=as.Date("2020-03-20"), y=383, vjust=-0.5, colour = "red", hjust = 0)+
  ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
  xlab("Tempo decorrente") + 
  ylab("Quantidade de pacientes (log2)") +
  scale_color_discrete(name = "Pacientes: ",
                       labels = c("Ultimos 15 dias", "Progressão sem isolamento domiciliar", "Inicio da ação do isolamento")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 week",date_labels = "%d-%m",
               limits = as.Date(c("2020-03-19", "2020-05-05"))) + 
  labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
       caption = "Fonte: Dashboard das UTIs") +
  theme(legend.position = "bottom") +
  geom_hline(yintercept=174, linetype="dashed",  color = "blue", size=0.5) +
  geom_hline(yintercept=255, linetype="dashed",  color = "orange", size=0.5) +
  geom_hline(yintercept=383, linetype="dashed",  color = "red", size=0.5) + 
  geom_vline(xintercept = today()) 



prefeito2 <- prefeito_dados %>% 
  filter(dt >= date("2020-03-16") & dt <= date("2020-03-29")) %>% 
  filter(leitos == "covid_positivo") %>% 
  ggplot(aes(x = dt, y = n, color = leitos, label = n)) + 
  geom_point() +  geom_smooth(method = "lm", se = FALSE, alpha = 0.5, fullrange = TRUE) +
  scale_y_continuous(trans='log2', n.breaks = 8, limits = c(NA,1024)) +
  geom_text(nudge_y = 0.5, show.legend = FALSE) + theme_light() +
  annotate(geom="text", label="174 Leitos de UTI extras para Corona", x=as.Date("2020-03-20"), y=174, vjust=-0.5, colour = "blue", hjust = 0) +
  annotate(geom="text", label="255 Plano em fase avançada", x=as.Date("2020-03-20"), y=255, vjust=-0.5, colour = "orange", hjust = 0) +
  annotate(geom="text", label="383 Leitos máximos de UTI com equipamento adicional", x=as.Date("2020-03-20"), y=383, vjust=-0.5, colour = "red", hjust = 0)+
  ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
  xlab("Tempo decorrente") + 
  ylab("Quantidade de pacientes (log2)") +
  scale_color_discrete(name = "Pacientes: ",
                       labels = c("Primeiros 15 dias", "Progressão sem isolamento domiciliar", "Inicio da ação do isolamento")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 week",date_labels = "%d-%m",
               limits = as.Date(c("2020-03-19", "2020-05-05"))) + 
  labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
       caption = "Fonte: Dashboard das UTIs") +
  theme(legend.position = "bottom") +
  geom_hline(yintercept=174, linetype="dashed",  color = "blue", size=0.5) +
  geom_hline(yintercept=255, linetype="dashed",  color = "orange", size=0.5) +
  geom_hline(yintercept=383, linetype="dashed",  color = "red", size=0.5) + 
  geom_vline(xintercept = today(), colour = cor) 



# [17/4 21:48] Natan Katz: Gráfico 1 - Bolinhas do dia 16 até dia 29/03, linha de tendencia do dia 16 até 29/03
# [17/4 21:50] Natan Katz: Gráfico 2 - Bolinhas do dia 16/03 até dia 08/04, linha de tendencia do dia 30/03 até dia 08/04
# [17/4 21:51] Natan Katz: Gráfico 3 - Bolinhas do dia 16/03 até dia 17/04, linha de tendencia do dia 08/04 até dia 17/04


prefeito_dados <- uti_agregado_com_escalas_imputada %>% 
  #filter(dt >= today() - 7) %>% 
  filter(adulto_ped == "UTI ADULTO") %>% 
  select(dt, covid_positivo) %>% 
  # mutate(covid_7dias = ifelse(dt >= today() - 8 & !(dt == today()), covid_positivo, NA)) %>% 
  mutate(covid_15dias = ifelse(dt >= today() - 14, covid_positivo, NA),
         covid_inicial = ifelse(dt <= min(dt) + 14, covid_positivo, NA)) %>% 
  mutate(covid_inicial = ifelse(covid_inicial >= 16, NA, covid_inicial)) %>% 
  pivot_longer(cols = -dt, names_to = "leitos", values_to = "n") 


prefeito_func(df = prefeito_dados, cor = vermelho, dt_inicial = "2020-03-16", dt_final = "2020-03-29")
prefeito_func(df = prefeito_dados, cor = verde, dt_inicial = "2020-03-30", dt_final = today()-15)
prefeito_func(df = prefeito_dados, cor = roxo, dt_inicial = today()-14, dt_final = today())



# Gráficos das simulações -------------------------------------------------


prefeito_inicial <- prefeito_func(df = prefeito_dados, cor = vermelho, dt_inicial = "2020-03-16", dt_final = "2020-03-29")
prefeito_intermediario <- prefeito_func(df = prefeito_dados, cor = verde, dt_inicial = "2020-03-30", dt_final = today()-15)
prefeito_final <- prefeito_func(df = prefeito_dados, cor = roxo, dt_inicial = today()-14, dt_final = today())
regressao_ultimos_7 <- prefeito_func(df = prefeito_dados, cor = roxo, dt_inicial = today()-7, dt_final = today())
today() - 15


prefeito2 <- prefeito_dados %>% 
  filter(dt >= date("2020-03-16") & dt <= date("2020-03-29")) %>% 
  #filter(dt >= date("2020-03-30") & dt <= date("2020-04-08")) %>% 
  #filter(dt >= date("2020-04-08") & dt <= date("2020-04-17")) %>% 
  #filter(dt >= date("2020-03-16") & dt <= date("2020-03-29")) %>% 
  filter(leitos == "covid_positivo") %>% 
  ggplot(aes(x = dt, y = n, color = leitos, label = n)) + 
  geom_point(data = prefeito_positivos, aes(x=dt, y = n)) +
  geom_point(colour = cor) +  geom_smooth(method = "lm", colour = cor, se = FALSE, alpha = 0.5, fullrange = TRUE) +
  scale_y_continuous(trans='log2', n.breaks = 8, limits = c(4,1024)) +
  geom_text(nudge_y = 0.5, show.legend = FALSE, colour =  cor) + theme_light() +
  annotate(geom="text", label="174 Leitos de UTI extras para Corona", x=as.Date("2020-03-20"), y=174, vjust=-0.5, colour = "blue", hjust = 0) +
  annotate(geom="text", label="255 Plano em fase avançada", x=as.Date("2020-03-20"), y=255, vjust=-0.5, colour = "orange", hjust = 0) +
  annotate(geom="text", label="383 Leitos máximos de UTI com equipamento adicional", x=as.Date("2020-03-20"), y=383, vjust=-0.5, colour = "red", hjust = 0)+
  ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
  xlab("Tempo decorrente") + 
  ylab("Quantidade de pacientes (log2)") +
  scale_color_discrete(name = "Pacientes: ",
                       labels = c("Primeiros 15 dias", "Progressão sem isolamento domiciliar", "Inicio da ação do isolamento")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(date_breaks = "1 week",date_labels = "%d-%m",
               limits = as.Date(c("2020-03-19", today() + 7))) + 
  labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
       caption = "Fonte: Dashboard das UTIs") +
  theme(legend.position = "none") +
  geom_hline(yintercept=174, linetype="dashed",  color = "blue", size=0.5) +
  geom_hline(yintercept=255, linetype="dashed",  color = "orange", size=0.5) +
  geom_hline(yintercept=383, linetype="dashed",  color = "red", size=0.5) + 
  #geom_vline(xintercept = today()) + theme(legend.position = "none") +
  geom_segment(aes(x = today(), y = 0, xend = today(), yend = 174))

tempo <- as.numeric(max(prefeito_dados$dt) - min(prefeito_dados$dt))


# Cálculos dos tempos ---------------------------------------------




tempo_duplicacao_7 <- double_time(data_inicial = today(), positivos_vetor[length(positivos_vetor)], data_final = today() - 6, positivos_vetor[length(positivos_vetor) - 6])
tempo_duplicacao_5 <- double_time(data_inicial = today(), positivos_vetor[length(positivos_vetor)], data_final = today() - 4, positivos_vetor[length(positivos_vetor) - 4])
tempo_duplicacao_3 <- double_time(data_inicial = today(), positivos_vetor[length(positivos_vetor)], data_final = today() - 2, positivos_vetor[length(positivos_vetor) - 4])


# Simulações 7, 5 e 3 dias ------------------------------------------------
 retorna_caos(dias_para_tras = 6)
 
 caos_7_dias <- retorna_caos(dias_para_tras = 6)
 caos_5_dias <- retorna_caos(dias_para_tras = 4)
 caos_3_dias <- retorna_caos(dias_para_tras = 2)
 

 prefeito_dados2 <- uti_agregado_com_escalas_imputada %>% 
   filter(dt <= caos_7_dias$data_estouro[3]) %>% 
   filter(adulto_ped == "UTI ADULTO" | is.na(adulto_ped)) %>% 
   select(dt, covid_positivo) %>% 
   # mutate(covid_7dias = ifelse(dt >= today() - 8 & !(dt == today()), covid_positivo, NA)) %>% 
   mutate(covid_15dias = ifelse(dt >= today() - 14, covid_positivo, NA),
          covid_inicial = ifelse(dt <= min(dt) + 14, covid_positivo, NA)) %>% 
   mutate(covid_inicial = ifelse(covid_inicial >= 16, NA, covid_inicial)) %>% 
   pivot_longer(cols = -dt, names_to = "leitos", values_to = "n") 

grafico_tendencia_ultimos_7 <- prefeito_func(df = prefeito_dados, cor = "blue", dt_inicial = today() - 6, dt_final = today())
grafico_tendencia_ultimos_5 <- prefeito_func(df = prefeito_dados, cor = verde, dt_inicial = today() - 4, dt_final = today())
grafico_tendencia_ultimos_3 <- prefeito_func(df = prefeito_dados, cor = roxo, dt_inicial = today() - 2, dt_final = today())



prefeito_func(df = prefeito_dados2, cor = "blue", dt_inicial = today() - 6, dt_final ="2020-08-04", data_final_grafico = "2020-08-04", fullrange = TRUE)
caos_7_dias



tempo_duplicacao_calculado_ultimos7 <- double_time(data_inicial = today() - 7, valor_inicial = 31, data_final = today(), 34)

datas_limites <- data_caos(n_inicial = positivos_vetor[length(positivos_vetor)], tp_duplicacao = tempo_duplicacao_calculado_ultimos7[2] )

md_data <- uti_agregado_com_escalas_imputada %>% 
  filter(dt >= today() - 14) %>% 
  group_by(dt) %>% 
  summarise(n = sum(covid_positivo), n_suspeitos = sum(covid_susp), dias = 1:length(n_suspeitos)) 

model <- lm(data = md_data, formula = n ~ dt)
summary(model)


desenha_grafico_regressao <- function(data, 
                                      data_inicial = date("2020-03-10"), 
                                      data_final =  date("2020-08-04"), 
                                      dt_inicial_regressao =  today() - 15, 
                                      dt_final_regressao =  today(), 
                                      leitos_fase_inicial =  174, 
                                      leitos_fase_intermediaria =  255, 
                                      leitos_fase_final =  383) {
  regressao <- uti_agregado_com_escalas_imputada_adulto %>% 
    filter(dt >= dt_inicial_regressao & dt <= dt_final_regressao)
  
  
  
  uti_agregado_com_escalas_imputada_adulto %>% 
    ggplot(aes(x = dt, y = covid_positivo, label = covid_positivo)) + 
    geom_rect(xmin = dt_inicial_regressao, xmax = dt_final_regressao, fill = roxo, alpha = 0.002, ymin = 0, ymax = 8) + 
    geom_line() + geom_point() +
    scale_x_date(date_breaks = "1 weeks", date_minor_breaks = "1 days", limits = c(data_inicial, data_final), date_labels =  "%d-%b") +
    scale_y_continuous(trans='log2', n.breaks = 8, limits = c(1,1024)) +
    geom_line(data = regressao, aes(x = dt, y = covid_positivo, label = covid_positivo, color = vermelho)) + geom_point(data = regressao, aes(color = vermelho)) + 
    geom_smooth(data = regressao, method = "lm", colour = vermelho, se = FALSE,  fullrange = TRUE) +
    geom_text(nudge_y = 0.2, show.legend = FALSE, colour =  vermelho) + theme_light() +
    annotate(geom="text", label=paste0(leitos_fase_inicial, " Leitos de UTI extras para Corona"), x=as.Date("2020-03-20"), y = leitos_fase_inicial, vjust=-0.5, colour = "blue", hjust = 0) +
    annotate(geom="text", label=paste0(leitos_fase_intermediaria, " Leitos - Plano em fase avançada"), x=as.Date("2020-03-20"), y = leitos_fase_intermediaria, vjust=-0.5, colour = "orange", hjust = 0) +
    annotate(geom="text", label=paste0(leitos_fase_final, " Leitos máximos de UTI com equipamento adicional"), x=as.Date("2020-03-20"), y=leitos_fase_final, vjust=-0.5, colour = "red", hjust = 0)+
    ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
    xlab("Tempo decorrente") + 
    ylab("Quantidade de pacientes (log2)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
         caption = "Fonte: Dashboard das UTIs") +
    theme(legend.position = "none") +
    geom_hline(yintercept = leitos_fase_inicial, linetype = "dashed",  color = "blue", size=0.5) +
    geom_hline(yintercept = leitos_fase_intermediaria, linetype = "dashed",  color = "orange", size=0.5) +
    geom_hline(yintercept = leitos_fase_final, linetype = "dashed",  color = "red", size=0.5)
  
}



# Teste com os suspeitos --------------------------------------------------


data_inicial <-  date("2020-03-10")
data_final <-   date("2020-08-04") 
leitos_fase_inicial <- 174
leitos_fase_intermediaria <-  255
leitos_fase_final <-  383
dt_inicial_regressao <- today() - 7
dt_final_regressao <- today()


quantidade_tempo_dobra <- uti_agregado_com_escalas_imputada_adulto %>% 
  select(dt, covid_positivo) %>% 
  filter(dt == dt_inicial_regressao | dt == dt_final_regressao)

tempo_duplicacao <- double_time(data_inicial = quantidade_tempo_dobra$dt[1], 
            valor_inicial = quantidade_tempo_dobra$covid_positivo[1],
            data_final = quantidade_tempo_dobra$dt[2], 
            valor_final = quantidade_tempo_dobra$covid_positivo[2])

anotacao <- paste0("Tempo de duplicação: ", round(tempo_duplicacao[2],2), " dias")

suspeitos <- uti_agregado_com_escalas_imputada_adulto %>% 
  select(dt, covid_susp, covid_positivo) %>% 
  group_by(dt) %>% 
  mutate(covid_total = covid_susp + covid_positivo)

regressao <- uti_agregado_com_escalas_imputada_adulto %>% 
  filter(dt >= dt_inicial_regressao & dt <= dt_final_regressao)

uti_agregado_com_escalas_imputada_adulto %>% 
  ggplot(aes(x = dt, y = covid_positivo, label = covid_positivo)) + 
  geom_rect(xmin = dt_inicial_regressao, xmax = dt_final_regressao, fill = roxo, alpha = 0.002, ymin = 0, ymax = 8) + 
  geom_line() + geom_point() + 
  geom_point(data = suspeitos, mapping = aes(x = dt, y = covid_total, color = "blue")) +
  scale_x_date(date_breaks = "1 weeks", date_minor_breaks = "1 days", limits = c(data_inicial, data_final), date_labels =  "%d-%b") +
  scale_y_continuous(trans='log2', n.breaks = 8, limits = c(1,1024)) +
  geom_line(data = regressao, aes(x = dt, y = covid_positivo, label = covid_positivo, color = vermelho)) + geom_point(data = regressao, aes(color = vermelho)) + 
  geom_smooth(data = regressao, method = "lm", colour = vermelho, se = FALSE,  fullrange = TRUE) +
  geom_text(nudge_y = 0.2, show.legend = FALSE, colour =  vermelho) + theme_light() +
  annotate(geom="text", label=paste0(leitos_fase_inicial, " Leitos de UTI extras para Corona"), x=as.Date("2020-03-20"), y = leitos_fase_inicial, vjust=-0.5, colour = "blue", hjust = 0) +
  annotate(geom="text", label=paste0(leitos_fase_intermediaria, " Leitos - Plano em fase avançada"), x=as.Date("2020-03-20"), y = leitos_fase_intermediaria, vjust=-0.5, colour = "orange", hjust = 0) +
  annotate(geom="text", label=paste0(leitos_fase_final, " Leitos máximos de UTI com equipamento adicional"), x=as.Date("2020-03-20"), y=leitos_fase_final, vjust=-0.5, colour = "red", hjust = 0)+
  ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
  xlab("Tempo decorrente") + 
  ylab("Quantidade de pacientes (log2)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
       caption = "Fonte: Dashboard das UTIs") +
  theme(legend.position = "none") +
  geom_hline(yintercept = leitos_fase_inicial, linetype = "dashed",  color = "blue", size=0.5) +
  geom_hline(yintercept = leitos_fase_intermediaria, linetype = "dashed",  color = "orange", size=0.5) +
  geom_hline(yintercept = leitos_fase_final, linetype = "dashed",  color = "red", size=0.5) +
  annotate(geom="text", x=today() + 5, y=4, label= anotacao, hjust = 0, 
           color="red")




desenha_grafico_regressao()

tail(uti_agregado_diario_imputada_adulto)
# Estudo para o artigo ----------------------------------------------------


# library(ggrepel)
# uti_diaria %>%
#   filter(grepl("ADULTO", adulto_ped)) %>% 
#   complete(dt, local) %>% 
#   fill(.direction = "updown") %>% 
#   group_by(dt) %>% 
#   summarise(leitos = sum(leitos, na.rm = TRUE),
#             covid_susp = sum(covid_susp, na.rm = TRUE),
#             covid_positivo = sum(covid_positivo, na.rm = TRUE)) %>% 
#   pivot_longer(cols = leitos:covid_positivo) %>% 
#   ggplot(aes(x = dt, y = value, color = name, label = value)) + geom_line() + geom_point() +
#   ggtitle("Quantidade de leitos de UTI, pacientes positivos e pacientes suspeitos de COVID-19") +
#   xlab("Tempo decorrente") + 
#   ylab("Quantidade") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   scale_x_date(date_breaks = "1 days",date_labels = "%d-%m",
#                limits = as.Date(c("2020-03-19", today() + 7))) + 
#   labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
#        caption = "Fonte: Dashboard das UTIs") +
#   geom_text_repel(show.legend = FALSE) +
#   scale_color_discrete(name = "Legenda:",
#                        labels = c("Covid (+)", "Suspeito", "Leitos"))

uti_agregado_diario_adulto$leitos

double_time(data_inicial = "2020-06-07", valor_inicial = 50, data_final = "2020-06-12", 66)

double_time(data_inicial = "2020-06-08", valor_inicial = 55, data_final = "2020-06-13", 72)







