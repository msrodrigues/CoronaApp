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








