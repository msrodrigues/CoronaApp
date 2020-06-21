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
library(shiny)
library(shinythemes)

Sys.setenv(TZ="Brazil/East")
options(tz="Brazil/East")
Sys.getenv("TZ")
options(scipen = 999999)

# source("~/Dropbox/Coding/R/funs/msrfun.R")
# source(here("src/carregamento_utis.R"))
#source("~/Dropbox/Coding/R/SMS/Corona/src/carregamento_utis.R")



# Carretamento do dashboard das UTIs --------------------------------------

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


getwd()
uti <- read_excel("uti.xlsx")
uti <- bind_cols(uti, as.data.frame(str_split(uti$`Local Informante`, pattern  = " - ", simplify = TRUE)))


names(uti) <- c("dt", "email", "local", "leitos", "bloqueados", "pacientes", 
                "covid_susp", "covid_positivo", "senha", "hospital", "adulto_ped", "tipo_uti")
uti <- uti %>% 
    mutate(dt_time = dt, 
           dt = as.Date(dt),
           pac_nao_covid = pacientes - covid_susp - covid_positivo,
           especializada = if_else(adulto_ped %in% c("UTI ADULTO", "UTI PEDIATRICA"), FALSE, TRUE))




# Criação dos DF de UTI auxiliares ----------------------------------------

# uti_diaria: Banco da UTI filtrado para a última observação diária (mais recente) 
uti_diaria <- uti %>%                 
    group_by(dt,local) %>% 
    filter(dt_time == max(dt_time)) %>% 
    ungroup()


# uti_agregado_diario - DF como o somatório das variáveis de cada Hospital, por dia 
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
    fill(leitos:especializada) %>% 
    ungroup()

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




# Definições de constantes ------------------------------------------------
cor = "#f35e5a" # vermelho
cor = "#17b12b" # verde
cor = "#C77CFF" # fuscia

vermelho = "#f35e5a" # vermelho
verde = "#17b12b" # verde
roxo = "#C77CFF" # roxo

# Função Simula Progressão ------------------------------------------------

geomSeries <- function(base, max) {
    base^(0:floor(log(max, base)))
}


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


double_time <- function(data_inicial, valor_inicial,  data_final, valor_final) {
    tempo <- as.numeric(date(data_final) - date(data_inicial))
    taxa_crescimento <- log(valor_final/valor_inicial) / tempo
    tempo_duplicacao <- log(2)/taxa_crescimento
    cat("\n\nTempo: ", tempo, "\n")
    cat("\nTaxa de Crescimento: ", taxa_crescimento, "\n")
    cat("\nTempo de Duplicação: ", tempo_duplicacao, "\n")
    c(taxa_crescimento = taxa_crescimento, tempo_duplicacao = tempo_duplicacao)
}

double_time("2020-06-04", 44, "2020-06-11", 69)
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
    )





# Função desenha gráfico --------------------------------------------------



desenha_grafico_regressao <- function(data, 
                                      data_inicial = date("2020-03-10"), 
                                      data_final =  date("2020-08-04"), 
                                      dt_inicial_regressao =  today() - 15, 
                                      dt_final_regressao =  today(), 
                                      leitos_fase_inicial =  174, 
                                      leitos_fase_intermediaria =  255, 
                                      leitos_fase_final =  383,
                                      v_line = today(),
                                      escalaY = "log2") {
    

    
    regressao <- uti_agregado_com_escalas_imputada_adulto %>% 
        filter(dt >= dt_inicial_regressao & dt <= dt_final_regressao)
    
    
    quantidade_tempo_dobra <- uti_agregado_com_escalas_imputada_adulto %>% 
        select(dt, covid_positivo) %>% 
        filter(dt == dt_inicial_regressao | dt == dt_final_regressao)
    
    tempo_duplicacao <- double_time(data_inicial = quantidade_tempo_dobra$dt[1], 
                                    valor_inicial = quantidade_tempo_dobra$covid_positivo[1],
                                    data_final = quantidade_tempo_dobra$dt[2], 
                                    valor_final = quantidade_tempo_dobra$covid_positivo[2])
    
    anotacao <- paste0(format(quantidade_tempo_dobra$dt[2], "%d/%m/%y"), ": ", quantidade_tempo_dobra$covid_positivo[2], " pacientes em UTI\n",
                       format(quantidade_tempo_dobra$dt[1], "%d/%m/%y"), ": ", quantidade_tempo_dobra$covid_positivo[1], " pacientes em UTI\n", 
                       "Diferença: ", quantidade_tempo_dobra$covid_positivo[2] -quantidade_tempo_dobra$covid_positivo[1], " paciente(s)\n",
                       "Dias entre as datas: ", quantidade_tempo_dobra$dt[2] - quantidade_tempo_dobra$dt[1], " dias\n",
        "Tempo de dobra: ", round(tempo_duplicacao[2],2), " dias")

    if (escalaY == "log2") {
        escala <- scale_y_continuous(trans='log2', n.breaks = 8, limits = c(1,1024))
        legendaY <- ylab("Quantidade de pacientes (log2)")
        anotacaoY <- annotate(geom="text", x=today() + 2, y = 2, label= anotacao, hjust = 0, size = 4,
                              fontface = "bold", color="red")
        retangulo <- geom_rect(xmin = dt_inicial_regressao, xmax = dt_final_regressao, fill = roxo, alpha = 0.002, ymin = 0, ymax = 8)
    } else {
        escala <- scale_y_continuous(n.breaks = 8, limits = c(1,400))
        legendaY <- ylab("Quantidade de pacientes")
        anotacaoY <- annotate(geom="text", x=today() + 2, y = 40, label= anotacao, hjust = 0, size = 4,
                              fontface = "bold", color="red")
        retangulo <- geom_rect(xmin = dt_inicial_regressao, xmax = dt_final_regressao, fill = roxo, alpha = 0.002, ymin = 0, ymax = 255)
    }
    
    uti_agregado_com_escalas_imputada_adulto %>% 
        ggplot(aes(x = dt, y = covid_positivo, label = covid_positivo)) + 
        retangulo + 
        geom_line() + geom_point() +
        scale_x_date(date_breaks = "1 weeks", date_minor_breaks = "1 days", limits = c(data_inicial, data_final), date_labels =  "%d-%b") +
        escala +
        geom_line(data = regressao, aes(x = dt, y = covid_positivo, label = covid_positivo, color = vermelho)) + geom_point(data = regressao, aes(color = vermelho)) + 
        geom_smooth(data = regressao, method = "lm", colour = vermelho, se = FALSE,  fullrange = TRUE) +
        geom_text(nudge_y = 0.2, show.legend = FALSE, colour =  vermelho) + theme_light() +
        annotate(geom="text", label=paste0(leitos_fase_inicial, " Leitos de UTI extras para Corona"), x=as.Date("2020-03-20"), y = leitos_fase_inicial, vjust=-0.5, colour = "blue", hjust = 0) +
        annotate(geom="text", label=paste0(leitos_fase_intermediaria, " Leitos - Plano em fase avançada"), x=as.Date("2020-03-20"), y = leitos_fase_intermediaria, vjust=-0.5, colour = "orange", hjust = 0) +
        annotate(geom="text", label=paste0(leitos_fase_final, " Leitos máximos de UTI com equipamento adicional"), x=as.Date("2020-03-20"), y=leitos_fase_final, vjust=-0.5, colour = "red", hjust = 0)+
        ggtitle("Progressão da quantidade de casos de UTI e linhas de tempo de duplicação") +
        xlab("Tempo decorrente") + 
        legendaY +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        labs(subtitle = paste0("Data: ", format(Sys.time(), "%d/%m/%Y as %X")), 
             caption = "Fonte: Dashboard das UTIs\nmsrodrigues@gmail.com") +
        theme(legend.position = "none") +
        geom_hline(yintercept = leitos_fase_inicial, linetype = "dashed",  color = "blue", size=0.5) +
        geom_hline(yintercept = leitos_fase_intermediaria, linetype = "dashed",  color = "orange", size=0.5) +
        geom_hline(yintercept = leitos_fase_final, linetype = "dashed",  color = "red", size=0.5) + 
        geom_vline(xintercept = v_line, linetype = "dashed", color = "black", size = 0.5) + 
        anotacaoY
        
    
}








# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Progressão do Total de Infectados em UTI Porto Alegre"),
    
    # Sidebar with a sliders
    sidebarLayout(
        sidebarPanel(width = 3,
                     
                     sliderInput(inputId = "linha_corte",
                                 label = "Linha de corte:",
                                 min = date("2020-03-19"),
                                 max = date("2020-12-31"),
                                 value = today()
                     ),
                     
                     sliderInput(inputId = "leitos_primeira_fase",
                                 label = "Leitos de UTI Corona na primeira fase:",
                                 min = 1,
                                 max = 400,
                                 value = 174
                     ),
                     
                     sliderInput(inputId = "leitos_fase_intermediaria",
                                 label = "Leitos de UTI Corona na fase intermediária:",
                                 min = 1,
                                 max = 400,
                                 value = 255
                     ),
                     
                     sliderInput(inputId = "leitos_fase_final",
                                 label = "Leitos de UTI Corona na fase avançada:",
                                 min = 1,
                                 max = 400,
                                 value = 383
                     ),
                     
                     # Entrada das Datas
                     sliderInput(inputId = "range_grafico", 
                                 label = "Limites das datas do gráfico (eixo x):", 
                                 min = date("2020-03-01"),
                                 max = date("2021-12-31"), 
                                 value = c(date("2020-03-19"), today() + 30),
                                 timeFormat = "%d-%b",
                                 dragRange = TRUE
                     ),
                     
                     
                     sliderInput(inputId = "range_regressao", 
                                 label = "Limites do datas da regressão:", 
                                 min = date("2020-03-01"),
                                 max = today(), 
                                 value = c(today() - 7, today()),
                                 timeFormat = "%d-%b",
                                 dragRange = TRUE
                     ),
                     sliderInput(inputId = "dimensoes", 
                                 label = "Dimensões do Gráfico (altura x largura)", 
                                 min = 250,
                                 max = 3800, 
                                 value = c(800, 1200),
                                 dragRange = TRUE
                     ),
                     radioButtons(inputId = "escalaY",
                                  label = "Escala do eixo Y", 
                                  choices = c("log2","linear"),
                                  selected = "log2", 
                                  inline = TRUE)
                     
        ), 
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("simulacao")
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    heightSize <- function() {
        input$dimensoes[1]
    }
    
    widthSize <- function() {
        input$dimensoes[2]
    }
    

    
    output$simulacao <- renderPlot({
        width <- input$dimensoes[2]
        height <- input$dimensoes[1]
        leitos_fase_inicial <- input$leitos_primeira_fase
        leitos_fase_intermediaria <- input$leitos_fase_intermediaria
        leitos_fase_final <- input$leitos_fase_final
        data_range_grafico <- input$range_grafico
        data_range_regressao <- input$range_regressao
        linha_corte <- input$linha_corte
        escalaY <- input$escalaY
        
        
        # draw the histogram with the specified number of bins
        desenha_grafico_regressao(data = uti_agregado_com_escalas_imputada_adulto, 
                                  data_inicial = data_range_grafico[1], 
                                  data_final = data_range_grafico[2], 
                                  leitos_fase_inicial = leitos_fase_inicial, leitos_fase_intermediaria = leitos_fase_intermediaria, leitos_fase_final = leitos_fase_final, 
                                  dt_inicial_regressao = data_range_regressao[1], dt_final_regressao = data_range_regressao[2], v_line = linha_corte,
                                  escalaY = escalaY)
    }, width = widthSize, height = heightSize, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)

