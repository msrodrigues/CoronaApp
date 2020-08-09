
# Carregamento das Bibliotecas --------------------------------------------



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
library(png)
library(grid)
library(gridExtra)
library(broom)

Sys.setenv(TZ="Brazil/East")
options(tz="Brazil/East")
Sys.getenv("TZ")
options(scipen = 999999)

# source("~/Dropbox/Coding/R/funs/msrfun.R")
# source(here("src/carregamento_utis.R"))
# source("~/Dropbox/Coding/R/SMS/Corona/src/carregamento_utis.R")


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

uti <- read_excel("uti.xlsx") %>% 
    arrange(desc(Timestamp))


uti <- bind_cols(uti, as.data.frame(str_split(uti$`Local Informante`, pattern  = " - ", simplify = TRUE)))


names(uti) <- c("dt", "email", "local", "leitos", "bloqueados", "pacientes", 
                "covid_susp", "covid_positivo", "senha", "emerg_covid", 
                "emerg_n_covid", "vent_mec", "hospital", "adulto_ped", "tipo_uti")

uti <- uti %>% 
    mutate(dt_time = dt, 
           dt = as.Date(dt),
           pac_nao_covid = pacientes - covid_susp - covid_positivo,
           especializada = if_else(adulto_ped %in% c("UTI ADULTO", "UTI PEDIATRICA"), FALSE, TRUE))

# Carregamento do brazão da prefeitura
logo_pmpa <- png::readPNG("logo_pmpa.png")
logo_pmpa <- matrix(rgb(logo_pmpa[,,1],logo_pmpa[,,2],logo_pmpa[,,3], logo_pmpa[,,4] * 0.2), nrow=dim(logo_pmpa)[1]) #0.2 is alpha


# Definições de cores ------------------------------------------------
vermelho = "#f35e5a" # vermelho
verde = "#17b12b"    # verde
roxo = "#C77CFF"     # roxo
rosa = "#FFA1FF"     # rosa



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
              utis_presentes = n(),
              emerg_covid = sum(emerg_covid, na.rm = TRUE),
              emerg_n_covid = sum(emerg_n_covid, na.rm = TRUE)
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
              emerg_covid = sum(emerg_covid, na.rm = TRUE),
              emerg_n_covid = sum(emerg_n_covid, na.rm = TRUE),
              vent_mec = sum(vent_mec, na.rm = TRUE),
              covid_susp = sum(covid_susp, na.rm = TRUE),
              covid_positivo_uti = sum(covid_positivo, na.rm = TRUE),
              pac_nao_covid = sum(pac_nao_covid, na.rm = TRUE),
              utis_presentes = n()
    ) %>% 
    mutate(covid_positivo = covid_positivo_uti + emerg_covid)

uti_agregado_diario_imputada_adulto <- uti_diaria_imputada_adulto_ped %>% 
    filter(grepl("ADULTO", adulto_ped)) %>% 
    group_by(dt) %>% 
    summarise(leitos = sum(leitos, na.rm = TRUE),
              bloqueados = sum(bloqueados, na.rm = TRUE),
              pacientes = sum(pacientes, na.rm = TRUE),
              emerg_covid = sum(emerg_covid, na.rm = TRUE),
              emerg_n_covid = sum(emerg_n_covid, na.rm = TRUE),
              vent_mec = sum(vent_mec, na.rm = TRUE),
              covid_susp = sum(covid_susp, na.rm = TRUE),
              covid_positivo_uti = sum(covid_positivo, na.rm = TRUE),
              pac_nao_covid = sum(pac_nao_covid, na.rm = TRUE),
              utis_presentes = n()
    ) %>% 
    mutate(covid_positivo = covid_positivo_uti,
           covid_total_uti = covid_positivo_uti + covid_susp,
           covid_total = covid_total_uti + emerg_covid) 



write.xlsx(x = uti_agregado_diario_imputada_adulto, file = "uti_agregado_diario_imputada_adulto.xlsx")
# Funções para cálculo do tempo de duplicação ------------------------------------------------

geomSeries <- function(base, max) {
    base^(0:floor(log(max, base)))
}


# Função para cálculo do tempo de duplicação
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
    )


# Função que calcula as previsões de data do estouro de leitos ------------

f_previsoes <- function(limites_de_leitos = c(174,255,383), 
                        data_inicial_regressao = today() - 6,
                        data_final_regressao = today()) {
    
    dias_uti <- nrow(uti_agregado_diario_imputada_adulto)
    uti_agregado_diario_imputada_adulto$dia <- 1:dias_uti
    
    uti_para_modelo <- uti_agregado_diario_imputada_adulto %>% 
        filter(dt >= data_inicial_regressao, dt <= data_final_regressao)
    
    
    modelo_positivo <- lm(formula = dia ~ covid_positivo, data = uti_para_modelo)
    futuro <- tibble(covid_positivo  = c(174, 255, 383))
    
    previsoes <- tibble(dias = predict(modelo_positivo, futuro))
    previsoes <- previsoes %>% 
        mutate(
            leitos = c(174, 255, 383),
            datas = min(uti_agregado_com_escalas_imputada_adulto$dt) + previsoes$dias -1
        )   
    previsoes
    
}
f_previsoes()

# Função desenha gráfico --------------------------------------------------

# Função que vai desenhar o gráfico das UTIs
desenha_grafico_regressao <- function(data,                                         # origem do dados
                                      data_inicial = date("2020-03-10"), 
                                      data_final =  date("2020-08-04"), 
                                      dt_inicial_regressao =  today() - 7, 
                                      dt_final_regressao =  today(), 
                                      leitos_fase_inicial =  174, 
                                      leitos_fase_intermediaria =  255, 
                                      leitos_fase_final =  383,
                                      v_line = today(),
                                      escalaY = "log2",
                                      tipo_regressao = "erro",
                                      suspeitos = TRUE) {
   
  
    # Captura a informação da ocupação atual
    ocupacao_atual <- tail(uti_agregado_diario_imputada_adulto$covid_positivo,1)
    
    
    # Cálculo das Previsões de Datas Limites
    previsoes <- f_previsoes(
        limites_de_leitos = c(leitos_fase_inicial, leitos_fase_intermediaria, leitos_fase_final),
        data_inicial_regressao = dt_inicial_regressao, data_final_regressao = dt_final_regressao
    )
    
    
    # Textos para serem acrescentados na lateral esquerda do gráfico  (previsões das datas de estouro)
    texto_leitos_primeira_fase <- paste0("Previsão de atingir ", leitos_fase_inicial, " leitos: ", format(previsoes$datas[1], "%d/%m/%Y"), "\n",
                                         "Dias para ", leitos_fase_inicial," leitos: ", floor(previsoes$datas[1] - today()), " dias")
    texto_leitos_fase_intermediaria <- paste0("Previsão de atingir ", leitos_fase_intermediaria, " leitos: ", format(previsoes$datas[2], "%d/%m/%Y"), "\n",
                                              "Previsão de atingir ", leitos_fase_final, " leitos: ",  format(previsoes$datas[3], "%d/%m/%Y"), "\n")
    texto_leitos_fase_final <- paste0("Previsão de atingir ", leitos_fase_final, " leitos: ", format(previsoes$datas[3], "%d/%m/%Y"), "\n")
    
    texto_previsoes <- case_when(
        ocupacao_atual <= leitos_fase_inicial ~ texto_leitos_primeira_fase,
        ocupacao_atual > leitos_fase_inicial & ocupacao_atual <= leitos_fase_intermediaria ~ texto_leitos_fase_intermediaria,
        TRUE ~ texto_leitos_fase_final
        
    )
    
    # Dados para cálculo da regressão e desenho da faixa 
    regressao <- uti_agregado_diario_imputada_adulto %>% 
        filter(dt >= dt_inicial_regressao & dt <= dt_final_regressao)
    
    
    
    # Seleção dos dias para cálculo do tempo de dobra
    quantidade_tempo_dobra <- uti_agregado_diario_imputada_adulto %>% 
        select(dt, covid_positivo) %>% 
        filter(dt == dt_inicial_regressao | dt == dt_final_regressao)
    
    
    # Cálculo do tempo de duplicação
    tempo_duplicacao <- double_time(data_inicial = quantidade_tempo_dobra$dt[1], 
                                    valor_inicial = quantidade_tempo_dobra$covid_positivo[1],
                                    data_final = quantidade_tempo_dobra$dt[2], 
                                    valor_final = quantidade_tempo_dobra$covid_positivo[2])
    
    # Criação do objeto anotação - com o texto para se colocado no canto direito do gráfico
    anotacao <- paste0(format(quantidade_tempo_dobra$dt[2], "%d/%m/%y"), ": ", quantidade_tempo_dobra$covid_positivo[2], " pacientes em UTI\n",
                       format(quantidade_tempo_dobra$dt[1], "%d/%m/%y"), ": ", quantidade_tempo_dobra$covid_positivo[1], " pacientes em UTI\n", 
                       "Diferença: ", quantidade_tempo_dobra$covid_positivo[2] -quantidade_tempo_dobra$covid_positivo[1], " paciente(s)\n",
                       "Variação: ", round(((quantidade_tempo_dobra$covid_positivo[2] - quantidade_tempo_dobra$covid_positivo[1]) / quantidade_tempo_dobra$covid_positivo[1]) * 100,2), " %\n", 
                       "Intervalo: ", quantidade_tempo_dobra$dt[2] - quantidade_tempo_dobra$dt[1], " dias\n",
                        "Tempo de dobra: ", round(tempo_duplicacao[2],2), " dias\n",
                       "Média de ", round((quantidade_tempo_dobra$covid_positivo[2] - quantidade_tempo_dobra$covid_positivo[1]) / as.numeric(quantidade_tempo_dobra$dt[2] - quantidade_tempo_dobra$dt[1]), 2 ), " pacientes por dia."
        )
    
    
    # Ajustes específicos para escala logarítica e linear
    if (escalaY == "log2") {
        escala <- scale_y_continuous(trans='log2', n.breaks = 8, limits = c(1,1024))
        rotulos <- geom_text(nudge_y = 0.3, show.legend = FALSE, colour =  vermelho, check_overlap = TRUE) 
        legendaY <- ylab("Quantidade de pacientes (log2)")
        anotacaoY <- annotate(geom="text", x=today() + 2, y = 2, label= anotacao, hjust = 0, size = 4,
                              fontface = "bold", color="red")
        retangulo <- geom_rect(xmin = dt_inicial_regressao, xmax = dt_final_regressao, fill = roxo, alpha = 0.002, ymin = 0, ymax = log(ocupacao_atual,2))
        anotacaoLinear <- NULL
        marca_dagua <- NULL
    } else {
        escala <- scale_y_continuous(n.breaks = 8, limits = c(1,400))
        rotulos <- geom_text(nudge_y = 10, show.legend = FALSE, colour =  vermelho, check_overlap = TRUE) 
        legendaY <- ylab("Quantidade de pacientes")
        anotacaoY <- annotate(geom="text", x=today() + 2, y = 40, label= anotacao, hjust = 0, size = 4,
                              fontface = "bold", color="red")
        anotacaoLinear <- NULL #annotate(geom="text", x= date("2020-03-19"), y = 100, label= texto_previsoes, hjust = 0, size = 4,
                          #fontface = "bold", color="red")
        retangulo <- geom_rect(xmin = dt_inicial_regressao, xmax = dt_final_regressao, fill = roxo, alpha = 0.002, ymin = 0, ymax = ocupacao_atual)
        marca_dagua <- annotation_custom(xmin = as.Date("2020-03-13"), ymin = 60, ymax = 350, grob = rasterGrob(logo_pmpa))
    }
    
    
    # Escolha dos tipos de regressão
    if (tipo_regressao == "erro") {
        regressao_grafico <- geom_smooth(data = regressao, method = "lm", colour = vermelho, se = TRUE,  fullrange = TRUE)
    } else if (tipo_regressao == "linear") {
        regressao_grafico <- geom_smooth(data = regressao, method = "lm", colour = vermelho, se = FALSE,  fullrange = TRUE)
    } else {
        regressao <- uti_agregado_com_escalas_imputada_adulto
        regressao_grafico <- geom_smooth(data = regressao, colour = vermelho, se = TRUE,  fullrange = TRUE)
    }
    
    
    # Criação do modelo
    modelo <- lm(covid_positivo ~ dt  , data = regressao)
    novos_dados <- tibble(dt = seq.Date(from = today(), to = today() + 45, by = 1))
    previsoes_futuras <- predict(modelo, novos_dados)
    dt_383 <- today() + which(previsoes_futuras >= 382)[[1]] -1
    regressao_fit <- augment(modelo)
    
   
       
    
    # Desenho do Gráfico propriamente dito
    uti_agregado_diario_imputada_adulto %>% 
        ggplot(aes(x = dt, y = covid_positivo, label = covid_positivo)) + 
        retangulo  +
        geom_line() + geom_point() + 
        scale_x_date(date_breaks = "1 weeks", date_minor_breaks = "1 days", limits = c(data_inicial, data_final), date_labels =  "%d-%b") +
        escala +
        geom_line(data = regressao, aes(x = dt, y = covid_positivo, label = covid_positivo, color = vermelho)) + geom_point(data = regressao, aes(color = vermelho)) + 
        regressao_grafico  + 
        rotulos + 
        theme_light() +
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
        anotacaoY + anotacaoLinear + marca_dagua +
        geom_hline(yintercept = 383, linetype = "dotted", size = 0.2) + 
        geom_vline(xintercept = dt_383, linetype = "dotted", size = 0.2)
                                                                                                          
    
}


desenha_grafico_regressao()

# Define UI for application ---------------------------

# ui <- navbarPage("Coronavirus Porto Alegre",
#                  tabPanel("Projeções"),
#                  navbarMenu("Medicamentos",
#                             tabPanel("Consumo"),
#                             tabPanel("Por Hospital"))
# )
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
                                  selected = "linear", 
                                  inline = TRUE),
                     
                     radioButtons(inputId = "tipo_regressao",
                                  label = "Opcionais", 
                                  choices = list("Linear" = "linear", "Linear com erro" = "erro", "Smooth" = "MA"),
                                  selected = "erro", 
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
        tipo_regressao <- input$tipo_regressao
        
        
        # draw the histogram with the specified number of bins
        desenha_grafico_regressao(data = uti_agregado_com_escalas_imputada_adulto, 
                                  data_inicial = data_range_grafico[1], 
                                  data_final = data_range_grafico[2], 
                                  leitos_fase_inicial = leitos_fase_inicial, leitos_fase_intermediaria = leitos_fase_intermediaria, leitos_fase_final = leitos_fase_final, 
                                  dt_inicial_regressao = data_range_regressao[1], dt_final_regressao = data_range_regressao[2], v_line = linha_corte,
                                  escalaY = escalaY,
                                  tipo_regressao = tipo_regressao)
    }, width = widthSize, height = heightSize, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
# 
# dt_inicial_regressao =  today() - 7
# dt_final_regressao =  today()
# regressao <- uti_agregado_diario_imputada_adulto %>% 
#     filter(dt >= dt_inicial_regressao & dt <= dt_final_regressao)
# regressao
# 
# 
# 
# 
# modelo <- lm(covid_positivo ~ dt  , data = regressao)
# novos_dados <- tibble(dt = seq.Date(from = today(), to = today() + 45, by = 1))
# previsoes_futuras <- predict(modelo, novos_dados)
# dt_383 <- today() + which(previsoes_futuras >= 382)[[1]] -1
# 
# 
# regressao_fit <- augment(modelo)
# ggplot(regressao, aes(x = dt, y = covid_positivo)) + 
#     geom_smooth(method = "lm",se = TRUE,  fullrange = TRUE) +
#     geom_point(data = regressao, aes(x = dt, y = covid_positivo)) +
#     scale_x_date(limits = c(dt_inicial_regressao, dt_final_regressao + 30), breaks = "weeks") +
#     scale_y_continuous(limits = c(300, 400), ) +
#     geom_hline(yintercept = 383, linetype = "dotted", size = 0.2) + 
#     geom_vline(xintercept = dt_383, linetype = "dotted", size = 0.2) + theme(axis.line = element_line(colour = "azure4", 
#     size = 0.2, linetype = "solid"), axis.ticks = element_line(colour = "gray28", 
#     linetype = "dashed")) +labs(title = "Regressão linear e datas de limite de leitos", 
#     x = "Datas", y = "Quantidade de pacientes", 
#     subtitle = "Fonte: Dashboard das UTIs", 
#     caption = "msrodrigues@gmail.com")
# 
# regressao



