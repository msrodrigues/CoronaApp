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


# Importa dados do Dashboard das UTIS
# Um instante de cada UTI por linha
uti <- read_sheet("https://docs.google.com/spreadsheets/d/1Wy8eskZoiI23Qui4Iamr_pOcLEoDaIVICE-Sgqsoi4s/edit#gid=830997102", sheet = 4) %>% 
  arrange(desc(Timestamp))




write.xlsx(x = uti,file = "uti.xlsx")

write.xlsx(x = uti,file = "Corona/uti.xlsx")

