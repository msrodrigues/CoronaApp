regressao <- lm(data = uti_agregado_com_escalas_imputada_adulto, dt ~ covid_positivo )
data_regressao(regressao, 174)
174 / tan(regressao$coefficients[2])

data_estouro <-  predict(regressao, data.frame(covid_positivo = 174))
data_estouro <- round(data_estouro)
date("1970-01-01") + (data_estouro)
date(round(), origin = "1970-01-01")



data_regressao <- function(modelo, leitos, origem = "1970-01-01") { 
  data_estouro <- predict(modelo, tibble(covid_positivo = leitos))
  data_estouro <- round(data_estouro)
  date(origem) + data_estouro
}


modelo_regressao <- lm(data = regressao, dt ~ covid_positivo)


data_inicial <- data_regressao(modelo = modelo_regressao, leitos = leitos_fase_inicial)
data_intermediaria <- data_regressao(modelo = modelo_regressao, leitos = leitos_fase_intermediaria)
data_final <- data_regressao(modelo = modelo_regressao, leitos = leitos_fase_final)
v_line <- data_inicial





#######

dt_inicial_regressao <- today() - 4
dt_final_regressao <- today()

regressao <- uti_agregado_com_escalas_imputada_adulto %>% 
  filter(dt >= dt_inicial_regressao & dt <= dt_final_regressao)

modelo_regressao <- lm(data = regressao, dt ~ covid_positivo)

data_regressao <- function(modelo, leitos, origem = "1970-01-01") { 
  data_estouro <- predict(modelo, tibble(covid_positivo = leitos))
  data_estouro <- round(data_estouro)
  date(origem) + data_estouro
}
data_inicial <- data_regressao(modelo = modelo_regressao, leitos = 174)
v_line <- data_inicial
