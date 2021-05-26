rm(list=ls())
library(tidyverse)

data <- tibble::as_tibble(readRDS("data/vehicle.rds"))
fipe <- tibble::as_tibble(readRDS("data/params.rds"))

data <- data %>%
    dplyr::mutate(
        brand = tolower(brand),
        model = tolower(model),
        version = trimws(stringr::str_replace(tolower(spec), brand, ""))
    )

fipe <- fipe %>%
    dplyr::mutate(
        value = stringr::str_replace_all(value, "\\D", ""),
        value = as.numeric(value),
        version = tolower(version),
        year = as.numeric(year)
    ) %>% 
    dplyr::distinct(brand, model, year, version, value) %>%
    tidyr::drop_na()


data <- data %>%
    mutate(
        v = stringr::str_extract(version, "\\b[0-9]{1}\\.[0-9]{1}\\b"),
        v2 = trimws(stringr::str_extract(version, "\\w+\\s(?=[0-9]\\.[0-9])")),
        v2 = trimws(stringr::str_replace(v2, model, "")),
        v2 = ifelse(v2 == "", NA, v2)
    ) 

# ===================================================================
# Regressão linear para estravassar os valores (1980:2017)
# ===================================================================
fipe <- fipe %>%
    mutate(
        v = stringr::str_extract(version, "\\b[0-9]{1}\\.[0-9]{1}\\b"),
        v2 = stringr::str_extract(version, stringr::str_c("\\b", na.exclude(unique(data$v2)), "\\b", collapse = "|")),
        v2 = trimws(stringr::str_replace(v2, model, "")),
        v2 = ifelse(v2 == "", NA, v2)
    ) %>%
    arrange(v2) %>%
    # filter(model == "celta" & v == "1.0" & v2 == "lt") %>% 
    group_by(brand, model, v, v2) %>%
    group_split() %>% 
    map(function(x) {
        m0 <- lm(value ~ year, data = x)
        x_hat <- tibble(year = 1980:2017)
        x_hat$value_hat <- predict(m0, newdata = x_hat)
        x %>% 
            select(brand, model, v, v2, year, value) %>% 
            full_join(x_hat, by = "year") %>% 
            mutate(brand = unique(brand)[1], model = unique(model)[1], v = unique(v)[1], v2 = unique(v2)[1])
    }) %>% 
    bind_rows()

# ===================================================================
# I - Join
# ===================================================================
data2 <- data %>%
    left_join(
        fipe %>%
            group_by(brand, model, year, v, v2) %>% 
            summarise(
                price = mean(value), 
                price_sd = sd(value), 
                price_hat = mean(value_hat), 
                price_hat_sd = sd(value_hat), 
                n = n(),
            ),
        by = c("brand", "model", "year", "v", "v2")
    )

# Total de missings
sum(is.na(data2$price_hat))

# data2 %>%
#     select(price, price_hat) %>%
#     gather(variable, value) %>%
#     ggplot(aes(x = value, fill = variable)) +
#     geom_density(alpha=.4)
# summary(data2$price_hat)

# ===================================================================
# II - Join - Calcula média do valor fipe por brand, model, year e v
# ===================================================================
data3 <- data2 %>%
    filter(is.na(price_hat)) %>%
    left_join(
        fipe %>%
            group_by(brand, model, year, v) %>% 
            summarise(
                price_v2 = mean(value), 
                price_sd_v2 = sd(value), 
                price_hat_v2 = mean(value_hat), 
                price_hat_sd_v2 = sd(value_hat), 
                n_v2 = n(),
            ),
        by = c("brand", "model", "year", "v")
    ) %>% 
    bind_rows(
        data2 %>%
            filter(!is.na(price_hat))
    )

# Não encontrou na fipe
data3 %>% 
    filter(is.na(price_hat_v2))

data3 %>%
    filter(is.na(price_hat)) %>%
    pull(price_hat_sd_v2) %>% 
    hist

data3 %>%
    filter(is.na(price_hat)) %>%
    pull(price_sd_v2) %>% 
    hist

data3 %>% 
    filter(model == "palio" & v == "1.0" & year == "2011")

## Carregando pacotes adicionais
library("FactoMineR")
library("factoextra")

# Nota média por modelo e ano
notas <- data3 %>%
    group_by(version, year) %>%
    summarize("Estilo" = mean(Estilo),
              "Acabamento" = mean(Acabamento),
              "Posição" = mean(`Posição de dirigir`),
              "Instrumentos" = mean(Instrumentos),
              "Interior" = mean(Interior),
              "Porta-malas" = mean(`Porta-malas`),
              "Desempenho" = mean(Desempenho),
              "Motor" = mean(Motor),
              "Câmbio" = mean(Câmbio),
              "Freios" = mean(Freios),
              "Suspensão" = mean(Suspensão),
              "Consumo" = mean(Consumo),
              "Estabilidade" = mean(Estabilidade),
              "Custo-Benefício" = mean(`Custo-Benefício`),
              "Recomendação" = mean(Recomendação),
              "Preço" = mean(price),
              "Preço_sd" = sd(price))


## Carregando alguns pacotes extras
library(corrplot)
library(FactoMineR)
source("functions.R")

## Visualizando a matriz de correlações
corrplot(cor(notas[,3:17]), method="circle")
corrplot(cor(notas[,3:17]), order = "FPC") # Primeiro componente principal
corrplot(cor(notas[,3:17]), order = "hclust") # Clusterização


## Componentes principais
model1 <- PCA(notas[,3:17], graph = FALSE)

## Variância dos componentes (autovalores)
get_eigenvalue(model1)

# Scree plot (dois componentes explicam cerca de 60% da variabilidade das notas)
fviz_eig(model1)

# Visualizando a divisão dos componentes
fviz_pca_var(model1)
fviz_pca_var(model1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) # Avoid text overlapping)


# Biplot
fviz_pca_biplot(model1)

# Qualidade das representações
var <- get_pca_var(model1)
corrplot(var$cos2, is.corr=FALSE)

# Contribuição para as componentes
var$contrib
corrplot(var$contrib, is.corr=FALSE)

## Entre dois ou três componentes deve ser suficiente
## Analise fatorial 
# Ideia: Vamos gerar duas ou três notas e ver quais notas dominam cada fator latente.
# Gerar uma nota para cada fator latente como uma média ponderada das notas originais.
# Criar uma base nova com duas ou três colunas adicionais.

ana_f2 <- factanal(notas[,3:17], factors = 2)
ana_f3 <- factanal(notas[,3:17], factors = 3)
ana_f4 <- factanal(notas[,3:17], factors = 4)

comp2 <- extract_components(object = ana_f2, factor = 2)
comp3 <- extract_components(object = ana_f3, factor = 3)
comp4 <- extract_components(object = ana_f4, factor = 4)

## Calculando as notas para o rankeamento
scores_f2 <- compute_score(object = ana_f2, factor = 2, data = notas)
scores_f3 <- compute_score(object = ana_f3, factor = 3, data = notas)
scores_f4 <- compute_score(object = ana_f4, factor = 4, data = notas)

## Criando uma base conjunta
data_score <- bind_cols(notas, scores_f2, scores_f3, scores_f4)
saveRDS(data_score, file = "data_score.rds")

## Fazendo a recomendação
tt <- recomenda_carro(criterio = c("Estilo", "Acabamento", "Consumo", "Estabilidade", "Freios"), 
                valor = 20000, qtd = 3, factor = 2, modelo = comp2, data = data_score)

tt2 <- recomenda_carro(criterio = c("Estilo", "Acabamento", "Consumo", "Estabilidade", "Freios"), 
                      valor = 20000, qtd = 3, factor = 3, modelo = comp3, data = data_score)

tt3 <- recomenda_carro(criterio = c("Suspensão", "Estabilidade", "Freios"), 
                       valor = 20000, qtd = 3, factor = 4, modelo = comp4, data = data_score)



