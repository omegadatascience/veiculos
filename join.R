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
# Regressão linear para extravagar os valores (1980:2017)
# ===================================================================
fipe <- fipe %>%
  mutate(
    v = stringr::str_extract(version, "\\b[0-9]{1}\\.[0-9]{1}\\b"),
    v2 = stringr::str_extract(
      version,
      stringr::str_c("\\b", na.exclude(unique(data$v2)), "\\b", collapse = "|")
    ),
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
      mutate(
        brand = unique(brand)[1],
        model = unique(model)[1],
        v = unique(v)[1],
        v2 = unique(v2)[1]
      )
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
  bind_rows(data2 %>%
    filter(!is.na(price_hat)))

# Não encontrou na fipe
data3 %>%
  filter(is.na(price_hat_v2) & is.na(price_hat) & is.na(price))

data3 <- data3 %>%
  dplyr::mutate(
    price_master = dplyr::case_when(
      !is.na(price) ~ price,
      !is.na(price_hat) ~ price_hat,
      !is.na(price_hat_v2) ~ price_hat_v2
    ),
    price_master_sd = dplyr::case_when(
      !is.na(price_sd) ~ price_sd,
      !is.na(price_hat_sd) ~ price_hat_sd,
      !is.na(price_hat_sd_v2) ~ price_hat_sd_v2
    )
  )

data3 <- data3 %>%
  dplyr::filter(!is.na(price_master))

saveRDS(data3,"data/fvehicle.rds")

w1 <- -1 * c(
  Consumo = -0.396718894619935,
  Recomendação = -0.318457470717304,
  `Custo-Benefício` = -0.271752474085786,
  Motor = -0.261592527637502,
  Desempenho = -0.166254198925394
)

notas$perfor <- c(as.matrix(data3[, names(w1)]) %*% cbind(w1))

w2 <- c(
  `Porta-malas` = 0.228077808451193,
  Estilo = 0.250031477708834,
  `Posição de dirigir` = 0.304590333769907,
  Acabamento = 0.315617816761883,
  Instrumentos = 0.324664718097992,
  Interior = 0.385784301404012
)
notas$design <- c(as.matrix(data3[, names(w2)]) %*% cbind(w2))