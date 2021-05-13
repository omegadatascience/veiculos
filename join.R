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
