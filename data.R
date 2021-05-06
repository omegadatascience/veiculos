library(tidyverse)

url_csv <- "https://raw.githubusercontent.com/leg-ufpr/hackathon/master/notas.csv"
tb_nt <- read.csv2(url_csv)
str(tb_nt)

url_json <- "https://raw.githubusercontent.com/leg-ufpr/hackathon/master/opinioes.json"
tb_op <- jsonlite::read_json(url_json, simplifyVector = TRUE)
tb_op <- as.data.frame(tb_op)
str(tb_op)

names(tb_op) <- c("ID", "title", "model", "usercity", "usage", "pro",
                  "con", "problem", "opinion", "ts")

tb_nt <- tb_nt %>%
    pivot_wider(id_cols = "ID", names_from = "quesito", values_from = "nota")
str(tb_nt)

tb <- inner_join(tb_nt,
                 tb_op[, c("ID", "model", "usercity", "usage", "ts")],
                 by = "ID")
str(tb)

tb$location <-
    tb$usercity %>%
    str_remove("^.*- ")

tb$uf <-
    tb$location %>%
    str_sub(start = -2)

tb$year <-
    tb$model %>%
    str_extract("\\d{4}/\\d{4}") %>%
    str_sub(end = 4) %>%
    as.integer()

tb$spec <-
    tb$model %>%
    str_remove(" \\d{4}/\\d{4}")

# Tempo que possui o veículo.
tb$period <-
    tb$usage %>%
    str_remove("menos de") %>%
    str_replace(".*há +(\\d+) +ano.*", "\\1") %>%
    as.integer()

# Último veículo.
tb$lastcar <-
    ifelse(str_detect(tb$usage, "Carro anterior:"),
           str_replace(tb$usage, ".*Carro anterior: (.*)$", "\\1"),
           NA_character_)

# Percurso.
tb$km <-
    ifelse(str_detect(tb$usage, "\\d+ km"),
           str_replace(tb$usage, ".* ([0-9.]+) km.*", "\\1"),
           NA_character_) %>%
    str_remove("\\.") %>%
    as.integer()

# Data da avaliação.
tb$ts <-
    tb$ts %>%
    as.POSIXct(format = "%d/%m/%Y %H:%M:%S")

# Fabricante.
tb$brand <-
    tb$spec %>%
    str_replace("^(\\w+) .*", "\\1")

tb$brand_last <-
    tb$lastcar %>%
    str_replace("^(\\w+) .*", "\\1")

tb$model <-
    tb$spec %>%
    str_replace("^\\w+ (\\w+) .*", "\\1")

saveRDS(tb, "data/vehicle.rds")

# tb$model <- NULL
# tb$usercity <- NULL
# tb$usage <- NULL