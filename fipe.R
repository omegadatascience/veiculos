library(XML)
library(httr)
library(progress)
library(tidyverse)

tb <- readRDS("data/vehicle.rds")

# ===================================================================
# Crawler functions
# ===================================================================

get_brand <- function() {
    page <- httr::GET("https://www.icarros.com.br/tabela-fipe/index.jsp")
    # httr::status_code(page)
    h <- XML::htmlParse(page)
    h <- XML::xpathSApply(h, "//select[@id='sltMake']//option")

    h_tb <- purrr::map(h, ~ tibble::tibble(brand = XML::xmlValue(.x), id_brand = XML::xmlGetAttr(.x, "value"))) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(id_brand = as.numeric(id_brand), brand = tolower(brand)) %>%
        tidyr::drop_na() %>%
    return(h_tb)
}

get_models <- function(id_brand) {
    tb <- jsonlite::read_json(stringr::str_interp("https://www.icarros.com.br/rest/select-options/marca/${id_brand}/modelos", list(id_brand = id_brand)))
    tb <- purrr::map(tb, tibble::as_tibble) %>%
        dplyr::bind_rows() %>%
        dplyr::rename(id_model = id, model = nome) %>% 
        dplyr::mutate(id_brand = !!id_brand, model = tolower(model)) %>% 
        dplyr::select(id_brand, model, id_model)
    return(tb)
}

get_years <- function(id_model) {
    tb <- jsonlite::read_json(stringr::str_interp("https://www.icarros.com.br/rest/select-options/modelo/${id_model}/anosModelo", list(id_model = id_model)))
    tb <- purrr::map(tb, tibble::as_tibble) %>%
        dplyr::bind_rows() %>%
        dplyr::rename(id_year = id, year = nome) %>% 
        dplyr::mutate(id_model = !!id_model) %>% 
        dplyr::select(id_model, year, id_year)
    return(tb)
}

get_versions <- function(id_model, id_year) {
    tb <- jsonlite::read_json(
        stringr::str_interp("https://www.icarros.com.br/rest/select-options/modelo/${id_model}/${id_year}/versoes",
            list(id_model = id_model, id_year = id_year))
    )
    tb <- purrr::map(tb, tibble::as_tibble) %>%
        dplyr::bind_rows() %>%
        dplyr::rename(id_version = id, version = nome) %>% 
        dplyr::mutate(id_model = !!id_model, id_year = !!id_year) %>% 
        dplyr::select(id_model, id_year, version, id_version)
    return(tb)
}

get_vehicle <- function(nome, email, uf, id_brand, id_model, id_year, id_version) {
    body <- stringr::str_interp("anoModelo=${id_year}&el=mai${email}&marca=${id_brand}&modelo=${id_model}&nome=${nome}&uf={uf}&valorselecionado=meucarro&versao=${id_version}")

    page <- httr::POST(

        url = "https://www.icarros.com.br/tabela-fipe/resultado.jsp",
        
        httr::add_headers(
            c(
                "user-agent" = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:87.0) Gecko/20100101 Firefox/87.0",
                'sec-ch-ua' = '" Not A;Brand";v="99", "Chromium";v="90", "Google Chrome";v="90"',
                'sec-ch-ua-mobile' = '?0',
                'Upgrade-Insecure-Requests' = '1',
                'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36',
                'Content-Type' = 'application/x-www-form-urlencoded',
                'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
                'Sec-Fetch-Site' = 'same-origin',
                'Sec-Fetch-Mode' = 'navigate',
                'Sec-Fetch-User' = '?1',
                'Sec-Fetch-Dest' = 'document'
            )
        ),

        body = body,
        encode = "raw"
    )
    
    cat(content(page, "text"), file = paste0("html/", id_brand, "_", id_model, "_", id_year, "_", id_version, ".html"))

    # httr::status_code(page)
    # h <- XML::htmlParse(page)
    # value <- XML::xpathSApply(h, "//section[@class='detail-content']//h3[@class='title title--price' and contains(text(), 'Preço FIPE')]/strong", XML::xmlValue)
    # value <- trimws(value)
    # return(value)
}

scrap_vehicle <- function(id_brand, id_model, id_year, id_version) {
    h <- XML::htmlParse(stringr::str_interp("html/${id_brand}_${id_model}_${id_year}_${id_version}.html"))

    img_url <- XML::xpathSApply(h, "//div[@class='col-xs-12 col-md-9']//img[@class='img-responsive imglazy']", XML::xmlGetAttr, "data-src")
    img_url <- stringr::str_subset(img_url, ".webp$")
    value <- XML::xpathSApply(h, "//section[@class='detail-content']//h3[@class='title title--price' and contains(text(), 'Preço FIPE')]/strong", XML::xmlValue)
    value <- trimws(value)

    dplyr::tibble(
        id_brand = id_brand,
        id_model = id_model,
        id_year = id_year,
        id_version = id_version,
        img_url = img_url,
        value = value
    )
}

# ===================================================================
# Getting FIPE
# ===================================================================
tb_brand <- get_brand()

tb_brand <- tb_brand %>%
    dplyr::filter(brand %in% tolower(unique(tb$brand)))

tb_params <- tibble::tibble()

for (.x in split(tb_brand, tb_brand$id_brand)) {
    tb_models <- get_models(.x$id_brand)
    tb_models <- .x %>%
        dplyr::left_join(tb_models, by = "id_brand") %>%
        dplyr::filter(model %in% (tb %>% dplyr::filter(tolower(brand) == .x[["brand"]]) %>% dplyr::pull(model) %>% unique() %>% tolower()))

    for (.y in split(tb_models, tb_models$id_model)) {
        tb_years <- get_years(.y$id_model)
        tb_years <- .y %>%
            dplyr::left_join(tb_years, by = "id_model") %>%
            dplyr::filter(id_year %in% (tb %>% dplyr::filter(tolower(brand) == .y[["brand"]] & tolower(model) == .y[["model"]]) %>% dplyr::pull(year) %>% unique()))
        
        for (.z in split(tb_years, tb_years$id_year)) {
            Sys.sleep(runif(1, 0, 5))
            
            tb_versions <- get_versions(.z$id_model, .z$id_year)
            tb_versions <- .z %>%
                dplyr::left_join(tb_versions, by = c("id_model", "id_year"))
            
            tb_params <- tb_params %>%
                dplyr::bind_rows(tb_versions)
            
            print(tail(tb_params))
        }
    }
}    

i <- 1
pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) eta: :eta", total = nrow(tb_params))
pb$tick(i - 1)

if (!dir.exists("html")) dir.create("html")
for (i in i:nrow(tb_params)) {
    Sys.sleep(runif(1, 0, 5))
    get_vehicle(
        nome = "Andryas Waurzenczak",
        email = "Andryaas@gmail.com",
        uf = "PR",
        id_brand = tb_params$id_brand[i],
        id_model = tb_params$id_model[i],
        id_year = tb_params$id_year[i],
        id_version = tb_params$id_version[i]
    )
    pb$tick(1)
}

tb_scrap <- purrr::map(split(tb_params, 1:nrow(tb_params)), ~ scrap_vehicle(.x$id_brand, .x$id_model, .x$id_year, .x$id_version)) %>% 
    dplyr::bind_rows()

params <- tb_params %>%
    dplyr::left_join(
        tb_scrap,
        by = c("id_brand", "id_model", "id_year", "id_version")
    )

saveRDS(params, "data/params.rds")
