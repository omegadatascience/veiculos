## Funções auxiliares: Projeto Omega Data Science ------------------------------
## Prof. Wagner Hugo Bonat Omega/UFPR ------------------------------------------
## Data: 26/05/2021 ------------------------------------------------------------


## Extrai componentes dos fatores de acordo com o número de fatores

extract_components <- function(object, factor) {
  composicao <- data.frame(object$loadings[,1:factor])
  composicao$Fator <- as.numeric(apply(object$loadings[,1:factor], 1, which.max))
  composicao <- composicao[order(composicao$Fator),]
  return(composicao)
}

## Calcula a nota para cada fator
compute_score <- function(object, factor, data) {
  comp <- extract_components(object = object, factor = factor)
  weights <- data.frame("W" = apply(comp[,1:factor], 1, max), "Fator" = comp$Fator)
  saida <- list()
  for(i in 1:factor) {
    saida[[i]] <- apply(data[rownames(weights[weights$Fator == i,])], 1, 
                        function(x) weighted.mean(x, w = weights[weights$Fator == i,]$W))
  }
  output <- data.frame(do.call(cbind, saida))
  names(output) <- paste("Fator", factor, 1:factor, sep = "_")
  return(output)
}

## Recomenda o carro
recomenda_carro <- function(criterio, valor, qtd, factor, modelo, data) {
  ## Extrai qual é o fator mais frequente
  fator <- as.numeric(names(which.max(table(modelo[criterio,]$Fator))))
  coluna <- paste("Fator", factor, fator, sep = "_")
  data$Y <- data[[coluna]]
  temp <- filter(data, Preço < valor) %>%
    arrange(desc(Y))
  return(list("Dados" = temp[1:qtd,], "Criterio" = coluna))
}
