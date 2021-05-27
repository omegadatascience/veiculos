shinyServer(function(input, output, session) {

    recomendacao <- shiny::eventReactive(
        input$calc,
        {
            # input <- list(aparencia = 1, custobeneficio = 0.5, desempenho = .3, estabilidade = .4, precomax = 25000)
            aparencia <- input$aparencia
            custobeneficio <- input$custobeneficio
            desempenho <- input$desempenho
            estabilidade <- input$estabilidade
            precomax <- input$precomax

            pesos <- c(aparencia, custobeneficio, desempenho, estabilidade)
            pesos <- pesos/sum(pesos)

            data <- data %>%
                dplyr::filter(Preço <= precomax)

            X <- data %>%
                ungroup() %>% 
                select(matches("_4_")) %>% 
                as.matrix()

            data$Y <- rowSums(sweep(X, 2, pesos, "*"))

            data %>%
                arrange(desc(Y)) %>% 
                head(5)
        }
    )

    output$table <- shiny::renderTable({
        req(recomendacao())
        recomendacao() %>%
            select(version, year, Preço)
    })

    output$plot <- shiny::renderPlot({
        req(recomendacao())

        recomendacao()[, 1:17] %>% 
            gather(variable, value, -version, -year) %>% 
            

            ggplot(aes(x = variable, y = value)) +
            # geom_col() +
            geom_point(size = 3) +
            facet_wrap(~ version, ncol = 2) +
            coord_flip() +
            theme_minimal()




    })

})