shinyServer(function(input, output, session) {
    output$firula <- shiny::renderUI({

        # e1 <- mtcars %>%
        #     e_charts(
        #         mpg,
        #         height = 200,
        #         elementId = "chart1" # specify id
        #     ) %>%
        #     e_scatter(wt) %>%
        #     e_datazoom(show = FALSE) # hide

        # e2 <- mtcars %>%
        #     e_charts(
        #         wt,
        #         height = 200,
        #         elementId = "chart2" # specify id
        #     ) %>%
        #     e_scatter(qsec) %>%
        #     e_datazoom(show = FALSE) # hide

        # e3 <- mtcars %>%
        #     e_charts(
        #         qsec,
        #         height = 200
        #     ) %>%
        #     e_scatter(hp) %>%
        #     e_datazoom() %>%
        #     e_connect(c("chart1", "chart2")) # connect

        df <- data.frame(
            x = LETTERS[1:5],
            e1 = runif(5, 1, 5),
            e2 = runif(5, 3, 7),
            e3 = runif(5, 3, 7),
            e4 = runif(5, 3, 7),
            e5 = runif(5, 3, 7),
            e6 = runif(5, 3, 7),
            e7 = runif(5, 3, 7),
            e8 = runif(5, 3, 7)
        )
        df <- df %>%
            gather(variable, value, -x)

        for (i in 1:8) {
            output[[paste0("plot", i)]] <- echarts4r::renderEcharts4r({

                df %>%
                    filter(variable == paste0("e",i)) %>% 
                    e_charts(x) %>%
                    e_radar(value, max = 7, name = "radar") %>%
                    e_tooltip(trigger = "item") %>% 
                    e_group("grp")
            })
        }

        output$master <- echarts4r::renderEcharts4r({
                
                paste0(
                    "df %>%
                        spread(variable, value) %>% 
                        e_charts(x) %>% ",
                    paste0(
                        paste0("e_radar(e", 1:8, ', max = 7, name = "e', 1:8, '")'),
                        collapse = " %>% "
                    )
                ) %>% parse(text = .) %>% 
                eval()

                
                    

                # e_group("grp") %>% # assign group
                # e_connect_group("grp")
        })

        shiny::fluidRow(
          bs4Card(
                title = "Recomendação 1",
                collapsible = FALSE,
                width = 3,
                echarts4rOutput(paste0("plot", 1))
            ),
            bs4Card(
                title = "Recomendação 2",
                collapsible = FALSE,
                width = 3,
                echarts4rOutput(paste0("plot", 2))
            ),
            bs4Card(
                title = "Recomendação 3",
                collapsible = FALSE,
                width = 3,
                echarts4rOutput(paste0("plot", 3))
            ),
            bs4Card(
                title = "Recomendação 4",
                collapsible = FALSE,
                width = 3,
                echarts4rOutput(paste0("plot", 4))
            ),
              bs4Card(
                title = "Recomendação 5",
                collapsible = FALSE,
                width = 3,
                echarts4rOutput(paste0("plot", 5))
            ),
            bs4Card(
                title = "Recomendação 6",
                collapsible = FALSE,
                width = 3,
                echarts4rOutput(paste0("plot", 6))
            ),
            bs4Card(
                title = "Recomendação 7",
                collapsible = FALSE,
                width = 3,
                echarts4rOutput(paste0("plot", 7))
            ),
            bs4Card(
                title = "Recomendação 8",
                collapsible = FALSE,
                width = 3,
                echarts4rOutput(paste0("plot", 8))
            ),
            bs4Card(
                title = "Todos",
                collapsible = FALSE,
                width = 12,
                echarts4rOutput("master")
            )
        )
    })
})