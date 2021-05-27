bs4Dash::bs4DashPage(
    bs4Dash::dashboardHeader(),
    bs4Dash::bs4DashSidebar(
        skin = "#ffffff",
        minified = FALSE,
        shiny::column(
            12,
            align = "center",
            shiny::sliderInput(
                inputId = "precomax",
                label = "Preço máximo",
                min = 0,
                max = 60000,
                value = 25000
            ),
            shiny::sliderInput(
                inputId = "precomax",
                label = "Preço máximo",
                min = 0,
                max = 60000,
                value = 25000
            ),
            h3("Suas prioridades"),
            shiny::sliderInput(
                inputId = "aparencia",
                label = "Aparência",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1
            ),
            shiny::sliderInput(
                inputId = "custobeneficio",
                label = "Custo Beneficio",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1
            ),
            shiny::sliderInput(
                inputId = "desempenho",
                label = "Desempenho",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1
            ),
            shiny::sliderInput(
                inputId = "estabilidade",
                label = "Estabilidade",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1
            ),

            actionButton(
                inputId = "calc",
                label = "",
                icon = icon("gear")
            )
        )
        

    ),
    bs4Dash::bs4DashBody(

        shiny::fluidRow(
            bs4Dash::bs4Card(
                title = "Rank",
                width = 3,
                shiny::column(
                    12,
                    align = "center",
                    shiny::tableOutput("table")
                )
            ),
            bs4Dash::bs4Card(
                title = "Plot",
                width = 9,
                shiny::plotOutput("plot", height = "800px")
            )        
        )
    )
)