bs4DashPage(
    bs4Dash::dashboardHeader(disable = FALSE),
    bs4DashSidebar(
        skin = "white",
        minified = FALSE,
        shiny::sliderInput(
            inputId = "precomax",
            min = 0,
            max = 60000,
            value = 20000,
            label = "Valor disposto a pagar"
        ),
        shiny::selectizeInput(
            inputId = "caracteristicas",
            label = "Seleciona até 5 características",
            multi = TRUE,
            choices = caracteristicas,
            options = list(
                maxItems = 5
            )
        ),
        shiny::sliderInput(
            inputId = "recomendacao",
            label = "Recomendação",
            min = 0,
            max = 10,
            value = 5
        )
    ),
    bs4DashBody(
        # shiny::tags$head(
        #     shiny::tags$style(
        #         "body {min-height: 100vh;}"
        #     )
        # ),
        waiter::use_waiter(),
        shinyjs::useShinyjs(),
        shiny::fluidRow(
            shiny::uiOutput("firula")
        )
    )
)
