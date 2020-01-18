shinyUI(
    fluidPage(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        useShinyjs(),
        useShinydashboardPlus(),
        column(2),
        column(
            8,

            fluidRow(
                align = "center",

                ## =============================================================
                ## Add
                ## =============================================================
                actionBttn(
                    inputId = "nu_add",
                    label = "Add",
                    style = "minimal",
                    color = "royal"
                ),

                ## =============================================================
                ## Classifica
                ## =============================================================
                actionBttn(
                    inputId = "nu_classifica",
                    label = "Classifica",
                    style = "minimal",
                    color = "royal"
                ),

                ## =============================================================
                ## Historico
                ## =============================================================
                actionBttn(
                    inputId = "nu_historico",
                    label = "Meus dados",
                    style = "minimal",
                    color = "royal"
                )
            )

        ),
        column(2)
    )
)
