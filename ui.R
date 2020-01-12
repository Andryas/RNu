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
                # fluidRow(align = "left", 
                #          shinyjs::hidden(
                #                       div(
                #                           id = "new_data_div",
                # 
                #                           # progressBar("new_data", 0, display_pct = TRUE),
                # 
                #                           uiOutput("classify"),
                # 
                #                           fluidRow(
                #                               align = "center", 
                #                               actionButton("nexti", "", icon("thumbs-up"), width = "20%")
                #                           )
                #                       )
                #                   )
                #          ))

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

