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
                ## History
                ## =============================================================
                actionBttn(
                    inputId = "in_nu_history",
                    label = "Meus dados", 
                    style = "minimal",
                    color = "royal"
                ),
                
                bsModal("history_modal", "Meus dados", "in_nu_history", size = "large", 
                        tabBox(width = 12, 
                               tabPanel("Histórico", dataTableOutput("history_tb")),
                               tabPanel("Classificação", dataTableOutput("classify_tb"))
                        ))
            )
            
            ),
        column(2)
    )
)

