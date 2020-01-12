shinyServer(function(input, output, session) {

    aux <- reactiveValues(class = NULL, cat = NULL)
    
    ## =========================================================================
    ## Add Nova Informação
    ## =========================================================================
    observeEvent(input$nu_add, {

        showModal(modalDialog(
            title = span(style = "color: #bd2df5;", "Nubank - Informações da fatura"),
            size = "l", 
            fluidRow(
                align = "center", 
                fileInput("nu_file", "Sua fatura do Nubank no formato .csv", accept = "csv", buttonLabel = "File", placeholder = "nubank-yyyy-mm.csv", width = "50%"),
                shinyjs::hidden(progressBar("nu_file_barra_progresso", 0, display_pct = TRUE, striped = TRUE, unit_mark = "")),
                shinyjs::hidden(div(id = "file_nome_invalido", h1("Nome do arquivo inválido!"))),
                shinyjs::hidden(div(id = "file_invalido", h1("Este arquivo não parece uma fatura do Nubank."))),
                shinyjs::hidden(div(id = "fatura_adicionada", h1("Essa fatura já foi adicionada!!"))),
                shinyjs::hidden(div(id = "fatura_adicionada2", h1("Fatura adicionada com sucesso!!")))
            ),
            footer = actionBttn("nu_add_close", "Fechar", style = "minimal", color = "royal")
            
        ))
        
    })

    observeEvent(input$nu_add_close, {
        shinyjs::reset("nu_file")
        shinyjs::show("nu_file")
        shinyjs::hide("fatura_adicionada")
        shinyjs::hide("fatura_adicionada2")
        shinyjs::hide("file_nome_invalido")
        shinyjs::hide("file_invalido")
        
        removeModal()
    })
    
    observe({
        req(input$nu_file)

        cat <- readNu("cat")

        ym_file <- readNu(3) %>% pull(ym_file)
        
        shinyjs::hide("nu_file")
        
        inFile <- input$nu_file

        ## Verifica se o arquivo é válido
        x <- try(read.csv(inFile$datapath), silent = TRUE)
        if ("try-error" %in% class(x)) {
            file_ <- TRUE 
        } else {
            file_ <- !all(c("date", "category", "title", "amount") %in% names(x))
        }
        
        if (length(ym_file) > 0 && str_extract(inFile$name, "[0-9]{4}-[0-9]{2}") %in% ym_file) {
            shinyjs::show("fatura_adicionada")
        } else if (str_detect(inFile$name,"20(1|2)[0-9]{1}-((0[1-9]{1})|(1[012]){1})", TRUE)) {
            shinyjs::show("file_nome_invalido")
        } else if (file_) {
            shinyjs::show("file_invalido")
        } else {
            shinyjs::show("nu_file_barra_progresso")
            
            x <- read.csv(inFile$datapath, stringsAsFactors = FALSE) %>%
                mutate(
                    tot_par = str_extract(title, "(?<=/)[0-9]{1,2}"), 
                    par = str_extract(title, "[0-9]{1,2}(?=/)"),
                    date = ymd(date),
                    ym = ymd(str_c(format(date, "%Y-%m"), "-01")),
                    ym_file = str_extract(inFile$name, "[0-9]{4}-[0-9]{2}"), 
                    title = trimws(str_replace(title, "[0-9]{1,2}/[0-9]{1,2}", ""))
                ) %>%
                rename(ymd = date)

            ## Verifica quais observações já foram classificadas
            if (!is.null(cat)) {
                x <- x %>%
                    left_join(cat, by = "title")
            } else {
                x <- x %>%
                    mutate(category2 = NA, tags = NA)
            }

            shinyjs::disable("nu_add_close")
            for (i in 1:nrow(x)) {
                updateProgressBar(
                    session = session,
                    id = "nu_file_barra_progresso",
                    value = i, total = nrow(x),
                    title = ""
                )
                Sys.sleep(0.1)
            }
            
            ## Update tabela nu com novas informações
            x %>%
                writeNu(type = "nu") %>%
                invisible()

            shinyjs::enable("nu_add_close")
            shinyjs::hide("nu_file_barra_progresso")
            shinyjs::show("fatura_adicionada2")
        }
    })

    ## =========================================================================
    ## Classifica
    ## =========================================================================
    observeEvent(input$nu_classifica, {
        aux$class <- readNu(2)
        aux$cat <- readNu(4)
    }, priority = 10)

    observeEvent(input$nu_classifica, {
        if (nrow(aux$class) > 0) {
            showModal(modalDialog(
                title =  span(style = "color: #bd2df5;", "Classificação"),
                footer = actionBttn("nu_classifica_close", "Fechar", style = "minimal", color = "royal"),
                size = "l",
                fluidPage(
                    fluidRow(
                        column(9, h3(style = "color:black;", "Nome da Compra: ", span(style = "color: #bd2df5;", aux$class[1, ]$title))), 
                        column(3, align = "right", h3(style = "color:black;", "Valor: ", span(style = "color: #bd2df5", aux$class[1, ]$amount)))
                    ),
                    
                    fluidRow(
                        column(5, h4(style = "color:black;", "Categoria Nubank: ", span(style = "color: #bd2df5", aux$class[1, ]$category))),
                        column(2, align = "center", h4(style = "color: black;", "Parcela: ",
                                                       span(style = "color: #bd2df5", ifelse(is.na(aux$class[1, ]$tot_par), "NA", paste0(aux$class[1, ]$par, "/", aux$class[1, ]$tot_par))))), 
                        column(5, align = "right", h4(style = "color:black;", "Data da compra: ", span(style = "color: #bd2df5", aux$class[1, ]$ymd)))
                    ),
                    hr(), 
                    
                    fluidRow(
                        column(4,
                               
                               selectizeInput(
                                   inputId = "add_category",
                                   label = "Categoria", 
                                   choices = unique(aux$cat$category2),
                                   options = list(
                                       create = TRUE
                                   )
                               )
                               
                               ),
                        column(8,
                               
                               selectizeInput(
                                   inputId = "add_tags",
                                   label = "Tags", 
                                   choices = unique(do.call(c,strsplit(aux$cat$tags,","))),
                                   width = "95%",
                                   selected = "", 
                                   multiple = TRUE, 
                                   options = list(
                                       create = TRUE
                                   )
                               )
                               
                               )
                    )
                )
            ))
        } else {
            showModal(modalDialog(
                title = "Sem itens para classificar",
                footer = NULL,
                size = "s",
                easyClose = TRUE
            ))
        }
        
        
    }, priority = 0)

    observeEvent(input$classifica, {
        ## Att banco de dados
    })

    observeEvent(input$nu_classifica_close,  {
        aux$cat <- NULL
        removeModal()
    })
    
    # observeEvent(input$nexti, {
    #     new_cat <- tibble(title = aux$new_nu[1, ]$title,
    #                       category2 = ifelse(is.null(input$add_category), NA, input$add_category),
    #                       tags = ifelse(is.null(input$add_tags), NA, paste0(input$add_tags, collapse = ","))) %>%
    #         mutate(category2 = tolower(category2),
    #                tags = tolower(tags))
    # 
    #     ## add na base dados
    #     writeNu(new_cat, type = "cat")
    # 
    #     ## Junta com o já carregado
    #     aux$cat <- aux$cat %>%
    #         bind_rows(new_cat)
    # 
    #     ## classifica todas as compras iguais
    #     ## Todos estão desclassificados aqui...
    #     aux$new_nu <- aux$new_nu %>%
    #         select(-category2, -tags) %>% 
    #         left_join(new_cat, by = "title")
    # 
    #     ## Apenda novas informações
    #     aux$new_nu %>%
    #         filter(!is.na(category2)) %>% 
    #         writeNu(x = ., type = "nu") 
    # 
    #     ## Sem categoria
    #     aux$new_nu <- aux$new_nu %>%
    #         filter(is.na(category2))
    # 
    # 
    # })
    

    ## =========================================================================
    ## Historico
    ## =========================================================================
    observeEvent(input$nu_historico, {
        showModal(modalDialog(
            title = span(style = "color: #bd2df5;", "Meus Dados"),
            size = "l",
            easyClose = TRUE,
            footer = NULL,
            fluidPage(
                tabBox(
                    width = 12, 
                    tabPanel("Histórico", dataTableOutput("history_tb")), 
                    tabPanel("Classificação", dataTableOutput("classifica_tb"))
                )
            )
            
        ))
    })
    
    output$history_tb <- renderDT({
        input$nu_history
        x <- readNu(1)
        datatable(x)
    })

    output$classifica_tb <- renderDT({
        input$nu_history
        x <- readNu(4)
        datatable(x)
    })
    
    
})
