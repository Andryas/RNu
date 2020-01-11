shinyServer(function(input, output, session) {

    ## =========================================================================
    ## add Nova Informação
    ## =========================================================================
    observeEvent(input$nu_add, {

        showModal(modalDialog(
            title = "Nubank - Informações da fatura",
            size = "l", 
            fluidRow(
                align = "center", 
                fileInput("nu_file", "Sua fatura do Nubank no formato .csv", accept = "csv", buttonLabel = "File", placeholder = "nubank-yyyy-mm.csv", width = "50%"),
                shinyjs::hidden(progressBar("nu_file_progress", 0, display_pct = TRUE, striped = TRUE, unit_mark = "")),
                shinyjs::hidden(div(id = "fatura_added", h1("Essa fatura já foi adicionada!!"))),
                shinyjs::hidden(div(id = "fatura_added2", h1("Fatura adicionada com sucesso!!")))
            ),
            footer = actionBttn("nu_add_close", "Fechar", style = "minimal", color = "royal")
            
        ))
        
    })

    observeEvent(input$nu_add_close, {
        removeModal()

        shinyjs::reset("nu_file")
        shinyjs::show("nu_file")
        shinyjs::hide("fatura_added")
        shinyjs::hide("fatura_added2")
        
    })
    
    observe({
        req(input$nu_file)

        ## Tras já classificados
        cat <- readNu("cat")

        ## Faturas já adicionadas
        ym_file <- FaturasAdded()
        
        shinyjs::hide("nu_file")
        
        inFile <- input$nu_file

        if (length(ym_file) > 0 && str_extract(inFile$name, "[0-9]{4}-[0-9]{2}") %in% ym_file) {
            shinyjs::show("fatura_added")
        } else if (str_detect(inFile$name,"20(1|2)[0-9]{1}-((0[1-9]{1})|(1[012]){1})", TRUE)) {
            ## nome do arquivo errado
        } else if ("try-error" %in% class(try(read.csv(inFile$datapath), silent = TRUE))) {
            ## arquivo invalido
        } else {
            shinyjs::show("nu_file_progress")
            
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
                    id = "nu_file_progress",
                    value = i, total = nrow(x),
                    title = ""
                )
                Sys.sleep(0.5)
            }
            
            ## Apenda novas informações classificadas a base e a cat
            x %>%
                writeNu(type = "nu") %>%
                invisible()

            x %>%
                select(title, category2, tags) %>%
                mutate(category2 = tolower(category2),
                       tags = tolower(tags)) %>% 
                writeNu(., type = "cat") %>%
                invisible()

            shinyjs::enable("nu_add_close")
            shinyjs::hide("nu_file_progress")
            shinyjs::show("fatura_added2")
        }

    })


    # observe({
    #     if (nrow(aux$new_nu) > 0) {
    #         output$classify <- renderUI({
    #             fluidRow(
    #                 style = "margin-left: 5%; margin-right: 5;", 
    #                 fluidRow(
    #                     column(9, h3("Nome da Compra: ", span(style = "color: purple;", aux$new_nu[1, ]$title))), 
    #                     column(3, h3("Valor: ", span(style = "color: purple", aux$new_nu[1, ]$amount)))
    #                 ),
    #                 
    #                 fluidRow(
    #                     column(6, h4("Categorização Nubank: ", span(style = "color: purple", aux$new_nu[1, ]$category))),
    #                     column(6, h4("Data da compra: ", span(style = "color: purple", aux$new_nu[1, ]$ymd)))
    #                 ),
    #                 
    #                 hr(), 
    #                 
    #                 fluidRow(
    #                     column(4,
    #                            
    #                            selectizeInput(
    #                                inputId = "add_category",
    #                                label = "Categoria", 
    #                                choices = unique(aux$cat$category2),
    #                                options = list(
    #                                    create = TRUE
    #                                )
    #                            )
    #                            
    #                            ),
    #                     column(8,
    #                            
    #                            selectizeInput(
    #                                inputId = "add_tags",
    #                                label = "Tags", 
    #                                choices = unique(do.call(c,strsplit(aux$cat$tags,","))),
    #                                width = "95%",
    #                                selected = "", 
    #                                multiple = TRUE, 
    #                                options = list(
    #                                    create = TRUE
    #                                )
    #                            )
    #                            
    #                            )
    #                 )
    #             )                
    #         })
    #         
    #     } else {
    #         shinyjs::hide("nexti")
    #         output$classify <- renderUI({
    #             fluidRow(h1(style = "color: purple; margin: 5%;",  "Todos gastos foram classificados."))
    #         })
    #     }
    #     
    # })

    ## 
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
    ## History & Classify
    ## =========================================================================
    output$history_tb <- renderDT({
        input$in_nu_history
        x <- readNu("nu")
        datatable(x)
    })

    output$classify_tb <- renderDT({
        input$in_nu_history
        x <- readNu("cat")
        datatable(x)
    })
    
    
})
