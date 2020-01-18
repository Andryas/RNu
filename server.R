shinyServer(function(input, output, session) {
    aux <- reactiveValues(class = NULL, cat = NULL)

    ## =========================================================================
    ## Add Nova Informação
    ## =========================================================================
    observeEvent(input$nu_add, {
        showModal(modalDialog(
            title = span(style = "color: #bd2df5;",
                         "Nubank - Informações da fatura"),
            size = "l",
            fluidRow(
                align = "center",
                fileInput(
                    "nu_file",
                    "Sua fatura do Nubank no formato .csv",
                    accept = "csv",
                    buttonLabel = "File",
                    placeholder = "nubank-yyyy-mm.csv",
                    width = "50%"
                ),
                shinyjs::hidden(
                    progressBar(
                        "nu_file_barra_progresso",
                        0,
                        display_pct = TRUE,
                        striped = TRUE,
                        unit_mark = ""
                    )
                ),
                shinyjs::hidden(div(
                    id = "file_nome_invalido",
                    h1("Nome do arquivo inválido!")
                )),
                shinyjs::hidden(div(
                    id = "file_invalido",
                    h1("Este arquivo não parece uma fatura do Nubank.")
                )),
                shinyjs::hidden(div(
                    id = "fatura_adicionada1",
                    h1("Fatura já adicionada!!")
                )),
                shinyjs::hidden(div(
                    id = "fatura_adicionada2",
                    h1("Fatura adicionada com sucesso!!")
                ))
            ),
            footer = actionBttn(
                "nu_add_close",
                "Fechar",
                style = "minimal",
                color = "royal"
            )

        ))

    })

    observeEvent(input$nu_add_close, {
        shinyjs::reset("nu_file")
        shinyjs::show("nu_file")

        ## TODO put a javascript here...
        shinyjs::hide("fatura_adicionada")
        shinyjs::hide("fatura_adicionada2")
        shinyjs::hide("file_nome_invalido")
        shinyjs::hide("file_invalido")

        removeModal()
    })

    observe({
        req(input$nu_file)

        cat <- readNu(3)

        shinyjs::hide("nu_file")

        inFile <- input$nu_file

        ## Verifica se o arquivo é válido
        x <- try(read.csv(inFile$datapath), silent = TRUE)
        if ("try-error" %in% class(x)) {
            file_ <- TRUE
        } else {
            file_ <- !all(c("date", "category", "title", "amount") %in% names(x))
        }

        if (file_) {
            ## Se der erro na leitura...
            shinyjs::show("file_invalido")
        } else if (str_detect(inFile$name,
                              "20(1|2)[0-9]{1}-((0[1-9]{1})|(1[012]){1})",
                              TRUE)) {
            ## Se o nome do arquivo estiver escrito errado...
            shinyjs::show("file_nome_invalido")
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
                rename(ymd = date) %>%
                group_by_at(.vars = vars(-amount)) %>%
                summarise(amount = sum(amount)) %>%
                mutate(id = str_c(str_replace_all(ymd, "-", ""),
                                  str_replace_all(tolower(title), " ", ""),
                                  category,
                                  ifelse(is.na(tot_par), "", "parcelado"),
                                  collapse = ""),
                       id = str_replace_all(id, "[[:punct:]]", "")
                ) %>%
                ungroup()

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
                    value = i,
                    total = nrow(x),
                    title = ""
                )
            }

            ## Update tabela nu com novas informações
            status <- writeNu(x)


            shinyjs::enable("nu_add_close")
            shinyjs::hide("nu_file_barra_progresso")
            if (status == 404) {
                shinyjs::show("fatura_adicionada1")
            } else {
                shinyjs::show("fatura_adicionada2")
            }
        }
    })

    ## =========================================================================
    ## Classifica
    ## =========================================================================
    observeEvent(input$nu_classifica,  {
        aux$class <- readNu(2)
        aux$cat <- readNu(4)
        if (nrow(aux$class) > 0) {
            output$ui_classifica <-
                renderUI({
                    updateUIClassifica(aux$class[1,], aux$cat)
                })
        } else {
            output$ui_classifica <- renderUI({
                fluidPage(column(
                    12,
                    align = "center",
                    h3(
                        style = "color:black;",
                        "Você já classificou todas suas desespesas!"
                    )
                ))
            })
        }
    }, priority = 10)

    observeEvent(input$nu_classifica, {
        showModal(modalDialog(
            title =  span(style = "color: #bd2df5;", "Classificação"),
            footer = actionBttn(
                "nu_classifica_close",
                "Fechar",
                style = "minimal",
                color = "royal"
            ),
            size = "l",
            uiOutput("ui_classifica")
        ))
    }, priority = 0)

    observeEvent(input$nu_classifica_item, {
        class_new <- aux$class[1,]
        class_new$category2 <- input$add_category
        class_new$tags <- paste0(input$add_tags, collapse = ",")

        aux$class <- aux$class %>%
            select(-category2, -tags) %>%
            left_join(class_new %>% select(title, category2, tags), by = "title")

        aux$class %>%
            filter(!is.na(category2)) %>%
            writeNu(type = "nu")

        aux$class <- aux$class %>%
            filter(is.na(category2))

        aux$cat <- readNu(4)

        if (nrow(aux$class) > 0) {
            output$ui_classifica <- renderUI({
                updateUiClassifica(aux$class[1,], aux$cat)
            })
        } else {
            output$ui_classifica <- renderUI({
                fluidPage(column(
                    12,
                    align = "center",
                    h3(
                        style = "color:black;",
                        "Você classificou todas suas desespesas!"
                    )
                ))
            })
        }
    })


    observeEvent(input$nu_classifica_close,  {
        removeModal()
    })

    ## =========================================================================
    ## Historico
    ## =========================================================================
    observeEvent(input$nu_historico, {
        showModal(
            modalDialog(
                title = span(style = "color: #bd2df5;", "Meus Dados"),
                size = "l",
                easyClose = TRUE,
                footer = NULL,
                fluidPage(tabBox(
                    width = 12,
                    tabPanel("Histórico", dataTableOutput("history_tb")),
                    tabPanel("Classificação", dataTableOutput("classifica_tb"))
                ))

            )
        )
    })

    output$history_tb <- renderDT({
        input$nu_historico
        x <- readNu(1)
        datatable(x)
    })

    output$classifica_tb <- renderDT({
        input$nu_historico
        x <- readNu(4)
        datatable(x)
    })


})
