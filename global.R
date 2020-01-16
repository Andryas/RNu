pacman::p_load(shiny, shinyjs, shinydashboard, shinydashboardPlus, shinyWidgets, shinyBS, dplyr, stringr, lubridate, RSQLite, DT)

## =============================================================================
## This runs once to create the database
## =============================================================================
con <- dbConnect(RSQLite::SQLite(), "www/Nubank.db")
tbs <- dbListTables(con)
if (length(tbs) == 0) {
    query_nu <- "CREATE
    TABLE
        nu (
            ymd VARCHAR( 10 ) NOT NULL
            ,ym VARCHAR( 10 ) NOT NULL
            ,ym_file VARCHAR( 10 ) NOT NULL
            ,title TEXT NOT NULL
            ,category VARCHAR (255)
            ,category2 VARCHAR (255)
            ,tags VARCHAR (255)
            ,tot_par TINYINT
            ,par TINYINT
            ,amount FLOAT NOT NULL
        );"

    out <- dbSendQuery(con, query_nu)
    dbClearResult(out)
}
dbDisconnect(con)

## =============================================================================
## Funções Nubank
## =============================================================================
readNu <- function(query = 1) {
    con <- dbConnect(RSQLite::SQLite(),  "www/Nubank.db")

    query <- switch(query,
                    "1" =  NULL, 
                    "2" = "SELECT * FROM nu WHERE category2 IS NULL;", 
                    "3" = "SELECT  ym_file, count(ym_file) as n FROM nu GROUP BY ym_file;",
                    "4" = "SELECT DISTINCT title, category2, tags FROM nu WHERE category2 IS NOT NULL;"
                    )

    if (is.null(query)) {
        x <-  dbReadTable(con, "nu")
    } else {
        out <- dbSendQuery(con, query)
        x <- fetch(out, n = -1)
        invisible(dbHasCompleted(out))
        dbClearResult(out)
    }

    dbDisconnect(con)
    return(x)
}

writeNu <- function(x, type = "nu") {
    con <- dbConnect(RSQLite::SQLite(),  "www/Nubank.db")

    if (type == "nu") {
        x <- x %>%
            mutate(ymd = as.character(ymd), ym = as.character(ym))
    }
    
    x <- try(switch(type,
                    "nu" = dbWriteTable(con, "nu", x, row.names = FALSE, append = TRUE, overwrite = FALSE),
                    "cat" = dbWriteTable(con, "cat", x, row.names = FALSE, append = TRUE, overwrite = FALSE),
                    "Please enter one of the three options: nu or cat"
                    ), silent = TRUE)

    if ("try-error" %in% class(x)) {
        dbDisconnect(con)
        x <- as.character(x)
        if (grepl("UNIQUE constraint failed", x)) {
            return(NULL)
        }
    }
    
    dbDisconnect(con)

    return(x)
}

updateUiClassifica <- function(class, cat) {
    fluidPage(
        fluidRow(
            column(9, h3(style = "color:black;", "Nome da Compra: ", span(style = "color: #bd2df5;", class$title))), 
            column(3, align = "right", h3(style = "color:black;", "Valor: ", span(style = "color: #bd2df5", class$amount)))
        ),
        
        fluidRow(
            column(5, h4(style = "color:black;", "Categoria Nubank: ", span(style = "color: #bd2df5", class$category))),
            column(2, align = "center", h4(style = "color: black;", "Parcela: ",
                                           span(style = "color: #bd2df5", ifelse(is.na(class$tot_par), "NA", paste0(class$par, "/", class$tot_par))))), 
            column(5, align = "right", h4(style = "color:black;", "Data da compra: ", span(style = "color: #bd2df5", class$ymd)))
        ),
        hr(), 
        
        fluidRow(
            column(4,
                   
                   selectizeInput(
                       inputId = "add_category",
                       label = "Categoria", 
                       choices = unique(cat$category2),
                       options = list(
                           create = TRUE
                       )
                   )
                   
                   ),
            column(8,
                   
                   selectizeInput(
                       inputId = "add_tags",
                       label = "Tags", 
                       choices = unique(do.call(c,strsplit(cat$tags,","))),
                       width = "95%",
                       selected = "", 
                       multiple = TRUE, 
                       options = list(
                           create = TRUE
                       )
                   )
                   
                   )
        ),
        fluidRow(
            align = "center", 
            actionBttn(
                inputId = "nu_classifica_item",
                label = "Classifica", 
                style = "minimal",
                color = "royal"
            )
        )
    )
    
}
