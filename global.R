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

    # query_cat <- "CREATE
    # TABLE
    #     cat (
    #         title TEXT NOT NULL
    #         ,category2 VARCHAR (255)
    #         ,tags VARCHAR (255)
    #         ,PRIMARY KEY (title)
    #     );"
    
    out <- dbSendQuery(con, query_nu)
    dbClearResult(out)
    
    # out <- dbSendQuery(con, query_cat)
    # dbClearResult(out)
}
dbDisconnect(con)

## =============================================================================
## Funções Nubank
## =============================================================================
# query
# 1: NOMRAL
# 2: CLASSIFICA
# 3: ARQUIVOS JA AMRAZENADOS
# 4: Tras as categorias
readNu <- function(query = 1) {
    con <- dbConnect(RSQLite::SQLite(),  "www/Nubank.db")

    query <- switch(query,
           "1" =  NULL, 
           "2" = "SELECT title, category, ymd, tot_par, par, amount FROM nu WHERE category2 IS NULL;", 
           "3" = "SELECT DISTINCT ym_file FROM nu;",
           "4" = "SELECT title, category2, tags FROM nu WHERE category2 IS NOT NULL;"
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
            mutate(ymd = as.character(ymd),
                   ym = as.character(ym))
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



FaturasAdicionadas <- function() {
    con <- dbConnect(RSQLite::SQLite(),  "www/Nubank.db")

    out <- dbSendQuery(con, )
    data <- fetch(out, n = -1)
    invisible(dbHasCompleted(out))
    dbClearResult(out)
    dbDisconnect(con)
    return(data$ym_file)
}


## =============================================================================
## Modulos
## =============================================================================

## =============================================================================
## Funções extras
## =============================================================================


# x <- read.csv("www/nubank-2019-10.csv", stringsAsFactors = FALSE) %>%
#     mutate(
#         tot_par = str_extract(title, "(?<=/)[0-9]{1,2}"), 
#         par = str_extract(title, "[0-9]{1,2}(?=/)"),
#         date = ymd(date),
#         ym = ymd(str_c(format(date, "%Y-%m"), "-01")),
#         ym_file = str_extract("www/nubank-2019-10.csv", "[0-9]{4}-[0-9]{2}"), 
#         title = trimws(str_replace(title, "[0-9]{1,2}/[0-9]{1,2}", ""))
#     ) %>%
#     rename(ymd = date)
# x$category2 <- "teste"
# x$tags <- "teste,teste2"
# readNu()
# writeNu(x)
