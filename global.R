pacman::p_load(shiny, shinyjs, shinydashboard, shinydashboardPlus, shinyWidgets, shinyBS, dplyr, stringr, lubridate, RSQLite, DT)

?progressBar

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

    query_cat <- "CREATE
    TABLE
        cat (
            title TEXT NOT NULL
            ,category2 VARCHAR (255)
            ,tags VARCHAR (255)
            ,PRIMARY KEY (title)
        );"
    
    out <- dbSendQuery(con, query_nu)
    dbClearResult(out)
    
    out <- dbSendQuery(con, query_cat)
    dbClearResult(out)
    
}
dbDisconnect(con)

## =============================================================================
## Nubank functions
## =============================================================================

readNu <- function(type = "nu") {
    con <- dbConnect(RSQLite::SQLite(),  "www/Nubank.db")
    
    x <- dbListTables(con)

    x <- switch(type,
                "nu" = dbReadTable(con, "nu"),
                "cat" = dbReadTable(con, "cat"),
                "Please enter one of the three options: nu or cat"
                )

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

FaturasAdded <- function() {
    con <- dbConnect(RSQLite::SQLite(),  "www/Nubank.db")

    out <- dbSendQuery(con, "SELECT DISTINCT ym_file FROM nu;")
    data <- fetch(out, n = -1)
    invisible(dbHasCompleted(out))
    dbClearResult(out)
    dbDisconnect(con)
    return(data$ym_file)
}


## =============================================================================
## Modules
## =============================================================================

## =============================================================================
## Extra functions
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
