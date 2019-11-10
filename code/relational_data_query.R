library(RMariaDB)
library(odbc)
con <- DBI::dbConnect(drv=RMariaDB::MariaDB(), 
                      user="guest", 
                      password="relational", 
                      host="relational.fit.cvut.cz", 
                      port="3306", 
                      dbname="ccs")

dfList <- dbListTables(con)

for (i in 1:length(dfList)){
  rs <- odbc::dbSendQuery(con,
                          paste0("SELECT * FROM ", "`", dfList[i], "`"))
  while (!odbc::dbHasCompleted(rs)) {
    assign(dfList[i], odbc::dbFetch(rs))
  }
  odbc::dbClearResult(rs)
}