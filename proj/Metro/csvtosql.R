library(DBI)
library(RSQLite)

con <- dbConnect(SQLite(), "metro.sqlite")
datos <- read.csv("M4_Tramos.csv", quote="")
dbWriteTable(con, "M4_TRAMOS", datos, overwrite = TRUE)
dbDisconnect(con)