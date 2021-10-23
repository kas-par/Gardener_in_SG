
sqlitePath = "rain_database.db"


db <- dbConnect(RSQLite::SQLite(), sqlitePath) #establish connection
temp_table = try(dbGetQuery(db, "SELECT * FROM Rain_Data"))
dbDisconnect(db)

if (class(temp_table) == "try-error"){
  db <- dbConnect(RSQLite::SQLite(), sqlitePath) #establish connection
  print("first time database created -> start counting from 1 on")
  feuchtigkeit_table = tibble(n = NA, bodenF = NA, bodenF_Ende = NA, 
                              luftF = NA, luftT = NA, zeit = NA, wasserstand = NA)
  
  dbWriteTable(db, "Rain_Data", feuchtigkeit_table)
  print("Table initialisiert")
  dbDisconnect(db)
  
} else {
  print("Nummerierung Start:")
  print(temp_table[nrow(temp_table),1])
  feuchtigkeit_table = as_tibble(temp_table[nrow(temp_table),])
}

i = 1
while (i < 200){

db <- dbConnect(RSQLite::SQLite(), sqlitePath) #establish connection
#neue Messungen in Tibble und SQL-Database einfügen
last_value <<- feuchtigkeit_table$n[nrow(feuchtigkeit_table)]+1
feuchtigkeit_table$zeit  = structure(feuchtigkeit_table$zeit,class=c('POSIXct','POSIXt'))

feuchtigkeit_table <<- add_row(feuchtigkeit_table, 
                               bodenF = sample(1:20,1), bodenF_Ende = sample(1:20,1), 
                               n = last_value, luftF = sample(1:20,1), luftT = sample(1:20,1), zeit = Sys.time(), wasserstand = sample(1:20,1))

dbWriteTable(db, "Rain_Data", feuchtigkeit_table[nrow(feuchtigkeit_table),], append = TRUE) #zu Tabelle "Rain_Data"
#hinzufügen (nur neuster Eintrag)
feuchtigkeit_table = feuchtigkeit_table[nrow(feuchtigkeit_table):nrow(feuchtigkeit_table),] #behalte nur den
#neusten Eintrag für die Ausgabe, Rest in DB gespeichert
#print(dbListTables(db))
dbDisconnect(db)
i = i+1
print(i)
}

