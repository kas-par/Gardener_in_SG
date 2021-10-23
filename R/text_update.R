

get_text_update <- function (db, sqlitePath, Table) {
  
  #Auslesen der letzten 200 Werte aus der Rain_Data Tabelle der Datenbank für Plot
  db <- dbConnect(RSQLite::SQLite(), sqlitePath, synchronous) #establish connection
  
  t_update <- tbl(db, from = Table) %>% 
    select(-zeit) %>% 
    arrange(desc(n)) %>% 
    head(1) %>% 
    collect()
  
  dbDisconnect(db)
  
  return(t_update)
  
}
