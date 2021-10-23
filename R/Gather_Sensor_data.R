library(serial)
library(tidyverse)
library(RSQLite)

con <- serialConnection(name = "get_temps",
                        port = "ttyUSB0",
                        mode = "9600,n,8,1",
                        buffering = "none",
                        newline = 1)
open(con)

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




while (TRUE) {
  
tmp = read.serialConnection(con)
#print(tmp)
tmp = str_split(tmp, ";") #split the outout of Arduino by ; delimiter
feuchtigkeit_val = as.numeric(tmp[[1]][length(tmp[[1]])-4]) #take only first value of the series as variable feuchtigkeit
feuchtigkeit_val_Ende = as.numeric(tmp[[1]][length(tmp[[1]])-3]) #take only first value of the series as variable feuchtigkeit
hum_val = as.numeric(tmp[[1]][length(tmp[[1]])-2])  #take last value, as sometimes several measurements are stored in tmp
air_temp_val = as.numeric(tmp[[1]][length(tmp[[1]])-1])
wasserstand_val = as.numeric(tmp[[1]][length(tmp[[1]])])
zeit_val = Sys.time()
zeit_val = as.POSIXct(zeit_val, tz = "CEST")

tryCatch({

if (is.na(feuchtigkeit_table$n[1])){
  db <- dbConnect(RSQLite::SQLite(), sqlitePath) #establish connection
  feuchtigkeit_table$n[1] <- 1
  feuchtigkeit_table$bodenF[1] <- feuchtigkeit_val
  feuchtigkeit_table$bodenF_Ende[1] <- feuchtigkeit_val_Ende
  feuchtigkeit_table$luftF[1] <- hum_val
  feuchtigkeit_table$luftT[1] <-  air_temp_val
  feuchtigkeit_table$wasserstand[1] <- wasserstand_val

  # last_value = 1
  # feuchtigkeit_table <<- add_case(feuchtigkeit_table, 
  #                                bodenF = feuchtigkeit_val, bodenF_Ende = feuchtigkeit_val_Ende, n = last_value, luftF = hum_val, luftT = air_temp_val)
  # feuchtigkeit_table <<- slice(feuchtigkeit_table, 2L)
  #print("IS NA")
  #print(feuchtigkeit_table)
  dbWriteTable(db, "Rain_Data", feuchtigkeit_table[nrow(feuchtigkeit_table),], append = FALSE, overwrite = TRUE) #zu Tabelle "Rain_Data"
  #print(dbListTables(db))
  print("First Exit")
  dbDisconnect(db)
  
  
} else {
  db <- dbConnect(RSQLite::SQLite(), sqlitePath) #establish connection
  #neue Messungen in Tibble und SQL-Database einfügen
  last_value <<- feuchtigkeit_table$n[nrow(feuchtigkeit_table)]+1
  feuchtigkeit_table$zeit  = structure(feuchtigkeit_table$zeit,class=c('POSIXct','POSIXt'))

  feuchtigkeit_table <<- add_row(feuchtigkeit_table, 
                                 bodenF = feuchtigkeit_val, bodenF_Ende = feuchtigkeit_val_Ende, 
                                 n = last_value, luftF = hum_val, luftT = air_temp_val, zeit = zeit_val, wasserstand = wasserstand_val)
  
  dbWriteTable(db, "Rain_Data", feuchtigkeit_table[nrow(feuchtigkeit_table),], append = TRUE) #zu Tabelle "Rain_Data"
  #hinzufügen (nur neuster Eintrag)
  feuchtigkeit_table = feuchtigkeit_table[nrow(feuchtigkeit_table):nrow(feuchtigkeit_table),] #behalte nur den
  #neusten Eintrag für die Ausgabe, Rest in DB gespeichert
  #print(dbListTables(db))
  dbDisconnect(db)
  print("second exit")
  #DB Log
  
}},error = function(e){cat("ERROR :", conditionMessage(e),"\n")})


#-------Create the mechanism to read in the automatic schedule of the watering from the csv file-------

if (file.exists("settings.csv")==FALSE){
  first_entry = tibble(frequency = "Täglich", hours= 20, length = 5)
  write.csv(first_entry, "settings.csv")
  print("Writing CSV Settings")
} else {
  settings = read_csv("settings.csv")
  print(settings$frequency)
  current_time = Sys.time()
  current_time = as.numeric(strftime(current_time, format="%H"))
  
  if (settings$hours == current_time) {
    print("Watering")
  }
}


Sys.sleep(5)


}
