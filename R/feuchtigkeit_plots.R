


feuchtigkeit_plot <- function (db, sqlitePath, Table, var) {
  
  #Auslesen der letzten 200 Werte aus der Rain_Data Tabelle der Datenbank für Plot
  db <- dbConnect(RSQLite::SQLite(), sqlitePath, synchronous) #establish connection
  
  
  
  t_feuchtigkeit <- tbl(db, from = Table) %>% 
    select(!!ensym(var), zeit, n) %>% 
    arrange(n) %>% 
    head(10000) %>% 
    collect()
  
  dbDisconnect(db)
  
  n2 = seq(from = 1, to = nrow(t_feuchtigkeit), by = 1 ) #to ensure that only part of the data is ploted, otherwise raspberry takes so long
  #to render the plot that app is constantly frozen
  t_feuchtigkeit = t_feuchtigkeit[n2,]
  mydates = structure(t_feuchtigkeit$zeit,class=c('POSIXt','POSIXct'))
  upper_bound = quantile(t_feuchtigkeit[,var], 0.95, na.rm =  T)
  index <- t_feuchtigkeit[,var] >= upper_bound
  t_feuchtigkeit$var[index] <- upper_bound
  
  #Plot erstellen
  ggplot_output <- ggplot(t_feuchtigkeit, aes(x = mydates, y = !!ensym(var)))+
    geom_point()+
    geom_smooth(na.rm = TRUE, se = FALSE) +
    xlab("Zeit")+
    ylab("Feuchtigkeitswert in %")
  
  return (ggplot_output)
  
  
}
