#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#----------Setup--------
library(shiny)
library(serial)
library(tidyverse)
library(rvest)
library(RSQLite)
library(shinyTime)

options(shiny.sanitize.errors = FALSE)

source("R/feuchtigkeit_plots.R")
source("R/get_weather_data.R")
source("R/text_update.R")

# port = system("ls /dev/ttyUSB*", intern = TRUE)
# port = substring(port, 6)
# 
# #Initialise the Connection to Arduino
# con <- serialConnection(name = "get_temps",
#                         port = port, #ttyUSB1 ; cu.usbserial-14130 cu.usbmodem1432201
#                         mode = "9600,n,8,1",
#                         buffering = "none",
#                         newline = 1)
# open(con)


# Connect to the database
sqlitePath <- "rain_database.db"
Table <- "Rain_Data"


already_watered <<- FALSE
heizung_bool = FALSE
print(paste("Heizungsbool: ", heizung_bool))

initial_settings_update_hours = FALSE
initial_settings_update_length <<- FALSE
initial_settings_update_freq = FALSE

db_timeout = 300000


#-------Shiny Function--------

shinyServer(function(input, output, session) {
    bool_settings <<- FALSE
  
    ##Timer für Unterbrüche in Ausführungszyklus ====
    autoInvalidateUpdateDB_Text <- reactiveTimer(10000)
    autoInvalidateUpdateDB_Plot <- reactiveTimer(30000)
    autoInvalidateWater <- reactiveTimer(2000)
    autoInvalidateCheckSettings <- reactiveTimer(1000)
    
    
    
    ## Erstellen des Feuchtigkeitplots ANFANG ====
    
    output$FeuchtigkeitsPlot <- renderPlot({
      
        #Timer um die Regelmässigkeit des Plots zu steuern
      autoInvalidateUpdateDB_Plot()
      
      if (input$variable == "Anfang") {
        
        feuchtigkeit_plot(db, sqlitePath = sqlitePath, Table = Table, var = "bodenF")
        
      } else {
        
        feuchtigkeit_plot(db, sqlitePath = sqlitePath, Table = Table, var = "bodenF_Ende")
        
      }
      
    }) #end Plot Feuchtigkeit
    
    
    # Text Outputs ----
    
    observe ({
      
      autoInvalidateUpdateDB_Text()
      
      t_update_text <- get_text_update(db, sqlitePath = sqlitePath, Table= Table)
      
      #Textoutput Feuchtigkeit
      output$aktuelleFeuchtigkeitText = renderText({
        aktuell_bodenF <- t_update_text$bodenF
      })
      
      #Textoutput Feuchtigkeit - ENDE
      output$aktuelleFeuchtigkeitText_Ende = renderText({
        aktuell_bodenF <- t_update_text$bodenF_Ende
      })
      
      #Textoutput Feuchtigkeit - Luft
      output$aktuelleHumText = renderText({
        aktuell_luftF <- t_update_text$luftF
      })
      
      #Textoutput Lufttemperatur
      output$aktuelleLuftText = renderText({
        aktuell_luftT <- t_update_text$luftT
      })
      
      #Textoutput Wasserstand
      output$aktueller_wasserstand = renderText({
        aktuell_wasserstand <- t_update_text$wasserstand
        
        if (aktuell_wasserstand == 0) {
          wasser = "Vorhanden"
        } else {
          wasser = "Kein Wasser vorhanden!"
        }
        
      })
      
    })

    
    
    #-----Scrape the Weather Data from srf Meteo ------
    scraping_weather = reactive({
      
      regenmenge = get_weather_data()
      
    })
    
    #Output of the Weather data scraped above
    output$weather_today = renderText({
        autoInvalidateFeuchtigkeitWeather()
        weather = isolate(scraping_weather())
        paste("Heutige Regenmenge: ", weather[1], "mm")
       
    }
    )
    
    #Output of the Weather data scraped above
    output$weather_tomorrow = renderText({
      autoInvalidateFeuchtigkeitWeather()
      weather = isolate(scraping_weather())
      paste("Morgige Regenmenge: ", weather[2], "mm")
      
    }
    )
    
    #-----Button Bewässerung  geklickt-----
    observeEvent(input$start_button_manual, {
        # Show a modal when the button is pressed
        showModal(modalDialog(
            title = "Die manuelle Bewässerung wurde gestartet",
            easyClose = TRUE,
            footer =modalButton("Fertig"),
            fade = TRUE,
            
        ))

      #-----Kommunikation zu Arduino------
      
      #Einstellungen der Dauer der Bewässerung auslesen + String für Kommunikation erstellen
      time_to_water = input$input_manuell_length_watering
      print("Kommunkation gestartet")
      time_to_water = as.numeric(time_to_water)
      write.serialConnection(con, time_to_water)

    })
    
    
    #-----Button Bewässerung  geklickt-----
    observeEvent(input$stop_button_manual, {
      # Show a modal when the button is pressed
      showModal(modalDialog(
        title = "Die manuelle Bewässerung wurde gestoppt",
        easyClose = TRUE,
        footer =modalButton("Fertig"),
        fade = TRUE,
        
      ))
      
      #-----Kommunikation zu Arduino------
      
      #Einstellungen der Dauer der Bewässerung auslesen + String für Kommunikation erstellen
      write.serialConnection(con, "off")
      
      
    })
    
    
    
    
    
    #----- Ausgewählte Pflanzen tränken -----
    
    observeEvent(input$start_button_manual_selected, {
      # Show a modal when the button is pressed
      pflanzen = input$Pflanzen_Auswahl
      pflanzen_print = ""
      for (x in pflanzen) {
        print(x)
        pflanzen_print =  paste(pflanzen_print, x, sep = ", ")
        
      }
      
      pflanzen_print = substring(pflanzen_print,2)
      
      if ("Kräuter" %in% pflanzen) {
        krauter = 1
      } else {
        krauter = 0
      }
      
      if ("Rosenbeet" %in% pflanzen){
        rosen = 1
      } else {
        rosen = 0
      }
      if ("Blumentöpfe" %in% pflanzen){
        blumen = 1
      } else {
        blumen = 0
      }
      
      if (pflanzen_print != "") {
        
      #Testen ob überhaupt eine Pflanze ausgewählt ist  
      showModal(modalDialog(
        title = "Die manuelle Bewässerung wurde gestartet",
        paste("Folgende Pflanzen werden getränkt: ", pflanzen_print),
        easyClose = TRUE,
        footer =modalButton("Fertig"),
        fade = TRUE,
        
      ))
        
        
        } else {
        
        showModal(modalDialog(
          title = "Keine Pflanzen ausgewählt!",
          "Bitte aus zuerst die gewünschten Pflanzen für die manuelle Bewässerung auswählen.",
          easyClose = TRUE,
          footer =modalButton("Fertig"),
          fade = TRUE,
          
        ))
        
      }
      
      
      #-----Kommunikation zu Arduino------
      
      #Einstellungen der Dauer der Bewässerung auslesen + String für Kommunikation erstellen
      time_to_water = input$input_manuell_length_watering
      print("Kommunkation gestartet")
      print(time_to_water)
      time_to_water = time_to_water*60 #calculate the time in seconds for the sys.sleep command
      
    })
    

 
    
    #-------Timer Management for Watering ---------
    
    observe({
      
      autoInvalidateWateringStart()
      time_to_fire = input$hours_watering
      frequency_watering = input$frequency
      current_hour =  as.numeric(format(Sys.time(), "%H"))
      
      if (current_hour == time_to_fire & already_watered == FALSE) {
        
        print("Es geht los, die Stunde hat geschlagen")
        already_watered <<- TRUE
        
        #Communication to Arduino
        time_to_water = input$input_manuell_length_watering
        write.serialConnection(con, "on1")
        time_to_water = time_to_water*60 #calculate the time in seconds for the sys.sleep command
        #Sys.sleep(time_to_water)
        write.serialConnection(con, "off1")
        
      } #end IF current hour to water
      
    }) #end of observe element
    
    
    #reset the already_watered bool each day
    observe ({
      if (input$frequency == "Täglich") {
        autoInvalidateWateringBoolDaily()
        already_watered <<- FALSE
        
      } else {
        
        autoInvalidateWateringBoolTwoDaily()
        already_watered <<- FALSE
      }

      
    })
    
 
    #Render the Text
    output$system_settings = renderText({
      autoInvalidateFeuchtigkeitText()
      
      frequency_watering = input$frequency
      
      if (frequency_watering == "Alle zwei Tage") {
        frequency_watering = "alle zwei Tage"
      } 
      
      if (frequency_watering == "Täglich") {
        frequency_watering = "täglich"
      }
      hour = input$hours_watering
      paste("Die Bewässerung findet ", frequency_watering, "um", hour, " Uhr statt")
      
    }
    ) #end system_settings
    
    
    
    
    #--------Save the Settings for all Users in CSV File -------
    
    #each value (length, hour and frequency) needs to be checked if it has changed -> than save to csv file 
    
    observeEvent(input$hours_watering,{
      
      if (bool_settings == TRUE) {
        
       
        
        settings_hours_watering = input$hours_watering
        settings_frequency = input$frequency
        settings_length = input$input_manuell_length_watering
        
        settings_complete = tibble(frequency = settings_frequency, hours = settings_hours_watering, length = settings_length)
        
        write.csv(settings_complete, "settings.csv")
        print("changed")
        print(settings_complete)
        
      }
      
    })
    
  
    observeEvent(input$frequency,{
    
      if (bool_settings == TRUE) {
    
      settings_hours_watering = input$hours_watering
      settings_frequency = input$frequency
      settings_length = input$input_manuell_length_watering
      
      settings_complete = tibble(frequency = settings_frequency, hours = settings_hours_watering, length = settings_length)
      
      write.csv(settings_complete, "settings.csv")
      print("changed")
      print(settings_complete)
      
      }
      
    })
    
    observeEvent(input$input_manuell_length_watering,{
      
      if (bool_settings == TRUE) {
      
      settings_hours_watering = input$hours_watering
      settings_frequency = input$frequency
      settings_length = input$input_manuell_length_watering
      
      settings_complete = tibble(frequency = settings_frequency, hours = settings_hours_watering, length = settings_length)
      
      write.csv(settings_complete, "settings.csv")
      print("changed")
      print(settings_complete)
      
      }
      
    })
    
    
    observe({
      autoInvalidateCheckSettings()
      autoInvalidateCheckSettings <<- reactiveTimer(10000000)
      print("settings")
      paste("BOOL Settings: ", bool_settings)
     
        
        if (file.exists("settings.csv")==FALSE){
          first_entry = tibble(frequency = "Täglich", hours= 20, length = 5)
          write.csv(first_entry, "settings.csv")
          print("Writing CSV Settings")
          
        } else {
          #Shiny/St-Gallen-Version/
          print("reading Settings in")
          settings = read_csv("settings.csv")
          print(settings$hours)
          updateNumericInput(inputId =  "input_manuell_length_watering", value = settings$length)
          updateNumericInput(inputId =  "hours_watering", value = settings$hours)
          updateNumericInput(inputId =  "input_manuell_length_watering", value = settings$length)
          updateSelectInput(session, inputId = "frequency", label =  "Standortauswahl:", choices =
                              c("Täglich", "Alle zwei Tage"), selected = settings$frequency)
        }
        
        bool_settings <<- TRUE
      
      
    })
    
  
}) #end Server


