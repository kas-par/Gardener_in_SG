#
#----------Setup--------
library(shiny)
library(serial)
library(tidyverse)
library(rvest)
library(RSQLite)
library(shinyTime)
library(shinythemes)
library(shinyalert)

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


# Connect to the database - Paths + Tables -----
sqlitePath <- "rain_database.db"
Table <- "Rain_Data"


# Definition of Constants ----

already_watered <<- FALSE
heizung_bool = FALSE
print(paste("Heizungsbool: ", heizung_bool))

initial_settings_update_hours = FALSE
initial_settings_update_length <<- FALSE
initial_settings_update_freq = FALSE

db_timeout = 300000

