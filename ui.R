#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(
    
    #dividing into multiple pages (head navigation)
    navbarPage(
        
        "Automatisches Bewässerungssystem",
        # ---- CV Panel -----
        
            tabPanel("Bewässerung",

                    sidebarLayout(
                        
                        sidebarPanel(
                            # ---- CV Panel: Sidebar ----
                            #sidebar content goes here
                            h3("Aktuelle Messwerte"),
                            tags$br(),
                            textOutput("aktuelleFeuchtigkeitText"),
                            tags$br(),
                            textOutput("aktuelleFeuchtigkeitText_Ende"),
                            tags$br(),
                            textOutput("aktuelleHumText"),
                            tags$br(),
                            textOutput("aktuelleLuftText"),
                            tags$br(),
                            textOutput("weather_today"),
                            tags$br(),
                            textOutput("weather_tomorrow"),
                            tags$br(),
                            textOutput("aktueller_wasserstand"),
                            hr(),
                            textOutput("system_settings"),
                            hr(),
                            actionButton("start_button_manual", label = "Bewässerung für alle Pflanzen starten"),
                            tags$br(),
                            actionButton("stop_button_manual", label = "Bewässerung stoppen"),
                            width = 4,
                            hr(),
                            checkboxGroupInput(inputId = "Pflanzen_Auswahl", label = "",
                                               choices = c("Rosenbeet",
                                                           "Kräuter",
                                                           "Blumentöpfe"),
                                               inline = TRUE),
                            hr(),
                            actionButton("start_button_manual_selected", label = "Bewässerung für gewählte Pflanzen starten")
                        ),
                
                        # Show a plot of the generated distribution
                        mainPanel(
                            selectInput("variable", "Standortauswahl:",
                                        c("Anfang" = "Anfang",
                                          "Ende" = "Ende")),
                            h3("Entwicklung der Bodenfeuchtigkeit"),
                            plotOutput("FeuchtigkeitsPlot")
                            
                          )#end main Panel
                        
                        )#end sidebar Layout
                
                     ), #end Messpanel
    
        
      
        
        tabPanel("Über", 
             
             navlistPanel(  
             
                 ### ---- About - Idee ----
                 tabPanel("Idee",
                          
                          p("Idee Inhalt")
                 ), #close Idea page
                 
                 tabPanel("Funktionsweise",
                          
                          p("funktionsweise")

                 ), #close funktions tab
                 
                 tabPanel("Einstellungen",
                          h3("Balkonpflanzen"),
                          numericInput("input_manuell_length_watering", "Dauer der manuellen Bewässerung [min]",
                                       value = 1, min = 1, max = 1000, step = 1),
                        
                 ), #close Einstellungen tab
                 
                 tabPanel("Zeiteinstellungen",
                          
                          h3("Einstellung der regelmässigen Bewässerung"),
                          numericInput("hours_watering", "Stunde", value = "17", 1, 24, 1),
                          selectInput(inputId = "frequency", "Standortauswahl:",
                                      c("Täglich" = "Täglich",
                                        "Alle zwei Tage" = "Alle zwei Tage"))
                 ), #close funktions tab
                 
                 
                 widths = c(2, 10) #set navl
                 
                 ) #End Navlist Panel
             
             
             
             
    ) #End About panel

    ) #end Navbar

) #end Shiny UI

