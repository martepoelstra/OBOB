# --------
# OBOB
# --------

# Installeer en laad shiny en shinyjs, dit is de basis van de app.

install.packages("shiny")
library(shiny)
install.packages("shinyjs")
library(shinyjs)


# ---- USER INTERFACE ----
# Dit is de layout van de app. Hieronder zijn de stylistische keuzes
# vastgelegd, zoals het formaat en de kleur van de knoppen en de reminder-stip.
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .big-button {
        width: 100%;
        height: 100px;
        font-size: 20px;
        font-weight: bold;
        align-items: center !important;
        justify-content: center !important;
        text-align: center !important;
        margin-bottom: 10px;
        line-height: 60px !important; 
      }
      #reminder-dot {
        height: 100px;
        width: 100px;
        background-color: red;
        border-radius: 50%;
        position: fixed;
        bottom: 20px;
        right: 20px;
        z-index: 9999;
      }
    "))
  ),
  
  # reminder en timer
  uiOutput("reminder"),
  uiOutput("intervalStatus"),
  uiOutput("timerUI"),
  
  # startknop (formaat en kleur)
  actionButton(
    "start", "Start",
    class = "big-button",
    style = "
      position: fixed;
      bottom: 20px;
      left: 20px;
      width: 200px !important;
      z-index: 9999;
      background-color: #d5f5fb;"
  ),
  
  # Hieronder worden de details van de knoppen uitgewerkt. De titels van de knoppen en de 
  # kolommen kunnen hier worden aangepast, evenals de kleuren.
  
  fluidRow(
    column(3,
           h3("Leerling: Gedrag Klas"),
           actionButton("g1", "Assertief gedrag", class = "big-button", style = "background-color:#2BB512;"),
           actionButton("g2", "Overassertief gedrag", class = "big-button", style = "background-color:#F5DB18;"),
           actionButton("g3", "Bedreigend gedrag", class = "big-button", style = "background-color:#F57818;"),
           actionButton("g4", "Gewelddadig gedrag", class = "big-button", style = "background-color:#F53D18;")
    ),
    column(3,
           h3("Leraar: Hoge Nabijheid"),
           actionButton("i11", "Regie bij leerling", class = "big-button", style = "background-color:#2BB512;"),
           actionButton("i21", "Gedeelde regie", class = "big-button", style = "background-color:#F5DB18;"),
           actionButton("i31", "Overname van regie", class = "big-button", style = "background-color:#F57818;"),
           actionButton("i41", "Regie bij leraar", class = "big-button", style = "background-color:#F53D18;")
        
    ),
    column(3,
           h3("Leraar: Lage Nabijheid"),
           actionButton("i12", "Regie bij leerling", class = "big-button", style = "background-color:#155709;"),
           actionButton("i22", "Gedeelde regie", class = "big-button", style = "background-color:#dbc416;"),
           actionButton("i32", "Overname van regie", class = "big-button", style = "background-color:#b15711;"),
           actionButton("i42", "Regie bij leraar", class = "big-button", style = "background-color:#8f240e;"),
           actionButton("i52", "Volledige regie bij leraar", class = "big-button", style = "background-color:#5c1709;")
    ),
    column(3,
           h3(""),
           actionButton("note", "Opmerkingen", class = "big-button", style="background-color:#e9b4cb;"),
           actionButton("undo", "Herstel", class = "big-button", style="background-color:#a2c4c9;"),
           actionButton("note2","Achtergrondinfo",class = "big-button",
                        style = "background-color:#CDC7E5;"),
           downloadButton("downloadData", "Downloaden", class = "big-button", style="background-color:#eeeeee;")
    )
  )
)


# ---- SERVER ----
# Dit onderdeel gaat over de echte werking van de app.
server <- function(input, output, session) {
  
  file <- reactiveVal(NULL)
  input_allowed <- reactiveVal(FALSE)
  observe({
    input_allowed(reminder_active())
  })
  
  # ---- TIMER ----
  
  timer_sec <- reactiveVal(0)
  timer_running <- reactiveVal(FALSE)
  
  observeEvent(input$start, {
    timer_sec(0)
    timer_running(TRUE)
    hide("start") # Zodra op de startknop wordt geklikt, verdwijnt deze knop en begint de timer.
    
    # De regel hieronder zorgt ervoor dat de data onder een vaste bestandsnaam wordt opgeslagen,
    # in het format "observaties_dag-maand-jaar-uur-minuut".
    file(
      paste0("observaties_", format(Sys.time(), "%d-%m-%Y_%H-%M"), ".csv")
    )
  })
  
  observe({
    invalidateLater(1000, session)
    if (timer_running()) {
      timer_sec(isolate(timer_sec()) + 1)
    }
  })
  
  # Hieronder wordt de timer uitgewerkt, inclusief stijlkeuzes.
  output$timerUI <- renderUI({
    minutes <- floor(timer_sec() / 60)
    seconds <- timer_sec() %% 60
    span(sprintf("%02d:%02d", minutes, seconds),
         style="font-weight:bold; font-size:72px; position:fixed; bottom:20px; left:20px;")
  })
  
  format_timer <- function(sec) {
    sprintf("%02d:%02d", floor(sec / 60), sec %% 60)
  }
  
  # ---- DATA OPSLAG ----
  # In dit onderdeel wordt de opslag van de data uitgewerkt. 
  
  data <- reactiveVal(
    data.frame(
      # De onderstaande variabelen zijn de gekozen kolommen voor onze versie. 
      # De woorden aan de linkerkant van het =-teken kunnen worden aangepast om aan te sluiten bij de data van uw onderzoek.
      # Let hierbij wel op dat de variabele aansluit bij het format aan de rechterkant.
      reminder = integer(),
      tijd = character(),
      leerlinggedrag = numeric(),
      leraargedrag = numeric(),
      opmerkingen = character(),
      achtergrondinfo = character(),
      stringsAsFactors = FALSE
    )
  )
  
  selected_row <- reactiveVal(NULL)
  
  
  # ---- REMINDER ----
  reminder_counter <- reactiveVal(0)
  reminder_active <- reactiveVal(FALSE)
  last_triggered <- reactiveVal(0)
  
  # ---- reminder ----
  observe({
    t <- timer_sec()
    
    # Hier is gekozen voor een reminder-stip die iedere 25 seconden in beeld komt.
    
    if (t > 0 && t %% 25 == 0 && last_triggered() != t) {
      last_triggered(t)
      
      # verhoog reminder counter, deze staat in verband met de 'interval' kolom in de dataset.
      nr <- reminder_counter() + 1
      reminder_counter(nr)
      
      reminder_active(TRUE)
      
      # Creëer een nieuwe (lege) rij wanneer de reminder-stip in beeld komt.
      df <- data()
      lege_rij <- data.frame(
        reminder_nr = nr,
        tijd = NA_character_,
        leerlinggedrag = NA,
        leraargedrag = NA,
        opmerkingen = NA_character_,
        achtergrondinfo = NA_character_,
        stringsAsFactors = FALSE
      )
      
      # Voeg de nieuwe rij toe aan de bestaande data.
      df <- rbind(df, lege_rij)
      data(df)
      
      selected_row(nrow(df))
      input_allowed(TRUE)
    }
  })
  
  output$reminder <- renderUI({
    if (reminder_active()) tags$div(id = "reminder-dot")
  })
  
  
# ---- TIJDELIJKE OPSLAG -----
  # Hierin worden de aangeklikte knoppen opgeslagen in de dataframe.
  
  geselecteerd_gedrag <- reactiveVal(NULL)
  geselecteerd_interventie <- reactiveVal(NULL)
  
  # Gedrag knoppen
  observeEvent(input$g1, { req(input_allowed()); geselecteerd_gedrag(1) })
  observeEvent(input$g2, { req(input_allowed()); geselecteerd_gedrag(2) })
  observeEvent(input$g3, { req(input_allowed()); geselecteerd_gedrag(3) })
  observeEvent(input$g4, { req(input_allowed()); geselecteerd_gedrag(4) })
  
  
  # Interventie knoppen
  for (i in 1:52) {
    local({
      idx <- i
      observeEvent(input[[paste0("i", idx)]], {
        req(input_allowed(), !is.null(geselecteerd_gedrag()))
        geselecteerd_interventie(idx)
      }, ignoreInit = TRUE)
    })
  }
  
  
  # Opslaan bij klik
  observe({
    g <- geselecteerd_gedrag()
    i <- geselecteerd_interventie()
    row <- selected_row()
    
    if (!is.null(g) && !is.null(i) && !is.null(row)) {
      df <- data()
      df$tijd[row] <- format_timer(timer_sec())
      df$leerlinggedrag[row] <- g
      df$leraargedrag[row] <- i
      data(df)
      
      # Maak een CSV bestand aan met de geregistreerde data.
      write.csv(data(), file(), row.names = FALSE)
      
      # Na het aanklikken van de knoppen verdwijnt de reminder en 
      # kan er geen gedrag- of interventieknop aangeklikt worden tot de volgende reminderstip verschijnt.
      reminder_active(FALSE)
      input_allowed(FALSE)
      geselecteerd_gedrag(NULL)
      geselecteerd_interventie(NULL)
    }
  })
  
  # Interval status
  
  output$intervalStatus <- renderUI({
    if (is.null(selected_row())) return(NULL)
    
    # Wanneer een nieuw interval opent, verschijnt de tekst "Wacht op invoer..."
    # Deze verdwijnt wanneer in beide kolommen een knop wordt aangeklikt.
    status <- if (input_allowed()) {
      "Wacht op invoer..."
    } else {
      "Ingevoerd ✔"
    }
    
    # Stylistische keuzes rondom dit deel.
    tags$div(
      style = "
      position: fixed;
      bottom: 110px;
      left: 20px;
      background-color: #f0f0f0;
      padding: 10px;
      border-radius: 8px;
      font-size: 20px;
    ",
      strong("Interval: "), selected_row(), br(),
      strong("Status: "), status
    )
  })
  
  
  # ---- OPMERKINGEN ----
  observeEvent(input$note, {
    showModal(modalDialog(
      #De "Opmerkingen" knop kan hier worden aangepast.
      textInput("opmerking_text", "Opmerkingen:", ""),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuleer"),
        actionButton("save_opmerking", "Opslaan")
      )
    ))
  })
  
  # Hieronder wordt de opmerking opgeslagen in de rij van het huidige interval.
  observeEvent(input$save_opmerking, {
    df <- data()
    row <- selected_row()
    
    if (!is.null(row) && row <= nrow(df)) {
      df$opmerkingen[row] <- input$opmerking_text
      data(df)
      write.csv(df, file(), row.names = FALSE)
    }
    
    removeModal()
  })
  
  
  # ---- ACHTERGRONDINFO ----
  
  observeEvent(input$note2, {
    showModal(modalDialog(
      #Hier kan de "achtergrondinfo" knop worden aangepast.
      textInput("achtergrond_text", "Achtergrondinfo:", ""),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuleer"),
        actionButton("save_achtergrond", "Opslaan")
      )
    ))
  })
  
  
  # Hieronder wordt de achtergrondinfo opgeslagen in de rij van het huidige interval.
  observeEvent(input$save_achtergrond, {
    df <- data()
    row <- selected_row()
    
    if (!is.null(row) && row <= nrow(df)) {
      df$achtergrondinfo[row] <- input$achtergrond_text
      data(df)
      write.csv(df, file(), row.names = FALSE)
    }
    
    removeModal()
  })
  
  
  # ---- HERSTEL ----

  observeEvent(input$undo, {
    df <- data()
    
    if (nrow(df) > 0) {
      
      # Heropen de laatste rij.
      selected_row(nrow(df))
      
      # Sta toe dat de knoppen opnieuw mogen worden aangeklikt.
      input_allowed(TRUE)
      
      # Verwijder de voorgaande selectie van de knoppen.
      geselecteerd_gedrag(NULL)
      geselecteerd_interventie(NULL)
      
      # Laat opnieuw de reminderstip zien tot 2 nieuwe knoppen worden aangeklikt.
      reminder_active(TRUE)
    }
  })
  
  # ---- DOWNLOAD ----
  output$downloadData <- downloadHandler(
    filename = function() {
      # De data wordt gedownload in het format dat hierboven is gekozen (observaties_datumnotering) 
      # Als er geen data is geregistreerd, wordt dat vermeld in de bestandsnaam.
      if (is.null(file())) {
        return("observaties_geen_data.csv")
      }
      return(file())
    },
    content = function(file) {
      # Schrijf de data naar het bestand dat de browser moet downloaden.
      df <- data()
      write.csv(df, file, row.names = FALSE)
    }
  )
}
shinyApp(ui = ui, server = server)
