# --------
# OBOB
# --------

# Install and load shiny and shinyjs, this is the basis of the app.

install.packages("shiny")
library(shiny)
install.packages("shinyjs")
library(shinyjs)


# ---- USER INTERFACE ----
# This is the layout of the app. Here, the stylistic choices
# are defined, such as the size and color of the buttons and the reminder dot.
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
  
  # reminder and timer
  uiOutput("reminder"),
  uiOutput("intervalStatus"),
  uiOutput("timerUI"),
  
  # start button (size and color)
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
  
  # Below, the details of the buttons are defined. The titles of the buttons and the 
  # columns can be adjusted here, as well as the colors.
  
  fluidRow(
    column(3,
           h3("Student: Behavior Class"),
           actionButton("g1", "Assertive behavior", class = "big-button", style = "background-color:#2BB512;"),
           actionButton("g2", "Overassertive behavior", class = "big-button", style = "background-color:#F5DB18;"),
           actionButton("g3", "Threatening behavior", class = "big-button", style = "background-color:#F57818;"),
           actionButton("g4", "Violent behavior", class = "big-button", style = "background-color:#F53D18;")
    ),
    column(3,
           h3("Teacher: Warm"),
           actionButton("i11", "Student in control", class = "big-button", style = "background-color:#2BB512;"),
           actionButton("i21", "Shared control", class = "big-button", style = "background-color:#F5DB18;"),
           actionButton("i31", "Teacher takeover", class = "big-button", style = "background-color:#F57818;"),
           actionButton("i41", "Teacher in control", class = "big-button", style = "background-color:#F53D18;")
           
    ),
    column(3,
           h3("Teacher: Cold"),
           actionButton("i12", "Student in control", class = "big-button", style = "background-color:#155709;"),
           actionButton("i22", "Shared control", class = "big-button", style = "background-color:#dbc416;"),
           actionButton("i32", "Teacher takeover", class = "big-button", style = "background-color:#b15711;"),
           actionButton("i42", "Teacher in control", class = "big-button", style = "background-color:#8f240e;"),
           #The button below is reserved for physical interventions by the teacher.
           actionButton("i52", "Complete teacher control", class = "big-button", style = "background-color:#5c1709;")
    ),
    column(3,
           h3(""),
           actionButton("note", "Notes", class = "big-button", style="background-color:#e9b4cb;"),
           actionButton("undo", "Undo", class = "big-button", style="background-color:#a2c4c9;"),
           actionButton("note2","Background Info",class = "big-button",
                        style = "background-color:#CDC7E5;"),
           downloadButton("downloadData", "Download", class = "big-button", style="background-color:#eeeeee;")
    )
  )
)


# ---- SERVER ----
# This part covers the actual functionality of the app.
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
    hide("start") # As soon as the start button is clicked, this button disappears and the timer starts.
    
    # The line below ensures that the data is saved under a fixed filename,
    # in the format "observations_day-month-year-hour-minute".
    file(
      paste0("observations_", format(Sys.time(), "%d-%m-%Y_%H-%M"), ".csv")
    )
  })
  
  observe({
    invalidateLater(1000, session)
    if (timer_running()) {
      timer_sec(isolate(timer_sec()) + 1)
    }
  })
  
  # The timer is defined below, including stylistic choices.
  output$timerUI <- renderUI({
    minutes <- floor(timer_sec() / 60)
    seconds <- timer_sec() %% 60
    span(sprintf("%02d:%02d", minutes, seconds),
         style="font-weight:bold; font-size:72px; position:fixed; bottom:20px; left:20px;")
  })
  
  format_timer <- function(sec) {
    sprintf("%02d:%02d", floor(sec / 60), sec %% 60)
  }
  
  # ---- DATA STORAGE ----
  # This part defines the data storage mechanism. 
  
  data <- reactiveVal(
    data.frame(
      # The variables below are the chosen columns for our version. 
      # The words on the left of the = sign can be adjusted to match your research data.
      # Please ensure the variable matches the format on the right.
      reminder = integer(),
      time = character(),
      studentbehavior = numeric(),
      teacherbehavior = numeric(),
      notes = character(),
      backgroundinfo = character(),
      stringsAsFactors = FALSE
    )
  )
  
  selected_row <- reactiveVal(NULL)
  
  
  # ---- REMINDER ----
  reminder_counter <- reactiveVal(0)
  reminder_active <- reactiveVal(FALSE)
  last_triggered <- reactiveVal(0)
  
  # reminder dot
  observe({
    t <- timer_sec()
    
    # A reminder dot has been chosen that appears every 25 seconds. 
    # If you want shorter or longer time intervals, change the number 25 below.
    
    if (t > 0 && t %% 25 == 0 && last_triggered() != t) {
      last_triggered(t)
      
      # Increment reminder counter, which corresponds to the 'interval' column in the dataset.
      nr <- reminder_counter() + 1
      reminder_counter(nr)
      
      reminder_active(TRUE)
      
      # Create a new (empty) row when the reminder dot appears.
      df <- data()
      empty_row <- data.frame(
        reminder_nr = nr,
        time = NA_character_,
        studentbehavior = NA,
        teacherbehavior = NA,
        notes = NA_character_,
        backgroundinfo = NA_character_,
        stringsAsFactors = FALSE
      )
      
      # Add the new row to the existing data.
      df <- rbind(df, empty_row)
      data(df)
      
      selected_row(nrow(df))
      input_allowed(TRUE)
    }
  })
  
  output$reminder <- renderUI({
    if (reminder_active()) tags$div(id = "reminder-dot")
  })
  
  
  # ---- TEMPORARY STORAGE -----
  # Here, the clicked buttons are stored in the dataframe.
  
  selected_behavior <- reactiveVal(NULL)
  selected_intervention <- reactiveVal(NULL)
  
  # Behavior buttons
  observeEvent(input$g1, { req(input_allowed()); selected_behavior(1) })
  observeEvent(input$g2, { req(input_allowed()); selected_behavior(2) })
  observeEvent(input$g3, { req(input_allowed()); selected_behavior(3) })
  observeEvent(input$g4, { req(input_allowed()); selected_behavior(4) })
  
  
  # Intervention buttons
  for (i in 1:52) {
    local({
      idx <- i
      observeEvent(input[[paste0("i", idx)]], {
        req(input_allowed(), !is.null(selected_behavior()))
        selected_intervention(idx)
      }, ignoreInit = TRUE)
    })
  }
  
  
  # Save on click
  observe({
    g <- selected_behavior()
    i <- selected_intervention()
    row <- selected_row()
    
    if (!is.null(g) && !is.null(i) && !is.null(row)) {
      df <- data()
      df$time[row] <- format_timer(timer_sec())
      df$studentbehavior[row] <- g
      df$teacherbehavior[row] <- i
      data(df)
      
      # Create a CSV file with the recorded data.
      write.csv(data(), file(), row.names = FALSE)
      
      # After clicking the buttons, the reminder disappears and
      # no behavior or intervention button can be clicked until the next reminder dot appears.
      reminder_active(FALSE)
      input_allowed(FALSE)
      selected_behavior(NULL)
      selected_intervention(NULL)
    }
  })
  
  # Interval status
  
  output$intervalStatus <- renderUI({
    if (is.null(selected_row())) return(NULL)
    
    # When a new interval opens, the text "Waiting for input..." appears.
    # This disappears when a button is clicked in both columns.
    status <- if (input_allowed()) {
      "Waiting for input..."
    } else {
      "Entered ✔"
    }
    
    # Stylistic choices around this part.
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
  
  
  # ---- NOTES ----
  observeEvent(input$note, {
    showModal(modalDialog(
      # The "Notes" button can be adjusted here.
      textInput("note_text", "Notes:", ""),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_note", "Save")
      )
    ))
  })
  
  # The note is saved below in the row of the current interval.
  observeEvent(input$save_note, {
    df <- data()
    row <- selected_row()
    
    if (!is.null(row) && row <= nrow(df)) {
      df$notes[row] <- input$note_text
      data(df)
      write.csv(df, file(), row.names = FALSE)
    }
    
    removeModal()
  })
  
  
  # ---- BACKGROUND INFO ----
  
  observeEvent(input$note2, {
    showModal(modalDialog(
      # The "Background Info" button can be adjusted here.
      textInput("background_text", "Background Info:", ""),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_background", "Save")
      )
    ))
  })
  
  
  # The background info is saved below in the row of the current interval.
  observeEvent(input$save_background, {
    df <- data()
    row <- selected_row()
    
    if (!is.null(row) && row <= nrow(df)) {
      df$backgroundinfo[row] <- input$background_text
      data(df)
      write.csv(df, file(), row.names = FALSE)
    }
    
    removeModal()
  })
  
  
  # ---- UNDO ----
  
  observeEvent(input$undo, {
    df <- data()
    
    if (nrow(df) > 0) {
      
      # Reopen the last row.
      selected_row(nrow(df))
      
      # Allow the buttons to be clicked again.
      input_allowed(TRUE)
      
      # Remove the previous selection of the buttons.
      selected_behavior(NULL)
      selected_intervention(NULL)
      
      # Show the reminder dot again until 2 new buttons are clicked.
      reminder_active(TRUE)
    }
  })
  
  # ---- DOWNLOAD ----
  output$downloadData <- downloadHandler(
    filename = function() {
      # The data is downloaded in the format chosen above (observations_date notation) 
      # If no data has been recorded, this is indicated in the filename.
      if (is.null(file())) {
        return("observations_no_data.csv")
      }
      return(file())
    },
    content = function(file) {
      # Write the data to the file that the browser must download.
      df <- data()
      write.csv(df, file, row.names = FALSE)
    }
  )
}
shinyApp(ui = ui, server = server)