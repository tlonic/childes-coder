fluidPage(
  useShinyjs(),
  extendShinyjs(text = hotkeys, functions = c("init")),
  # Include shinyjs
  
  
  tags$head(tags$style(
    HTML(
      "
        td {
          padding: 15px;
        }

        tr:nth-child(even) {background-color: #f2f2f2;}

        tr.target_utterance {
          background-color: #4CAF50;
        }

    "
    )
  )),
  
  # App title ----
  titlePanel("CHILDES Coder"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for window size
      htmlOutput(outputId = "enterer_panel"),

      sliderInput(
        inputId = "window_size",
        label = "Window Size:",
        min = 1,
        max = 15,
        value = 5
      ),
      
      htmlOutput(outputId = "coding"),
      
      hr(),
      
      actionButton(inputId = "prev", label = "< Previous"),
      actionButton(inputId = "nxt", label = "Next >")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Transcript", htmlOutput("transcript")),
        tabPanel(
          "Responses",
          downloadButton("downloadResponses", "Download responses"),
          dataTableOutput("responses")
        ),
        tabPanel(
          "All Tokens",
          downloadButton("downloadTokens", "Download all token data"),
          dataTableOutput("tokens")
        ),
        tabPanel(
          "Utilities",
          downloadButton("downloadSQL", "Download SQL file to create table"),
	  actionButton(inputId = "restart", label = "Restart Shiny Session")
        )
      )
      
    )
  )
)
