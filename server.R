# childes-coder version 0.1
# https://github.com/tlonic/childes-coder

function(input, output, session) {
  validateLibraries <- function() {
      validate(need(libraries_loaded,
                    "Couldn't load all necessary libraries. Please confirm they are installed."))
  }
  validateConfig <- function() {
      validate(need(! isError(config),
                    "Config file failed to load properly."))
      validate(need(config$enterers > 0,
                    "Valid enterers not found."))
  }
  validateEnterers <- function() {
      validate(need(input$enterer, "Loading..."))
  }
  needsFields <- function() {
      unlist(lapply(names(config[["fields"]]), function(field) {
        c(need(! grepl("[^0-9,a-z,A-Z$_]", field),
               paste0("Invalid field name ", 
                      field,
                      "! Please use valid characters.")
          ),
          need(config[["fields"]][[field]][["type"]] %in% c("checkbox",
					                             	        "select",
					                                        "radio",
					                                        "text"),
	           paste0("Invalid field type for ", field, "! Check config.")
          ),
          need(length(config[["fields"]][[field]][["label"]]) == 1,
               paste0("Invalid field label for ", field, "! Fields must have exactly one label.")
          ),
          need(is.character(config[["fields"]][[field]][["label"]]),
               paste0("Invalid label for field", field, "! Labels must be strings.")
          ),
          if (config[["fields"]][[field]][["type"]] %in% c("select", "radio")) {
                need(is.character(config[["fields"]][[field]][["choices"]]),
                     paste0("Invalid choices for field ", field, "! Choices must be strings.")
                )
          }
        )
    }))
  }
  needsChildes <- c(need(! isError(childes_pool),
                    "CHILDES DB connection could not be established."),
                    need(! isError(search_tokens),
                    "Something failed with the token search. Was it configured properly?"))
  needsResponses <- need(! isError(responses_pool),
                    "Responses DB connection could not be established. Does it need to be created?")

  formData <- reactive({
    target_token <- subset(search_tokens, id == metadata$id)
    utid <- target_token$utterance_id
    data <-
      data.frame(
        token_id = metadata$id,
        utterance_id = utid,
        enterer = input$enterer,
        stringsAsFactors = FALSE
      )
    for (field in names(config[["fields"]])) {
      if (typeof(input[[field]]) == "logical") {
        data[field] = as.integer(input[[field]])
      }
      else{
        data[field] = input[[field]]
      }
    }
    data
  })
  
  metadata <- reactiveValues(initialized = FALSE,
                             unsaved_changes = FALSE,
                             new_responses = TRUE)
 
  output$enterer_panel <- renderUI({
  validateLibraries()
  validateConfig()
  selectInput(
        inputId = "enterer",
        label = "Enterer",
        choices = config$enterers)
  })

  output$coding <- renderUI({
    validateEnterers()
    if (!metadata$initialized) {
      validate(needsFields(), needsChildes, needsResponses)
      metadata$id <- getFirstToken(input$enterer, responses_pool)
      metadata$position <-
        which(sorted_search_tokens$id == metadata$id)
      metadata$initialized <- TRUE
    }
    token_data <- getData(tid = metadata$id,
			  enterer = input$enterer,
			  responses_pool)
    tagList(
      tags$div(
        strong("Token ID: "),
        as.character(metadata$id),
        "(",
        as.character(metadata$position),
        "/",
        as.character(nrow(sorted_search_tokens)),
        ")"
      ),
      lapply(names(config[["fields"]]), function(field) {
        switch(config[["fields"]][[field]][["type"]],
          "checkbox" = 
            checkboxInput(
                inputId = field,
                label = config[["fields"]][[field]][["label"]],
                value = isTRUE(as.logical(token_data[[field]]))
            ),
          "select" = 
            selectInput(
                inputId = field,
                label = config[["fields"]][[field]][["label"]],
                choices = config[["fields"]][[field]][["choices"]],
                multiple = FALSE,
                selected = ifelse(is.na(token_data[[field]]), 
			                      config[["fields"]][[field]][["choices"]][1],
			                      token_data[[field]]) 
          ),
          "radio" = 
            radioButtons(
                inputId = field,
                label = config[["fields"]][[field]][["label"]],
                choices = config[["fields"]][[field]][["choices"]],
                selected = ifelse(is.na(token_data[[field]]), 
			                        config[["fields"]][[field]][["choices"]][1],
			                        token_data[[field]]) 
             ),
          "text" = 
             textInput(
                inputId = field,
                label = config[["fields"]][[field]][["label"]],
                value = ifelse(is.na(token_data[[field]]), "", token_data[[field]])
          )
        )
      }),
      actionButton(
        inputId = "save",
        icon = icon("save"),
        label = "Save"
      )
    )
  })
  
  output$transcript <- renderUI({
    if (metadata$initialized) {
        target_token <- subset(search_tokens, id == metadata$id)
        utid <- target_token$utterance_id
        target_utterance <- getUtterances(utid, childes_pool)
        preceding_utterances <-
            getSurroundingUtterances(utid,
                               window = input$window_size,
                               connection = childes_pool)
        following_utterances <-
            getSurroundingUtterances(
            utid,
            position = "after",
            window = input$window_size,
            connection = childes_pool
            )
        transcript_url <-
        getChildesLink(utid, connection = childes_pool)
    
        tagList(
            tags$a(
                href = transcript_url,
                target = "_blank",
                "See full CHILDES transcript"
            ),
            tags$table(
                getContextUtteranceTags(preceding_utterances),
                getTargetUtteranceTags(target_utterance, target_token$token_order),
                getContextUtteranceTags(following_utterances)
            )
        )
    }
    else{
        tags$p("Loading...")
    }
  })
  
  output$responses <- renderDataTable({
    validateLibraries()
    validateConfig()
    validate(needsChildes, needsResponses)
    if (metadata$new_responses) {
        metadata$new_responses <- FALSE
    }
    responses <- getResponseTable(childes_pool, responses_pool)
    validate(need(nrow(responses) > 0, "No responses entered yet."))
    responses$token_id <-
      paste0(
        "<a onclick='Shiny.onInputChange(\"token_change\",",
        responses$token_id,
        ")' href='#'>",
        responses$token_id,
        "</a>"
      )
    responses
  }, escape = FALSE)
  
  output$tokens <- renderDataTable({
    validateLibraries()
    validateConfig()
    validate(needsChildes, needsResponses)
    tokens <- getTokenTable(childes_pool)
    tokens$token_id <-
      paste0(
        "<a onclick='Shiny.onInputChange(\"token_change\",",
        tokens$token_id,
        ")' href='#'>",
        tokens$token_id,
        "</a>"
      )
    tokens
  }, escape = FALSE)
  
  output$downloadResponses <- downloadHandler(
    filename = function() {
      date <- format(Sys.time(), "%Y-%m-%d_%H:%M:%S")
      paste("childes_responses_", date, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getResponseTable(childes_pool, responses_pool),
                file,
                row.names = FALSE)
    }
  )
  
  output$downloadTokens <- downloadHandler(
    filename = function() {
      date <- format(Sys.time(), "%Y-%m-%d_%H:%M:%S")
      paste("childes_tokens_", date, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getTokenTable(childes_pool), file, row.names = FALSE)
    }
  )
  
  output$downloadSQL <- downloadHandler(
    filename = function() {
      date <- format(Sys.time(), "%Y-%m-%d_%H:%M:%S")
      paste0(config$responses_db$dbname, "_", date, ".sql")
    },
    content = function(file) {
      fileConn <- file(file)
      prefaceLines <- c(
        "CREATE TABLE responses (",
        "token_id INT NOT NULL,",
        "enterer VARCHAR(99) NOT NULL,",
        "utterance_id INT NOT NULL,"
      )
      fieldLines <- sapply(names(config[["fields"]]), function(field) {
        switch(config[["fields"]][[field]][["type"]],
               "checkbox" = paste(field, "BOOLEAN,"),
               "select"   = paste(field, "VARCHAR(99) NOT NULL,"),
               "radio"    = paste(field, "VARCHAR(99) NOT NULL,"),
               "text"     = paste(field, "VARCHAR(65535) NULL,"))
      })
      writeLines(
        c(
          prefaceLines,
          fieldLines,
          "CONSTRAINT Token_Enterer PRIMARY KEY (token_id, enterer));"
        ),
        fileConn
      )
      close(fileConn)
    }
  )
 
  observeEvent(input$enterer, {
    metadata$initialized <- FALSE
    metadata$unsaved_changes <- FALSE
  })
  
  observeEvent(input$save, {
    saveData(formData(), responses_pool)
    metadata$unsaved_changes <- FALSE
    metadata$new_responses <- TRUE
    showNotification("Input saved.")
  })
  
  unsavedChangesModal <- function(direction) {
    modalDialog(
      title = "Are you sure?",
      "There are still unsaved changes. Are you sure you want to move to another token?",
      footer = tagList(actionButton(paste0(
        "yes_", direction
      ), "Yes"),
      modalButton("Cancel"))
    )
  }
  
  observeEvent(input$prev,
               {
                 if (metadata$unsaved_changes) {
                   showModal(unsavedChangesModal("prev"))
                 }
                 else if (metadata$position != 1) {
                   metadata$position <- metadata$position - 1
                   metadata$id <-
                     sorted_search_tokens[(metadata$position), ]$id
                 }
               })
  
  observeEvent(input$nxt,
               {
                 if (metadata$unsaved_changes) {
                   showModal(unsavedChangesModal("nxt"))
                 }
                 else if (metadata$position != nrow(sorted_search_tokens)) {
                   metadata$position <- metadata$position + 1
                   metadata$id <-
                     sorted_search_tokens[(metadata$position), ]$id
                 }
               })
  
  observeEvent(input$yes_prev, {
    removeModal()
    metadata$unsaved_changes <- FALSE
    click("prev")
  })
  
  observeEvent(input$yes_nxt, {
    removeModal()
    metadata$unsaved_changes <- FALSE
    click("nxt")
  })

  observeEvent(input$restart, {
    file.create("restart.txt")
    runjs("location.reload(true);")
  })
  
  field_observers <- if (!isError(config)) sapply(names(config[["fields"]]), function(x) {
    onclick(x,
            {
              metadata$unsaved_changes <- TRUE
            })
  })
  
  observeEvent(input$token_change, {
    metadata$id <- input$token_change
    metadata$position <- which(sorted_search_tokens$id == metadata$id)
  })
  
  
}
