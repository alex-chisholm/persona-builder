library(shiny)
library(bslib)
library(tidyverse)
library(httr)
library(jsonlite)

# UI
ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    preset = "shiny",
    primary = "#0066cc",
    "font-size-base" = "0.95rem"
  ),
  
  title = "Persona Builder",
  
  sidebar = sidebar(
    width = 350,
    
    card(
      card_header("Add Comments"),
      card_body(
        textAreaInput("manual_comment", "Enter a comment:", 
                      rows = 3, width = "100%",
                      placeholder = "Type a comment here..."),
        actionButton("add_comment", "Add Comment", 
                     class = "btn-primary w-100",
                     icon = icon("plus"))
      )
    ),
    
    card(
      card_header("Upload CSV"),
      card_body(
        fileInput("csv_file", NULL,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"),
        uiOutput("column_selector")
      )
    ),
    
    card(
      card_header("Generate"),
      card_body(
        numericInput("num_personas", "Number of Personas:", 
                     value = 3, min = 2, max = 10, step = 1),
        actionButton("generate", "Generate Personas", 
                     class = "btn-success w-100",
                     icon = icon("wand-magic-sparkles"))
      )
    ),
    
    card(
      card_body(
        value_box(
          title = "Comments Loaded",
          value = textOutput("comment_count_value"),
          theme = "primary",
          showcase = icon("comments")
        ),
        verbatimTextOutput("api_status", placeholder = TRUE)
      )
    )
  ),
  
  navset_card_tab(
    nav_panel(
      "Comments",
      card_body(
        layout_columns(
          actionButton("clear_comments", "Clear All", 
                       class = "btn-outline-warning btn-sm",
                       icon = icon("trash")),
          col_widths = 12
        ),
        tableOutput("comments_table")
      )
    ),
    nav_panel(
      "Personas",
      icon = icon("users"),
      card_body(
        uiOutput("personas_output")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  comments_data <- reactiveVal(data.frame(comment = character(), stringsAsFactors = FALSE))
  personas_result <- reactiveVal(NULL)
  
  # Check API key status
  output$api_status <- renderText({
    api_key <- Sys.getenv("ANTHROPIC_API_KEY")
    if (api_key != "" && nchar(api_key) > 0) {
      "✓ API key configured"
    } else {
      "⚠ Set ANTHROPIC_API_KEY\nenvironment variable"
    }
  })
  
  # Display comment count for value box
  output$comment_count_value <- renderText({
    nrow(comments_data())
  })
  
  # Add manual comment
  observeEvent(input$add_comment, {
    req(input$manual_comment)
    if (nchar(trimws(input$manual_comment)) > 0) {
      current <- comments_data()
      new_comment <- data.frame(comment = trimws(input$manual_comment), 
                                stringsAsFactors = FALSE)
      comments_data(bind_rows(current, new_comment))
      updateTextAreaInput(session, "manual_comment", value = "")
    }
  })
  
  # Handle CSV upload - show column selector
  output$column_selector <- renderUI({
    req(input$csv_file)
    df <- read_csv(input$csv_file$datapath, show_col_types = FALSE)
    selectInput("comment_column", "Select comment column:", 
                choices = names(df),
                width = "100%")
  })
  
  # Load comments from CSV when column is selected
  observeEvent(input$comment_column, {
    req(input$csv_file, input$comment_column)
    df <- read_csv(input$csv_file$datapath, show_col_types = FALSE)
    
    new_comments <- df %>%
      select(comment = !!sym(input$comment_column)) %>%
      filter(!is.na(comment), nchar(trimws(as.character(comment))) > 0) %>%
      mutate(comment = as.character(comment))
    
    current <- comments_data()
    comments_data(bind_rows(current, new_comments))
  })
  
  # Clear comments
  observeEvent(input$clear_comments, {
    showModal(modalDialog(
      title = "Clear all comments?",
      "This will remove all comments and generated personas.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear", "Clear All", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear, {
    comments_data(data.frame(comment = character(), stringsAsFactors = FALSE))
    personas_result(NULL)
    removeModal()
  })
  
  # Display comments
  output$comments_table <- renderTable({
    df <- comments_data()
    if (nrow(df) > 0) {
      df %>% mutate(ID = row_number()) %>% select(ID, Comment = comment)
    } else {
      data.frame(Message = "No comments yet. Add some above to get started!")
    }
  }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%")
  
  # Generate personas
  observeEvent(input$generate, {
    req(nrow(comments_data()) > 0)
    
    api_key <- Sys.getenv("ANTHROPIC_API_KEY")
    if (api_key == "" || nchar(api_key) == 0) {
      showModal(modalDialog(
        title = tags$div(icon("exclamation-triangle"), " API Key Required"),
        "Please set the ANTHROPIC_API_KEY environment variable before generating personas.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    # Show progress
    showModal(modalDialog(
      title = tags$div(icon("spinner", class = "fa-spin"), " Generating Personas..."),
      p("Analyzing comments and creating personas. This may take a moment."),
      footer = NULL
    ))
    
    tryCatch({
      comments <- comments_data()$comment
      num_personas <- input$num_personas
      
      # Create prompt
      prompt <- paste0(
        "Analyze these open-ended comments and identify ", num_personas, " distinct personas. ",
        "For each persona:\n",
        "1. Give it a descriptive name\n",
        "2. Write a brief description (2-3 sentences)\n",
        "3. Include 2-3 representative quotes from the actual comments that best exemplify this persona\n\n",
        "Comments:\n",
        paste(paste0(seq_along(comments), ". ", comments), collapse = "\n"),
        "\n\nFormat your response as JSON with this structure:\n",
        '{"personas": [{"name": "...", "description": "...", "quotes": ["...", "..."]}]}'
      )
      
      # Call Anthropic API
      response <- POST(
        url = "https://api.anthropic.com/v1/messages",
        add_headers(
          "x-api-key" = api_key,
          "anthropic-version" = "2023-06-01",
          "content-type" = "application/json"
        ),
        body = toJSON(list(
          model = "claude-3-5-sonnet-20240620",
          max_tokens = 4096,
          messages = list(
            list(
              role = "user",
              content = prompt
            )
          )
        ), auto_unbox = TRUE),
        encode = "json"
      )
      
      if (status_code(response) == 200) {
        result <- content(response, "parsed")
        
        # Extract the text content
        text_content <- result$content[[1]]$text
        
        # Parse JSON from the response
        # Try to extract JSON if it's wrapped in markdown code blocks
        json_text <- text_content
        if (grepl("```json", text_content)) {
          json_text <- sub(".*```json\\s*", "", text_content)
          json_text <- sub("```.*", "", json_text)
        } else if (grepl("```", text_content)) {
          json_text <- sub(".*```\\s*", "", text_content)
          json_text <- sub("```.*", "", json_text)
        }
        
        personas <- fromJSON(json_text)
        personas_result(personas)
        removeModal()
        
      } else {
        removeModal()
        showModal(modalDialog(
          title = tags$div(icon("circle-xmark"), " API Error"),
          p(paste("Error:", status_code(response))),
          p(content(response, "text")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = tags$div(icon("circle-xmark"), " Error"),
        p("Error generating personas:"),
        tags$code(e$message),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
  })
  
  # Display personas
  output$personas_output <- renderUI({
    if (is.null(personas_result())) {
      div(
        class = "text-center text-muted p-5",
        icon("users", class = "fa-3x mb-3"),
        h4("No personas generated yet"),
        p("Add some comments and click 'Generate Personas' to begin")
      )
    } else {
      personas <- personas_result()$personas
      
      persona_cards <- lapply(seq_len(nrow(personas)), function(i) {
        p <- personas[i, ]
        
        quotes_list <- lapply(p$quotes[[1]], function(q) {
          tags$blockquote(
            class = "blockquote border-start border-3 ps-3 py-2 mb-2",
            tags$p(class = "mb-0 fst-italic", q)
          )
        })
        
        card(
          class = "mb-3",
          card_header(
            class = "bg-primary text-white",
            h4(class = "mb-0", icon("user"), " ", p$name)
          ),
          card_body(
            p(class = "lead", p$description),
            h5(class = "mt-3", icon("quote-left"), " Representative Quotes"),
            div(class = "ms-3", quotes_list)
          )
        )
      })
      
      do.call(tagList, persona_cards)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
