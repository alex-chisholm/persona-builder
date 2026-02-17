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
  
  title = div(
    h2(class = "mb-1", "Persona Builder"),
    p(class = "text-muted mb-0", style = "font-size: 0.9rem;", 
      "Turn open ended comments into targeted personas")
  ),
  
  sidebar = sidebar(
    width = 400,
    
    card(
      card_header("Upload CSV"),
      card_body(
        fileInput("csv_file", NULL,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"),
        uiOutput("column_selector")
      ),
      card_footer(
        class = "text-muted small",
        div("Maximum: 2MB file size, 100 rows"),
        div(
          class = "mt-2",
          "or ",
          actionLink("load_example", "use our example data", 
                     icon = icon("file-csv"))
        )
      )
    ),
    
    card(
      card_header("Generate"),
      card_body(
        numericInput("num_personas", "Number of Personas:", 
                     value = 3, min = 2, max = 10, step = 1),
        uiOutput("generate_button")
      )
    ),
    
    card(
      card_body(
        value_box(
          title = "Comments Loaded",
          value = textOutput("comment_count_value")
        ),
        verbatimTextOutput("api_status", placeholder = TRUE)
      )
    )
  ),
  
  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      "Comments",
      card_body(
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
  generating <- reactiveVal(FALSE)
  current_quote_index <- reactiveVal(1)
  
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
  
  # Render generate button - disabled until comments are loaded
  output$generate_button <- renderUI({
    has_comments <- nrow(comments_data()) > 0
    
    if (has_comments) {
      actionButton("generate", "Generate Personas", 
                   class = "btn-success w-100",
                   icon = icon("wand-magic-sparkles"))
    } else {
      tags$button(
        class = "btn btn-success w-100 disabled",
        disabled = "disabled",
        icon("wand-magic-sparkles"), " Generate Personas"
      )
    }
  })
  
  # Load example data
  observeEvent(input$load_example, {
    tryCatch({
      example_df <- read_csv("example.csv", show_col_types = FALSE)
      
      # Check if there's a 'comment' column, otherwise use first column
      if ("comment" %in% names(example_df)) {
        example_comments <- example_df %>%
          select(comment) %>%
          filter(!is.na(comment), nchar(trimws(as.character(comment))) > 0) %>%
          mutate(comment = as.character(comment))
      } else {
        # Use first column as comments
        example_comments <- example_df %>%
          select(comment = 1) %>%
          filter(!is.na(comment), nchar(trimws(as.character(comment))) > 0) %>%
          mutate(comment = as.character(comment))
      }
      
      comments_data(example_comments)
      
      showNotification("Example data loaded successfully!", 
                       type = "message", 
                       duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error loading example data:", e$message), 
                       type = "error", 
                       duration = 5)
    })
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
    
    # Check file size (2MB limit)
    file_size_mb <- file.info(input$csv_file$datapath)$size / (1024^2)
    if (file_size_mb > 2) {
      showModal(modalDialog(
        title = tags$div(icon("exclamation-triangle"), " File Too Large"),
        p(paste0("The uploaded file is ", round(file_size_mb, 2), "MB.")),
        p("Please upload a CSV file smaller than 2MB."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    df <- read_csv(input$csv_file$datapath, show_col_types = FALSE)
    
    # Check row count (100 row limit)
    if (nrow(df) > 100) {
      showModal(modalDialog(
        title = tags$div(icon("exclamation-triangle"), " Too Many Rows"),
        p(paste0("The uploaded file contains ", nrow(df), " rows.")),
        p("Please upload a CSV file with 100 rows or fewer."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    new_comments <- df %>%
      select(comment = !!sym(input$comment_column)) %>%
      filter(!is.na(comment), nchar(trimws(as.character(comment))) > 0) %>%
      mutate(comment = as.character(comment))
    
    current <- comments_data()
    comments_data(bind_rows(current, new_comments))
  }, ignoreInit = TRUE)
  
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
    
    # Start generating
    generating(TRUE)
    current_quote_index(1)
    
    # Get sample comments to display
    all_comments <- comments_data()$comment
    sample_comments <- if(length(all_comments) > 5) {
      sample(all_comments, min(5, length(all_comments)))
    } else {
      all_comments
    }
    
    # Show initial progress modal with sample comments
    showModal(modalDialog(
      title = tags$div(icon("spinner", class = "fa-spin"), " Generating Personas..."),
      div(
        p("Analyzing comments and creating personas..."),
        tags$hr(),
        div(
          class = "p-3 bg-light rounded",
          tags$em(class = "text-muted small", "Sample comments being analyzed:"),
          tags$ul(
            class = "mt-2 mb-0",
            lapply(sample_comments, function(comment) {
              tags$li(
                style = "font-style: italic; margin-bottom: 8px;",
                paste0('"', strtrim(comment, 100), if(nchar(comment) > 100) '..."' else '"')
              )
            })
          )
        )
      ),
      footer = NULL,
      size = "m"
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
          model = "claude-sonnet-4-5",
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
        generating(FALSE)
        removeModal()
        
        # Switch to Personas tab to show results
        nav_select("main_tabs", selected = "Personas")
        
      } else {
        generating(FALSE)
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
      generating(FALSE)
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
      
      # Create accordion panels for each persona
      persona_panels <- lapply(seq_len(nrow(personas)), function(i) {
        p <- personas[i, ]
        
        quotes_list <- lapply(p$quotes[[1]], function(q) {
          tags$blockquote(
            class = "blockquote border-start border-3 ps-3 py-2 mb-2",
            tags$p(class = "mb-0 fst-italic", q)
          )
        })
        
        accordion_panel(
          title = div(icon("user"), " ", p$name),
          value = paste0("persona_", i),
          div(
            p(class = "lead mb-3", p$description),
            h5(class = "mt-3 mb-3", icon("quote-left"), " Representative Quotes"),
            div(class = "ms-3", quotes_list)
          )
        )
      })
      
      # Return accordion with all persona panels
      accordion(
        id = "personas_accordion",
        multiple = FALSE,  # Only one panel open at a time
        !!!persona_panels
      )
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
