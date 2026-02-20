library(shiny)
library(bslib)
library(tools)
library(colourpicker)
library(tidyverse)

# Define UI ----
ui <- page_fillable(
  titlePanel("Beatty Graph Display Tool"),

  layout_columns(
    col_widths = c(2, 2, 8),

    # ---- Files Card ----
    card(
      card_header("Files"),
      fileInput("file1", "Choose .TXT Files",
                accept = ".txt", multiple = TRUE),
      tableOutput("file.list")
    ),

    # ---- Colors Card ----
    card(
      card_header("Colors"),
      uiOutput("color_inputs")
    ),

    # ---- Plot Card (BIG) ----
    card(
      card_header("Plot"),

      navset_tab(
        id = "active_tab",
        nav_panel(
          "Scale vs Relative Length",
          value = "plot1",
          plotOutput("plot.out", height = "600px")
        ),
        nav_panel(
          "SCR Threshold",
          value = "plot2",

          selectInput(
            "scr_sample",
            "Select Sample to Plot:",
            choices = NULL  # We'll populate dynamically in the server
          ),

          numericInput(
            "scr_threshold",
            "SCR Threshold:",
            value = 1.086,
            step = 0.001
          ),

          plotOutput("plot.out.2", height = "600px")
        )
      ),

      # ---- Export Section ----
      card_header("Export Plot"),

      # Wrap everything in a div with padding
      div(
        style = "padding: 15px;",

        # Full-width text input for filename with margin-bottom
        div(
          style = "margin-bottom: 10px;",
          textInput(
            "out.file.name",
            "Export File Name",
            placeholder = "Enter name (no .ext)..."
          )
        ),

        # Full-width select input for file type with margin-bottom
        div(
          style = "margin-bottom: 10px;",
          selectInput(
            "select",
            "Select Export File Option Below:",
            list(
              "EPS" = ".eps",
              "PS" = ".ps",
              "PDF" = ".pdf",
              "JPEG" = ".jpg",
              "TIFF" = ".tiff",
              "PNG" = ".png",
              "BMP" = ".bmp",
              "SVG" = ".svg"
            )
          )
        ),

        # Download button centered with margin-top
        div(
          style = "text-align: center; margin-top: 10px;",
          downloadButton("download_plot", "Export Graph")
        )
      )
    )


  )
)

# Define server logic ----
server <- function(input, output, session) {

  uploaded_files <- reactive({
    req(input$file1)

    ext <- tools::file_ext(input$file1$datapath)
    validate(
      need(all(ext == "txt"), "Please upload only .txt files")
    )

    input$file1
  })

  #### Function List ----
  extract_title <- function(filename) {
    name <- tools::file_path_sans_ext(basename(filename))
    chars <- strsplit(name, "")[[1]]
    is_non_alpha <- !grepl("[A-Za-z]", chars) & chars != " "
    non_alpha_pos <- which(is_non_alpha)
    if (length(non_alpha_pos) >= 4) {
      cut_pos <- non_alpha_pos[4] - 1
      if (chars[cut_pos] == " ") cut_pos <- cut_pos - 1
      substr(name, 1, cut_pos)
    } else {
      name
    }
  }

  #### Data Extraction & File Table ----
  output$file.list <- renderTable({
    req(input$file1)
    data.frame("Uploaded Files" = input$file1$name)
  })

  #### Combine Data (fixed) ----
  combined_data <- reactive({
    req(input$file1)
    dataset <- list()

    for (i in seq_len(nrow(input$file1))) {
      filepath <- input$file1$datapath[i]
      filename <- input$file1$name[i]
      title <- extract_title(filename)

      # Read lines safely with encoding fallback
      raw_lines <- tryCatch(
        readLines(filepath, encoding = "UTF-8"),
        error = function(e) readLines(filepath, encoding = "latin1")
      )

      # Convert to ASCII to remove multibyte issues
      raw_lines <- iconv(raw_lines, from = "", to = "ASCII//TRANSLIT")

      # Find header line
      header_line <- grep("Scale.*Relative length.*Fractal.*R2", raw_lines, ignore.case = TRUE)[1]
      if (is.na(header_line)) next

      # Extract data lines
      data_lines <- raw_lines[(header_line + 1):length(raw_lines)]
      data_lines <- data_lines[!grepl("^\\s*$", data_lines)]        # remove empty lines
      data_lines <- data_lines[!grepl("^[^0-9+-]", data_lines)]    # remove non-numeric starting lines

      if (length(data_lines) == 0) next

      # Read table without header
      df <- read.table(text = data_lines, header = FALSE, fill = TRUE, stringsAsFactors = FALSE)

      # Ensure 4 columns
      if (ncol(df) < 4) df[(ncol(df)+1):4] <- NA
      else if (ncol(df) > 4) df <- df[, 1:4]

      colnames(df) <- c("Scale", "RelativeLength", "FractalComplexity", "R2")

      # Remove rows where Scale is not numeric
      df <- df %>% filter(!grepl("[^0-9eE.+-]", Scale))

      # Convert all columns to numeric safely
      df[] <- lapply(df, function(col) {
        if (is.list(col)) col <- unlist(col)
        suppressWarnings(as.numeric(col))
      })

      # Remove rows with NA in essential plotting columns
      df <- df %>% filter(!is.na(Scale) & !is.na(RelativeLength))

      dataset[[paste0("table: ", title)]] <- df
    }

    bind_rows(dataset, .id = "Sample")
  })

  # Unique samples
  samples <- reactive({
    req(combined_data())
    unique(combined_data()$Sample)
  })

  # Update sample select input
  observe({
    req(samples())
    updateSelectInput(session, "scr_sample", choices = samples(), selected = samples()[1])
  })

  # Dynamic color pickers
  output$color_inputs <- renderUI({
    req(samples())
    lapply(seq_along(samples()), function(i) {
      colourInput(
        inputId = paste0("color_", i),
        label = samples()[i],
        value = scales::hue_pal()(length(samples()))[i]
      )
    })
  })

  selected_colors <- reactive({
    req(samples())
    sapply(seq_along(samples()), function(i) input[[paste0("color_", i)]]) |>
      setNames(samples())
  })

  #### Main Plot ----
  plot_object <- reactive({
    req(combined_data(), selected_colors())
    ggplot(combined_data(), aes(x = Scale, y = RelativeLength, color = Sample)) +
      geom_line(linewidth = 0.7) +
      scale_color_manual(values = selected_colors()) +
      scale_x_log10(labels = scales::comma_format(accuracy = 1)) +
      labs(title = "Scale-Sensitive Fractal Analysis",
           x = "Scale (µm)",
           y = "Relative Length") +
      theme_minimal(base_size = 14)
  })

  output$plot.out <- renderPlot({ plot_object() })

  #### SCR Threshold Plot ----
  output$plot.out.2 <- renderPlot({
    req(combined_data(), input$scr_sample, input$scr_threshold)
    df <- combined_data() %>% filter(Sample == input$scr_sample)

    ggplot(df, aes(x = Scale, y = RelativeLength)) +
      geom_line(color = "blue", linewidth = 0.7) +
      scale_x_log10(labels = scales::label_comma(accuracy = 1)) +
      labs(title = paste("Scale-Sensitive Fractal Analysis -", input$scr_sample),
           x = "Scale (µm)", y = "Relative Length") +
      theme_minimal(base_size = 14) +
      geom_hline(yintercept = input$scr_threshold, linetype = "dashed", color = "red") +
      annotate("text", x = min(df$Scale, na.rm = TRUE),
               y = input$scr_threshold, label = "SCR Threshold",
               color = "red", vjust = -1)
  })

  #### Current Plot for Download ----
  current_plot <- reactive({
    req(input$active_tab)
    if (input$active_tab == "plot1") plot_object()
    else if (input$active_tab == "plot2") {
      req(input$scr_sample, input$scr_threshold)
      df <- combined_data() %>% filter(Sample == input$scr_sample)
      ggplot(df, aes(x = Scale, y = RelativeLength)) +
        geom_line(color = "blue", linewidth = 0.7) +
        scale_x_log10(labels = scales::label_comma()) +
        labs(title = paste("Scale-Sensitive Fractal Analysis -", input$scr_sample),
             x = "Scale (µm)", y = "Relative Length") +
        theme_minimal(base_size = 14) +
        geom_hline(yintercept = input$scr_threshold, linetype = "dashed", color = "red") +
        annotate("text", x = min(df$Scale, na.rm = TRUE),
                 y = input$scr_threshold, label = "SCR Threshold",
                 color = "red", vjust = -1)
    }
  })

  #### Download Handler ----
  output$download_plot <- downloadHandler(
    filename = function() {
      req(input$out.file.name)
      paste0(input$out.file.name, input$select)
    },
    content = function(file) {
      ext <- tools::file_ext(file)
      switch(ext,
             pdf  = pdf(file),
             eps  = postscript(file),
             ps   = postscript(file),
             svg  = svg(file),
             png  = png(file, width = 2000, height = 1600, res = 300),
             jpg  = jpeg(file, width = 2000, height = 1600, res = 300),
             tiff = tiff(file, width = 2000, height = 1600, res = 300),
             bmp  = bmp(file, width = 2000, height = 1600, res = 300)
      )
      print(current_plot())
      dev.off()
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
