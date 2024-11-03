library(shiny)
library(leaflet)
library(DT)
library(sf)
library(viridis)

ui <- fluidPage(
  tags$head(
    tags$style(HTML('
            body { font-family: Arial, sans-serif; background: #f5f5f5; }
            .well { 
                background-color: white; 
                border-radius: 4px; 
                box-shadow: 0 1px 3px rgba(0,0,0,0.1);
                margin-bottom: 15px;
            }
            h4 { font-size: 16px; margin: 0 0 10px 0; color: #333; }
            .stats-panel { 
                background: #f8f9fa; 
                padding: 15px; 
                border-radius: 4px; 
                margin-top: 20px; 
                font-family: monospace;
            }
            .tooltip-icon {
                color: #666;
                cursor: help;
                margin-left: 5px;
            }
            .metric-header {
                display: inline-block;
            }
            .table-condensed {
                font-size: 12px;
            }
            .table-condensed td, .table-condensed th {
                padding: 4px;
            }
        '))
  ),
  
  titlePanel("Jakarta GWR Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Add tab selection
      tabsetPanel(
        id = "sidebar_tabs",
        
        # Tab 1: Original GWR Analysis
        tabPanel("GWR Analysis",
                 # Model Selection Panel
                 wellPanel(
                   h4("Model Selection"),
                   radioButtons("model_type", NULL,
                                choices = c("Fixed Bandwidth" = "fixed",
                                            "Adaptive Bandwidth" = "adaptive")),
                   
                   h4("Display Variable"),
                   selectInput("variable", NULL,
                               choices = c(
                                 "Local R²" = "r2",
                                 "School Density Coefficient" = "coef"
                               ))
                 ),
                 
                 # Model Comparison Panel
                 wellPanel(
                   div(
                     h4(
                       span(class = "metric-header", "Model Comparison"),
                       tags$span(
                         class = "tooltip-icon",
                         title = "AICc measures model fit. Lower values indicate better fit.",
                         icon("info-circle")
                       )
                     )
                   ),
                   tableOutput("model_summary")
                 ),
                 
                 # Download Button
                 wellPanel(
                   downloadButton("download_data", "Download Results",
                                  class = "btn-block")
                 )
        ),
        
        # Tab 2: Opportunity Analysis
        tabPanel("Opportunity Analysis",
                 wellPanel(
                   h4("Opportunity Zones"),
                   p("Showing priority areas for school development in North Jakarta"),
                   br(),
                   selectInput("opp_display", "Display Metric",
                               choices = c(
                                 "Opportunity Score" = "score",
                                 "School Density" = "density",
                                 "Local R²" = "r2"
                               ))
                 ),
                 
                 wellPanel(
                   h4("Summary Statistics"),
                   verbatimTextOutput("opp_summary")
                 ),
                 
                 wellPanel(
                   downloadButton("download_opp", "Download Opportunity Analysis",
                                  class = "btn-block")
                 )
        )
      ),
      width = 3
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.sidebar_tabs == 'GWR Analysis'",
        h4(textOutput("analysis_title")),
        leafletOutput("map", height = "500px"),
        div(class = "stats-panel",
            h4("Detailed Results"),
            DTOutput("results_table"))
      ),
      
      conditionalPanel(
        condition = "input.sidebar_tabs == 'Opportunity Analysis'",
        h4("Opportunity Zones Analysis"),
        leafletOutput("opp_map", height = "500px"),
        div(class = "stats-panel",
            h4("Priority Areas"),
            DTOutput("opp_table"))
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  # Load both datasets
  gwr_results <- readRDS("data/rds/gwr_results.rds")
  opp_results <- readRDS("data/rds/opp_score.rds")
  
  # Create reactive to identify complete cases once
  complete_data <- reactive({
    # Get indices where we have complete data
    complete_indices <- which(!is.na(gwr_results$data$school_density))
    
    # Create list of aligned data
    list(
      batas = gwr_results$spatial$batas[complete_indices,],
      higher_ed_ratio = gwr_results$data$higher_ed_ratio,
      school_density = gwr_results$data$school_density,
      fixed = list(
        local_R2 = gwr_results$fixed$local_R2,
        coefficients = gwr_results$fixed$coefficients,
        bandwidth = gwr_results$fixed$bandwidth,
        diagnostics = gwr_results$fixed$diagnostics
      ),
      adaptive = list(
        local_R2 = gwr_results$adaptive$local_R2,
        coefficients = gwr_results$adaptive$coefficients,
        bandwidth = gwr_results$adaptive$bandwidth,
        diagnostics = gwr_results$adaptive$diagnostics
      ),
      indices = complete_indices
    )
  })
  
  # Get values based on selection
  get_values <- reactive({
    data <- complete_data()
    if(input$variable == "r2") {
      if(input$model_type == "fixed") {
        data$fixed$local_R2
      } else {
        data$adaptive$local_R2
      }
    } else {
      if(input$model_type == "fixed") {
        data$fixed$coefficients
      } else {
        data$adaptive$coefficients
      }
    }
  })
  
  output$analysis_title <- renderText({
    paste(input$model_type, "GWR:",
          if(input$variable == "r2") "Local R²" else "School Density Coefficient")
  })
  
  # Model comparison table
  output$model_summary <- renderTable({
    data <- complete_data()
    
    data.frame(
      `Model Type` = c("Fixed", "Adaptive"),
      Bandwidth = c(
        sprintf("%.2f", data$fixed$bandwidth),
        sprintf("%d neighbors", data$adaptive$bandwidth)
      ),
      AICc = c(
        sprintf("%.2f", data$fixed$diagnostics$AICc),
        sprintf("%.2f", data$adaptive$diagnostics$AICc)
      )
    )
  }, 
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE,
  align = 'c',
  width = "100%",
  rownames = FALSE)
  
  # Original GWR Map
  output$map <- renderLeaflet({
    data <- complete_data()
    values <- get_values()
    
    pal <- if(input$variable == "r2") {
      colorNumeric("viridis", domain = values)
    } else {
      colorNumeric("RdBu", domain = range(values), reverse = TRUE)
    }
    
    leaflet(data$batas) %>%
      addTiles() %>%
      setView(lng = 106.8456, lat = -6.2088, zoom = 11) %>%
      addPolygons(
        fillColor = ~pal(values),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste(
          "District:", district, "<br>",
          "Subdistrict:", subdistrict, "<br>",
          if(input$variable == "r2") "Local R²: " else "Coefficient: ",
          round(values, 4)
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = values,
        title = if(input$variable == "r2") "Local R²" else "Coefficient"
      )
  })
  
  # Opportunity Map
  output$opp_map <- renderLeaflet({
    req(input$opp_display)
    
    districts <- opp_results$spatial$districts_wgs84
    values <- switch(input$opp_display,
                     "score" = districts$opportunity_score,
                     "density" = districts$senior_high_density,
                     "r2" = districts$local_R2)
    
    title <- switch(input$opp_display,
                    "score" = "Opportunity Score",
                    "density" = "School Density",
                    "r2" = "Local R²")
    
    pal <- colorNumeric("viridis", domain = values, na.color = "gray")
    
    leaflet(districts) %>%
      addTiles() %>%
      setView(lng = 106.8456, lat = -6.2088, zoom = 11) %>%
      addPolygons(
        fillColor = ~pal(values),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste(
          "District:", district, "<br>",
          title, ": ", round(values, 4), "<br>",
          "Current Density:", round(senior_high_density, 2), "<br>",
          "Priority Level:", recommendation
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = values,
        title = title,
        labFormat = labelFormat(digits = 3)
      )
  })
  
  # Results table for GWR
  output$results_table <- renderDT({
    data <- complete_data()
    
    datatable(
      data.frame(
        District = data$batas$district,
        Subdistrict = data$batas$subdistrict,
        Higher_Ed_Ratio = round(data$higher_ed_ratio, 4),
        School_Density = data$school_density,
        Fixed_Coefficient = round(data$fixed$coefficients, 4),
        Adaptive_Coefficient = round(data$adaptive$coefficients, 4)
      ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      colnames = c(
        "District",
        "Subdistrict",
        "Higher Education Ratio",
        "School Density",
        "Fixed GWR Coefficient",
        "Adaptive GWR Coefficient"
      )
    )
  })
  
  # Results table for opportunity analysis
  output$opp_table <- renderDT({
    datatable(
      opp_results$summary,
      options = list(
        pageLength = 6,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      colnames = c(
        "District",
        "Current Density",
        "Local R²",
        "Local Coefficient",
        "Opportunity Score",
        "Recommendation"
      )
    )
  })
  
  # Summary statistics for opportunity analysis
  output$opp_summary <- renderText({
    paste(
      "Target City:", opp_results$target_city, "\n",
      "Average School Density:", round(opp_results$stats$avg_density, 2), "\n",
      "Average Local R²:", round(opp_results$stats$avg_R2, 4), "\n",
      "High Priority Districts:", opp_results$stats$districts_high_priority, "\n",
      "Medium Priority Districts:", opp_results$stats$districts_med_priority
    )
  })
  
  # Original download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("gwr_analysis_", format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      data <- complete_data()
      
      write.csv(
        data.frame(
          District = data$batas$district,
          Subdistrict = data$batas$subdistrict,
          Higher_Ed_Ratio = data$higher_ed_ratio,
          School_Density = data$school_density,
          Fixed_Coefficient = data$fixed$coefficients,
          Adaptive_Coefficient = data$adaptive$coefficients
        ),
        file, 
        row.names = FALSE
      )
    }
  )
  
  # Download handler for opportunity analysis
  output$download_opp <- downloadHandler(
    filename = function() {
      paste0("opportunity_analysis_", format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(opp_results$summary, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)