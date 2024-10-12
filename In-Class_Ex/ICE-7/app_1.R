pacman::p_load(sf, spdep, tmap, tidyverse, sfdep, bslib, shiny)
hunan <- st_read(dsn = "data/geospatial", 
                 layer = "Hunan")
hunan2012 <- read_csv("data/aspatial/Hunan_2012.csv")
hunan_data <- left_join(hunan, hunan2012, by = c("County" = "County"))

ui <- fluidPage(
  titlePanel("Choropleth Mapping"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="variable",
                  label="Mapping variable",
                  choices=list("Gross Domestic Product, GDP" = "GDP",
                               "Gross Domestic Product Per Capita, GDPPC" = "GDPPC",
                               "Gross Industry Output, GIO" = "GIO",
                               "Output Value of Agriculture" = "OVA",
                               "Output Value of Service" = "OVS"),
                  selected = "GDPPC"),
      sliderInput(inputId = "classes",
                  label = "Number of classes",
                  min = 5,
                  max = 10,
                  value = c(6))
    ),
    mainPanel(
      plotOutput("mapPlot",
                 width = "100%",
                 height = 400)
    )
  )
)

server <- function(input, output){
  output$mapPlot <- renderPlot(
    {
      tmap_options(check.and.fix = TRUE) +
        tm_shape(hunan_data)+
        tm_fill(input$variable,
                n = input$classes,
                style = "quantile",
                palette= blues9) +
        tm_borders(lwd= 0.1, alpha = 1)
    }
  )
}

shinyApp(ui = ui, server = server)
