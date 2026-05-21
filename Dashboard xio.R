# Creado para RStudio con Shiny
install.packages(c("gapminder", "shiny", "plotly", "tidyverse", "DT", "readxl", "ggthemes"))

library(gapminder)
library(shiny)
library(plotly)
library(tidyverse)
library(DT)
library(readxl)
library(ggthemes)

savehistory(file = ".Rhistory")
# datos
data("gapminder")

# Limpieza
data <- gapminder %>% 
  filter(year >= 1957) %>% 
  mutate(continent = as.factor(continent))

# Revisar valores nulos
colSums(is.na(data))

# Revisar estructura
str(data)

# Interfaz de Usuario
midashboard <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  
  titlePanel("Gapminder Global Insights (1957 - 2007)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Filtros de Exploración"),
      sliderInput("year_range", "Seleccione el Año:",
                  min = 1957, max = 2007, value = 2007, step = 5,
                  animate = animationOptions(interval = 1000, loop = FALSE)),
      
      checkboxGroupInput("continents", "Continentes:",
                         choices = levels(gapminder$continent),
                         selected = levels(gapminder$continent)),
      
      hr()
      # Texto
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # TAB 1: MAPAS Y TENDENCIAS
        tabPanel("Vista Global", 
                 fluidRow(
                   column(6, h4("Mapa: Esperanza de Vida (Burbujas)"), plotlyOutput("map_lifeexp")),
                   column(6, h4("Mapa: Densidad de PIB"), plotlyOutput("map_gdp"))
                 ),
                 fluidRow(
                   column(6, h4("Composición Demográfica (Barras Apiladas)"), plotlyOutput("stacked_demographics")),
                   column(6, h4("Tendencia: Esperanza de Vida"), plotlyOutput("trend_life"))
                 )
        ),
        
        # TAB 2: ANÁLISIS DE IMPACTO
        tabPanel("Análisis y Regresión",
                 fluidRow(
                   column(12, h4("Relación PIB vs Esperanza de Vida (Burbujas con Regresión)"),
                          plotlyOutput("bubble_plot"))
                 ),
                 fluidRow(
                   column(6, h4("Impacto: Ricos vs Pobres (Distribución)"), plotlyOutput("rich_poor_impact")),
                   column(6, h4("Top 10 Países más Ricos"), plotlyOutput("top_rich"))
                 )
        ),
        
        # TAB 3: DATOS
        tabPanel("Explorador de Datos",
                 DTOutput("data_table")
        )
      )
    )
  )
)

# Lógica del Servidor (Server)
server <- function(input, output) {
  
  # Filtro reactivo de datos
  filtered_data <- reactive({
    gapminder %>%
      filter(year == input$year_range,
             continent %in% input$continents)
  })
  
  # 1. Mapa Esperanza de Vida con Burbujas
  output$map_lifeexp <- renderPlotly({
    plot_geo(filtered_data()) %>%
      add_trace(
        z = ~lifeExp, 
        color = ~lifeExp, 
        colors = 'Reds',
        text = ~paste("País:", country, "<br>Esp. Vida:", lifeExp, "años<br>Pob:", pop),
        hoverinfo = "text",
        locations = ~country, 
        locationmode = 'country names'
      ) %>%
      layout(geo = list(projection = list(type = 'natural earth')))
  })
  
  # 2. Mapa PIB
  output$map_gdp <- renderPlotly({
    plot_geo(filtered_data()) %>%
      add_trace(z = ~gdpPercap, color = ~gdpPercap, colors = 'Blues',
                text = ~country, locations = ~country, locationmode = 'country names') %>%
      layout(geo = list(projection = list(type = 'natural earth')))
  })
  
  # 3. Gráfico de Barras Apiladas (Composición Demográfica)
  output$stacked_demographics <- renderPlotly({
    df_stack <- filtered_data() %>%
      group_by(continent) %>%
      summarise(total_pop = sum(as.numeric(pop)))
    
    plot_ly(df_stack, x = ~continent, y = ~total_pop, type = 'bar', color = ~continent) %>%
      layout(xaxis = list(title = "continente"), yaxis = list(title = "Población Total"), barmode = 'stack')
  })
  
  # 4. Tendencia Esperanza de Vida
  output$trend_life <- renderPlotly({
    df_trend <- gapminder %>% filter(continent %in% input$continents) %>%
      group_by(year) %>% summarise(prom_vida = mean(lifeExp))
    
    plot_ly(df_trend, x = ~year, y = ~prom_vida, type = 'scatter', mode = 'lines',
            fill = 'tozeroy', fillcolor = 'rgba(225, 29, 72, 0.2)') %>%
      layout(xaxis = list(title = "año"), yaxis = list(title = "prom_vida"))
  })
  
  # 5. Regresión y Burbujas con Tooltips Mejorados
  output$bubble_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = gdpPercap, y = lifeExp, size = pop, color = continent, 
                                     text = paste("País:", country, "<br>PIB:", gdpPercap, "<br>Esp:", lifeExp))) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
      scale_x_log10() +
      theme_minimal() +
      labs(x = "PIB per Capita (log)", y = "Esperanza de Vida")
    
    ggplotly(p, tooltip = "text")
  })
  
  # 6. Gráfico Ricos vs Pobres (Distribución de PIB) - Solo Boxplot
  output$rich_poor_impact <- renderPlotly({
    plot_ly(filtered_data(), y = ~gdpPercap, color = ~continent, type = "box", boxpoints = FALSE) %>%
      layout(yaxis = list(title = "PIB per Capita (Distribución)"))
  })
  
  # 7. Top 10 Ricos
  output$top_rich <- renderPlotly({
    top10 <- filtered_data() %>% arrange(desc(gdpPercap)) %>% head(10)
    plot_ly(top10, x = reorder(top10$country, top10$gdpPercap), y = ~gdpPercap, 
            type = 'bar', marker = list(color = '#e11d48')) %>%
      layout(xaxis = list(title = "País"), yaxis = list(title = "PIB per Capita"))
  })
  
  # 8. Tabla de Datos
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

# Ejecutar la Aplicación
shinyApp(ui = midashboard, server = server)