install.packages(c("gapminder", "shiny", "plotly", "tidyverse", "DT", "readxl", "ggthemes", "shinythemes", "scales"))

library(gapminder)
library(shiny)
library(plotly)
library(tidyverse)
library(DT)
library(readxl)
library(ggthemes)
library(shinythemes)
library(scales)

data("gapminder")

# ============================================================
# VARIABLES MACROECONÓMICAS DERIVADAS
# ============================================================

gapminder_macro <- gapminder %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    gdp_growth=
      (gdpPercap-lag(gdpPercap))/
      lag(gdpPercap)*100,
    
    pop_growth=
      (pop-lag(pop))/
      lag(pop)*100,
    
    life_growth=
      (lifeExp-lag(lifeExp))/
      lag(lifeExp)*100,
    
    productivity_index=
      gdpPercap/lifeExp,
    
    productivity_population=
      gdpPercap/pop,
    
    health_efficiency=
      lifeExp/gdpPercap,
    
    welfare_index=
      (lifeExp*gdpPercap)/pop,
    
    development_index=
      sqrt(gdpPercap*lifeExp),
    
    prosperity_score=
      (gdpPercap*lifeExp)/1000,
    
    inequality_proxy=
      log(gdpPercap)/lifeExp,
    
    demographic_pressure=
      pop/gdpPercap,
    
    economic_efficiency=
      gdpPercap/(pop/1000000),
    
    hdi_proxy=
      (lifeExp/85)*(log(gdpPercap)/log(50000)),
    
    log_income=
      log(gdpPercap),
    
    wealth_index=
      gdpPercap*pop,
    
    welfare_ratio=
      lifeExp/pop,
    
    stability_index=
      gdp_growth/pop_growth,
    
    consumption_proxy=
      gdpPercap*0.65,
    
    savings_proxy=
      gdpPercap*0.18,
    
    investment_proxy=
      gdpPercap*0.24,
    
    gov_spending_proxy=
      gdpPercap*0.20,
    
    exports_proxy=
      gdpPercap*0.30,
    
    imports_proxy=
      gdpPercap*0.28,
    
    trade_balance=
      exports_proxy-imports_proxy,
    
    openness_index=
      (exports_proxy+imports_proxy)/gdpPercap,
    
    debt_proxy=
      gdpPercap*0.45,
    
    debt_burden=
      debt_proxy/gdpPercap,
    
    inflation_proxy=
      abs(gdp_growth-pop_growth),
    
    unemployment_proxy=
      15-life_growth,
    
    human_capital_proxy=
      lifeExp*log(gdpPercap),
    
    competitiveness_index=
      exports_proxy/imports_proxy,
    
    economic_risk=
      debt_proxy/exports_proxy,
    
    volatility_proxy=
      sd(gdpPercap),
    
    total_gdp=
      gdpPercap*pop
    
  ) %>%
  ungroup() %>%
  replace_na(list(
    gdp_growth=0,
    pop_growth=0,
    life_growth=0,
    stability_index=0
  ))

# ============================================================
# UI
# ============================================================

midashboard <- fluidPage(
  
  theme=shinytheme("flatly"),
  
  titlePanel(
    "Gapminder Global Insights Dashboard"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width=3,
      
      h3("Filtros de Exploración"),
      
      sliderInput(
        "year_range",
        "Seleccione el Año:",
        min=1952,
        max=2007,
        value=2007,
        step=5,
        animate=animationOptions(interval=1000)
      ),
      
      checkboxGroupInput(
        "continents",
        "Continentes:",
        choices=levels(gapminder$continent),
        selected=levels(gapminder$continent)
      ),
      
      hr(),
      
      h4("Indicadores"),
      
      verbatimTextOutput("kpi_global_gdp"),
      verbatimTextOutput("kpi_global_life"),
      verbatimTextOutput("kpi_population")
      
    ),
    
    mainPanel(
      
      width=9,
      
      tabsetPanel(
        
        # ====================================================
        # TAB 1
        # ====================================================
        
        tabPanel(
          
          "Vista Global",
          
          fluidRow(
            
            column(
              6,
              h4("Mapa Esperanza de Vida"),
              plotlyOutput("map_lifeexp")
            ),
            
            column(
              6,
              h4("Mapa PIB"),
              plotlyOutput("map_gdp")
            )
          ),
          
          fluidRow(
            
            column(
              6,
              h4("Demografía"),
              plotlyOutput("stacked_demographics")
            ),
            
            column(
              6,
              h4("Tendencia Vida"),
              plotlyOutput("trend_life")
            )
          )
        ),
        
        # ====================================================
        # TAB 2
        # ====================================================
        
        tabPanel(
          
          "Regresiones y Boxplots",
          
          fluidRow(
            
            column(
              12,
              h4("Regresión PIB vs Vida"),
              plotlyOutput("bubble_plot")
            )
          ),
          
          fluidRow(
            
            column(
              6,
              h4("Distribución PIB"),
              plotlyOutput("rich_poor_impact")
            ),
            
            column(
              6,
              h4("Top 10 Ricos"),
              plotlyOutput("top_rich")
            )
          ),
          
          fluidRow(
            
            column(
              6,
              h4("Boxplot Esperanza Vida"),
              plotlyOutput("life_box")
            ),
            
            column(
              6,
              h4("Boxplot Crecimiento PIB"),
              plotlyOutput("growth_box")
            )
          )
        ),
        
        # ====================================================
        # TAB 3
        # ====================================================
        
        tabPanel(
          
          "Histogramas",
          
          fluidRow(
            
            column(
              6,
              h4("Histograma PIB"),
              plotlyOutput("hist_gdp")
            ),
            
            column(
              6,
              h4("Histograma Esperanza Vida"),
              plotlyOutput("hist_life")
            )
          ),
          
          fluidRow(
            
            column(
              6,
              h4("Histograma Crecimiento"),
              plotlyOutput("hist_growth")
            ),
            
            column(
              6,
              h4("Histograma Productividad"),
              plotlyOutput("hist_productivity")
            )
          )
        ),
        
        # ====================================================
        # TAB 4
        # ====================================================
        
        tabPanel(
          
          "Explorador de Datos",
          
          DTOutput("data_table")
          
        )
        
      )
      
    )
    
  )
  
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output) {
  
  filtered_data <- reactive({
    
    gapminder_macro %>%
      filter(
        year==input$year_range,
        continent %in% input$continents
      )
    
  })
  
  # ==========================================================
  # KPIs
  # ==========================================================
  
  output$kpi_global_gdp <- renderText({
    
    paste(
      "PIB Promedio:",
      round(mean(filtered_data()$gdpPercap),2)
    )
    
  })
  
  output$kpi_global_life <- renderText({
    
    paste(
      "Esperanza Vida:",
      round(mean(filtered_data()$lifeExp),2)
    )
    
  })
  
  output$kpi_population <- renderText({
    
    paste(
      "Población Total:",
      comma(sum(filtered_data()$pop))
    )
    
  })
  
  # ==========================================================
  # MAPAS
  # ==========================================================
  
  output$map_lifeexp <- renderPlotly({
    
    plot_geo(filtered_data()) %>%
      add_trace(
        z=~lifeExp,
        color=~lifeExp,
        colors="Reds",
        locations=~country,
        locationmode="country names"
      )
    
  })
  
  output$map_gdp <- renderPlotly({
    
    plot_geo(filtered_data()) %>%
      add_trace(
        z=~gdpPercap,
        color=~gdpPercap,
        colors="Blues",
        locations=~country,
        locationmode="country names"
      )
    
  })
  
  # ==========================================================
  # DEMOGRAFÍA
  # ==========================================================
  
  output$stacked_demographics <- renderPlotly({
    
    df_stack <- filtered_data() %>%
      group_by(continent) %>%
      summarise(
        total_pop=sum(pop)
      )
    
    plot_ly(
      df_stack,
      x=~continent,
      y=~total_pop,
      type="bar",
      color=~continent
    )
    
  })
  
  # ==========================================================
  # TENDENCIA VIDA
  # ==========================================================
  
  output$trend_life <- renderPlotly({
    
    df_trend <- gapminder %>%
      filter(continent %in% input$continents) %>%
      group_by(year) %>%
      summarise(
        prom_vida=mean(lifeExp)
      )
    
    plot_ly(
      df_trend,
      x=~year,
      y=~prom_vida,
      type="scatter",
      mode="lines"
    )
    
  })
  
  # ==========================================================
  # REGRESIÓN
  # ==========================================================
  
  output$bubble_plot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x=gdpPercap,
        y=lifeExp,
        size=pop,
        color=continent
      )
    )+
      geom_point(alpha=0.7)+
      geom_smooth(
        method="lm",
        se=FALSE,
        color="black"
      )+
      scale_x_log10()+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # BOXPLOTS
  # ==========================================================
  
  output$rich_poor_impact <- renderPlotly({
    
    plot_ly(
      filtered_data(),
      y=~gdpPercap,
      color=~continent,
      type="box"
    )
    
  })
  
  output$life_box <- renderPlotly({
    
    plot_ly(
      filtered_data(),
      y=~lifeExp,
      color=~continent,
      type="box"
    )
    
  })
  
  output$growth_box <- renderPlotly({
    
    plot_ly(
      filtered_data(),
      y=~gdp_growth,
      color=~continent,
      type="box"
    )
    
  })
  
  # ==========================================================
  # TOP RICOS
  # ==========================================================
  
  output$top_rich <- renderPlotly({
    
    top10 <- filtered_data() %>%
      arrange(desc(gdpPercap)) %>%
      head(10)
    
    plot_ly(
      top10,
      x=~reorder(country,gdpPercap),
      y=~gdpPercap,
      type="bar"
    )
    
  })
  
  # ==========================================================
  # HISTOGRAMAS
  # ==========================================================
  
  output$hist_gdp <- renderPlotly({
    
    plot_ly(
      filtered_data(),
      x=~gdpPercap,
      type="histogram"
    )
    
  })
  
  output$hist_life <- renderPlotly({
    
    plot_ly(
      filtered_data(),
      x=~lifeExp,
      type="histogram"
    )
    
  })
  
  output$hist_growth <- renderPlotly({
    
    plot_ly(
      filtered_data(),
      x=~gdp_growth,
      type="histogram"
    )
    
  })
  
  output$hist_productivity <- renderPlotly({
    
    plot_ly(
      filtered_data(),
      x=~productivity_index,
      type="histogram"
    )
    
  })
  
  # ==========================================================
  # TABLA
  # ==========================================================
  
  output$data_table <- renderDT({
    
    datatable(
      filtered_data(),
      options=list(pageLength=15)
    )
    
  })
  
}

# ============================================================
# EJECUTAR APP
# ============================================================

shinyApp(ui=midashboard,server=server)