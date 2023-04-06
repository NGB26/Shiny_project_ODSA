

library(tidyverse)
library(readxl)
library(dplyr)
library(shiny)
library(descr)


library(haven)
df<- read_dta("C:/Users/Usuario/NICO/UTDT/MECAP/TESIS/BASES/2023/base_tesis_resp_2017_2021_30.1.23.dta")



# Define UI
ui <- fluidPage(
  titlePanel("Ingresos en la Argentina actual"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Selecciona el año:",
                  min = min(df$year),
                  max = max(df$year),
                  value = min(df$year),
                  step = 1)
    ),
    mainPanel(
      plotOutput("barplot"),
      plotOutput('boxplot')
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filtrar datos por año seleccionado
  data_year <- reactive({
    df %>% 
      filter(year == input$year)
  })
  
  # Calcular porcentaje de población en cada categoría de est_clase_4
  est_clase_4_percent <- reactive({
    data_year() %>% 
      group_by(est_clase_4) %>% 
      summarise(pct = n()/nrow(data_year())*100)
  })
  
  # Crear gráfico de barras
  output$barplot <- renderPlot({
    ggplot(est_clase_4_percent(), aes(x = est_clase_4, y = pct, fill=factor(est_clase_4))) +
      geom_bar(stat = "identity") +
      labs(x="Estrato", title = paste("Porcentaje de población en cada estrato socioeconómico en el año 20", input$year+1))+
      theme_bw()+ylim(0,40)+geom_text(aes(label=round(pct,2)), vjust=1.5, color='black')+
      scale_fill_discrete(name='Estrato socioeconómico',
                          breaks=c(1,2,3,4),
                          labels=c('Medio profesional','Medio no profesional', 'Bajo integrado', 'Bajo marginal'))
  })
  
  # Calcular estadísticas de ingresos por año y est_clase_4
  ingresos_year_est_clase_4 <- reactive({
    data_year() %>% 
      group_by (est_clase_4) %>% 
      summarise(media = mean(ing_lab_2021, na.rm = TRUE)/1000, 
                cv=var(ing_lab_2021, na.rm=T)/media
                )
  })
  
  # Crear gráfico de boxplot
  output$boxplot <- renderPlot({
    ggplot(ingresos_year_est_clase_4(), aes(x = factor(est_clase_4), y = media,
                                            color=factor(est_clase_4))) +
      geom_point(aes(size=cv)) +
      labs(title = "Media de ingresos por año y estrato socioeconómico", x="Estrato",
           y="Ingreso a precios 2021", caption="El tamaño de los circulos indica la variabilidad en los ingresos mediante el CV")+theme_bw()+
      ylim(0, 120)+
      scale_color_discrete(name='Estrato socioeconómico',
                          breaks=c(1,2,3,4),
                          labels=c('Medio profesional','Medio no profesional', 
                                   'Bajo integrado', 'Bajo marginal'))+
      guides(size=F)
  })
  
  # Agregar barra deslizante para cambiar el año
  sliderInput("year", "Año", min = 2017, max = 2021, value = 2021)
  
}

# Run the application 
shinyApp(ui = ui, server = server)



