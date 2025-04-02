library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Simulación del Teorema del Límite Central"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Selecciona la distribución:",
                  choices = c("Normal" = "norm",
                              "Uniforme" = "unif",
                              "Exponencial" = "exp")),
      numericInput("mean", "Media de la población:", value = 6000),
      numericInput("sd", "Desviación estándar de la población:", value = 200, min = 0.1),
      numericInput("n", "Tamaño de la muestra (n):", value = 30, min = 2),
      numericInput("num_samples", "Número de muestras:", value = 1000, min = 1),
      actionButton("generate", "Generar Muestras")
    ),
    
    mainPanel(
      h3("Distribución de la Variable Original"),
      plotOutput("histOriginal"),
      
      h3("Distribución del Promedio de las Muestras"),
      plotOutput("histSampleMean"),
      
      h3("Distribución de los Datos de Cada Muestra"),
      plotOutput("histSampleDist"),
      
      h3("Estadísticas de la Primera Muestra"),
      verbatimTextOutput("stats")
    )
  )
)

# Servidor
server <- function(input, output) {
  
  # Función reactiva para generar datos y muestras
  data <- eventReactive(input$generate, {
    set.seed(123)
    n_total <- 250000  # Tamaño grande de la población
    population <- switch(input$distribution,
                         "norm" = rnorm(n_total, mean = input$mean, sd = input$sd),
                         "unif" = runif(n_total, min = input$mean - input$sd * sqrt(3), max = input$mean + input$sd * sqrt(3)),
                         "exp" = rexp(n_total, rate = 1 / input$mean))
    
    # Generar muestras y calcular estadísticas
    samples <- replicate(input$num_samples, sample(population, input$n), simplify = FALSE)
    
    sample_means <- sapply(samples, mean)
    sample_sds <- sapply(samples, sd)
    sample_ses <- sample_sds / sqrt(input$n)
    ci_lower <- sample_means - 1.96 * sample_ses
    ci_upper <- sample_means + 1.96 * sample_ses
    
    # Convertir datos de muestras a formato largo para ggplot2 (hasta 9 muestras para visualización)
    if (input$num_samples > 9) {
      samples <- samples[1:9]
    }
    
    sample_data <- do.call(rbind, lapply(seq_along(samples), function(i) {
      data.frame(Valor = samples[[i]], 
                 Muestra = factor(i), 
                 Promedio = sample_means[i], 
                 CI_Lower = ci_lower[i], 
                 CI_Upper = ci_upper[i])
    }))
    
    list(population = population, 
         sample_means = sample_means, 
         sample_sds = sample_sds, 
         ci_lower = ci_lower, 
         ci_upper = ci_upper, 
         sample_data = sample_data,
         first_sample_mean = sample_means[1],
         first_sample_ci = c(ci_lower[1], ci_upper[1]))
  })
  
  # Histograma de la variable original
  output$histOriginal <- renderPlot({
    ggplot(data.frame(x = data()$population), aes(x)) +
      geom_histogram(bins = 50, fill = "blue", alpha = 0.5, color = "black") +
      geom_vline(xintercept = input$mean, color = "red") + 
      labs(title = "Cantidad de palabras manejadas por estudiantes (n = 250k)", x = "Valor", y = "Frecuencia") +
      theme_minimal()
  })
  
  # Histograma del promedio de las muestras
  output$histSampleMean <- renderPlot({
    ggplot(data.frame(x = data()$sample_means), aes(x)) +
      geom_histogram(bins = 50, fill = "red", alpha = 0.5, color = "black") +
      labs(title = paste0("Cantidad de palabras manejadas por estudiantes (n=", input$n, ")"), 
           x = "Media Muestral", y = "Frecuencia") +
      theme_minimal()
  })
  
  # Histograma de la distribución de cada muestra con IC
  output$histSampleDist <- renderPlot({
    ggplot(data()$sample_data, aes(x = Valor)) +
      geom_histogram(bins = 20, fill = "green", alpha = 0.5, color = "black") +
      geom_vline(aes(xintercept = Promedio), color = "red", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = CI_Lower), color = "blue", linetype = "dotted", size = 1) +
      geom_vline(aes(xintercept = CI_Upper), color = "blue", linetype = "dotted", size = 1) +
      facet_wrap(~Muestra, scales = "free") +
      labs(title =  paste0("Cantidad de palabras manejadas por estudiantes (Muestras n:", input$n, ")"),
           x = "Valor", y = "Frecuencia",
           subtitle = "Línea roja: Promedio | Líneas azules: Intervalo de confianza 95%") +
      theme_minimal()
  })
  
  # Cálculo de estadísticas de la primera muestra
  output$stats <- renderPrint({
    first_sample_mean <- data()$first_sample_mean
    first_sample_ci <- data()$first_sample_ci
    
    cat("Promedio de la primera muestra:", round(first_sample_mean, 2), "\n")
    cat("Intervalo de confianza 95% de la primera muestra:", 
        paste0("[", round(first_sample_ci[1], 2), ", ", round(first_sample_ci[2], 2), "]"), "\n")
  })
}

# Ejecutar la app
shinyApp(ui, server)
