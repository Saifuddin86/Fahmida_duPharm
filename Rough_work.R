library(shiny)

ui <- fluidPage(
  titlePanel("Central Limit Theorem Simulation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose Distribution:",
                  choices = c("Uniform" = "unif", "Exponential" = "exp", "Poisson" = "pois")),
      sliderInput("n", "Sample Size (n):", min = 5, max = 100, value = 30),
      sliderInput("samples", "Number of Samples:", min = 100, max = 5000, value = 1000),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$simulate, {
    output$histPlot <- renderPlot({
      n <- input$n
      samples <- input$samples
      dist <- input$dist
      
      sample_means <- numeric(samples)
      
      for (i in 1:samples) {
        if (dist == "unif") {
          sample_means[i] <- mean(runif(n))
        } else if (dist == "exp") {
          sample_means[i] <- mean(rexp(n, rate = 1))
        } else if (dist == "pois") {
          sample_means[i] <- mean(rpois(n, lambda = 4))
        }
      }
      
      hist(sample_means, breaks = 30, probability = TRUE, main = "Sampling Distribution of the Mean",
           xlab = "Sample Means", col = "skyblue", border = "white")
      curve(dnorm(x, mean(sample_means), sd(sample_means)), col = "red", lwd = 2, add = TRUE)
    })
  })
}

shinyApp(ui = ui, server = server)


#knitr::purl("Analysis.qmd", output = "Analysis_script.R")


