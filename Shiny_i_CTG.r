library("shiny")
library("shinyWidgets")

ui <- fluidPage(
  
  tags$style(HTML("body {font-family: Times New Roman;}")),
  
  setBackgroundColor("bisque"),
  
  titlePanel(title = "Centralne Twierdzenie Graniczne (CTG)"),

  sidebarLayout(position = "left",
                sidebarPanel(chooseSliderSkin("Flat", color = "darkgreen"),
                             sliderInput("los",
                                         "Liczba losowań",
                                         min = 1,
                                         max = 50000,
                                         value = 10000),
                             sliderInput("n",
                                         "Liczebność próby",
                                         min = 5,
                                         max = 100,
                                         value = 20),
                             sliderInput("mean1",
                                         "Średnia pierwszej populacji",
                                         min = 0,
                                         max = 200,
                                         value = 165),
                             sliderInput("mean2",
                                         "Średnia drugiej populacji",
                                         min = 0,
                                         max = 200,
                                         value = 175),
                             sliderInput("sd1",
                                         "Odchylenie standardowe pierwszej populacji",
                                         min = 0,
                                         max = 30,
                                         value = 20),
                             sliderInput("sd2",
                                         "Odchylenie standardowe drugiej populacji",
                                         min = 0,
                                         max = 30,
                                         value = 10)),
                mainPanel(style = "font-size:20px",
                          plotOutput("wykresy"),
                          textOutput("text1"),
                          textOutput("text2")
                          )
                )
  )

server <- function(input, output) {
  
  output$text1 <- renderText({
    paste("n₁ = ", input$n, ", μ₁ = ", input$mean1, ", σ₁ = ", input$sd1)
  })
  
  output$text2 <- renderText({
    paste("n₂ = ", input$n, ", μ₂ = ", input$mean2, ", σ₂ = ", input$sd2)
  })
  
  output$wykresy <- renderPlot({
    
    symulacja <- function(los, n, mean, sd) {
      posymulacji <- c()
      replicate(los,
                {
                  posymulacji <- c(posymulacji, mean(rnorm(n, mean, sd)))
                })
    }
    
    s1 <- symulacja(input$los, input$n, input$mean1, input$sd1)
    s2 <- symulacja(input$los, input$n, input$mean2, input$sd2)
    
    
    t_purple = adjustcolor("darkorchid4", alpha.f = 0.5)
    t_green = adjustcolor("limegreen", alpha.f = 0.5)
    
    par(bg = "bisque")
    
    hist(s1,
         freq = FALSE,
         xlim = c(min(s1, s2) - max(input$sd1, input$sd2), max(s1, s2) + max(input$sd1, input$sd2)),
         ylim = c(0, max(prop.table(table(round(s1))), prop.table(table(round(s2))))),
         main = " ",
         xlab = "Średnia",
         ylab = "Gęstość",
         col = t_purple,
    )

    curve(dnorm(x, input$mean1, input$sd1),
          add = TRUE,
          col = "darkorchid4",
          lwd = 2
    )
    
    hist(s2,
         add = TRUE,
         freq = FALSE,
         col = t_green
    )

    curve(dnorm(x, input$mean2, input$sd2),
          add = TRUE,
          col = "limegreen",
          lwd = 2
    )
    
  })
}

shinyApp(ui, server)

