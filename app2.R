library(data.table)
library(ggplot2)
library(shiny)

set.seed(707)

options(scipen = 15, digits.secs = 6)
dat <- fread('book_ts.csv', header = TRUE)

convert_time_fast <- function(tim) {
  b <- tim - tim %/% 10 ^ 12 * 10 ^ 12
  # hhmmssffffff
  ms <- b %% 10 ^ 6
  b <- (b - ms) / 10 ^ 6
  ss <- b %% 10 ^ 2
  b <- (b - ss) / 10 ^ 2
  mm <- b %% 10 ^ 2
  hh <- (b - mm) / 10 ^ 2
  # if hours>=22, subtract 24 (previous day)
  hh <- hh - (hh >= 22) * 24
  return(hh + mm / 60 + ss / 3600 + ms / (3600 * 10 ^ 6))
}

dat[, Tc := convert_time_fast(Time)]

# filters for market hours
dat2 <- copy(dat[Tc %between% c(15, 20.25)])
Tc <- dat2$Tc
LastPrice <- dat2$LastPrice



ui <- fluidPage(
  titlePanel("Trading Data for E-mini S&P 500"),
  h1("June 30, 2011"),
  strong("Visual Shiny App to assist locating spoofing"),
  br(),
  
  sidebarLayout(sidebarPanel(
    sliderInput(
      'Tc',
      "Time Interval",
      min = 15,
      max = 20.25,
      2,
      value = c(15, 20.25)
    )
  ),
  mainPanel(plotOutput("plot1")))
)


server <- function(input, output, session) {
  dat5 <- reactive({
    dat5 <- copy(dat[Tc %between% c(input$Tc[1], input$Tc[2])])
  })
  
  output$plot1 <- renderPlot({
    ggplot(dat5(),
           aes(x = Tc,
               y = LastPrice)) +
      geom_line(size = 2,
                alpha = 0.5) +
      geom_point(size = 3) +
      xlab("Time") +
      ylab("Price") +
      theme(text = element_text(size = 18),
            legend.position = 'bottom')
  })
}


shinyApp(ui = ui, server = server)