library(shiny)
library(ggplot2)

ui <- fluidPage(

  titlePanel("Proporcja użycia silnika spalinowego i elektrycznego"),

  sidebarLayout(
    sidebarPanel(

      fileInput("file", "Wgraj plik CSV"),

      numericInput(
        "b",
        "Maksymalne spalanie benzyny (l/100 km)",
        value = 6.5,
        min = 0.1
      ),

      actionButton("run", "Oblicz"),

      hr(),
      downloadButton("download", "Pobierz wynik CSV")
    ),

    mainPanel(

      h4("Podsumowanie"),
      verbatimTextOutput("summary"),

      hr(),

      h4("Wykres kołowy"),
      plotOutput("Wykres"),

      hr(),

      h4("Podgląd danych"),
      tableOutput("table")

    )
  )
)

server <- function(input, output) {

  data_processed <- eventReactive(input$run, {

    req(input$file)

    trip <- read.csv(input$file$datapath)

    validate(
      need("Average.fuel.consumption.in.l.100km" %in% names(trip),
           "Brak kolumny: Average.fuel.consumption.in.l.100km"),
      need("Mileage.in.km" %in% names(trip),
           "Brak kolumny: Mileage.in.km")
    )

    trip$ICE_share <- trip$Average.fuel.consumption.in.l.100km / input$b
    trip$ICE_share <- pmax(0, pmin(1, trip$ICE_share))

    trip$ICE_km <- trip$Mileage.in.km * trip$ICE_share
    trip$EV_km  <- trip$Mileage.in.km - trip$ICE_km

    trip
  })

  output$summary <- renderText({

    trip <- data_processed()

    total_km <- sum(trip$Mileage.in.km, na.rm = TRUE)
    total_ice <- sum(trip$ICE_km, na.rm = TRUE)
    total_ev <- sum(trip$EV_km, na.rm = TRUE)

    paste0(
      "Całkowity przebieg: ", round(total_km, 1), " km\n",
      "ICE: ", round(total_ice, 1), " km (",
      round(total_ice / total_km * 100, 1), "%)\n",
      "EV: ", round(total_ev, 1), " km (",
      round(total_ev / total_km * 100, 1), "%)"
    )
  })

  output$donut <- renderPlot({

    trip <- data_processed()

    total_km <- sum(trip$Mileage.in.km, na.rm = TRUE)
    total_ice <- sum(trip$ICE_km, na.rm = TRUE)
    total_ev <- sum(trip$EV_km, na.rm = TRUE)

    df <- data.frame(
      type = c("ICE", "EV"),
      value = c(total_ice, total_ev)
    )

    df$percent <- df$value / sum(df$value) * 100

    ggplot(df, aes(x = 2, y = value, fill = type)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      theme_void() +
      geom_text(aes(label = paste0(round(percent, 1), "%")),
                position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values = c("ICE" = "orange", "EV" = "darkgreen")) +
      ggtitle("Udział przebiegu benzynowego vs elektrycznego")
  })

  output$table <- renderTable({
    head(data_processed(), 20)
  })

  output$download <- downloadHandler(

    filename = function() {
      "wynik_EV_ICE.csv"
    },

    content = function(file) {
      write.csv(data_processed(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
