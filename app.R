library(shiny)

ui <- fluidPage(

  titlePanel("EV / ICE Trip Calculator"),

  sidebarLayout(
    sidebarPanel(

      fileInput("file", "Wgraj plik CSV"),

      numericInput(
        "b",
        "Maksymalne spalanie ICE (l/100 km)",
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

      h4("Podgląd danych"),
      tableOutput("table")

    )
  )
)

server <- function(input, output) {

  data_processed <- eventReactive(input$run, {

    req(input$file)

    trip <- read.csv(input$file$datapath)

    # kontrola czy kolumny istnieją
    validate(
      need("Average.fuel.consumption.in.l.100km" %in% names(trip),
           "Brak kolumny: Average.fuel.consumption.in.l.100km"),
      need("Mileage.in.km" %in% names(trip),
           "Brak kolumny: Mileage.in.km")
    )

    # obliczenia
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
      "Benzyna (ICE): ", round(total_ice, 1), " km (",
      round(total_ice / total_km * 100, 1), "%)\n",
      "Elektrycznie (EV): ", round(total_ev, 1), " km (",
      round(total_ev / total_km * 100, 1), "%)"
    )
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