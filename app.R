library(shiny)
library(ggplot2)

ui <- fluidPage(

  titlePanel("EV / ICE Trip Calculator"),

  sidebarLayout(
    sidebarPanel(

      fileInput("file", "Wgraj plik CSV"),

      numericInput(
        "b",
        "Referencyjne spalanie ICE (l/100 km)",
        value = 7.3,
        min = 0.1
      ),

      numericInput(
        "ev_threshold",
        "Próg zużycia prądu dla przejazdu ICE (kWh/100 km)",
        value = 1,
        min = 0,
        step = 0.1
      ),

      actionButton("run", "Oblicz"),

      hr(),
      downloadButton("download", "Pobierz wynik CSV")
    ),

    mainPanel(

      h4("Podsumowanie"),
      verbatimTextOutput("summary"),

      hr(),

      h4("Liczba przejazdów"),
      verbatimTextOutput("trip_counts"),

      hr(),

      h4("Wykres (donut)"),
      plotOutput("donut"),

      hr(),

      h4("Podgląd danych"),
      tableOutput("table")

    )
  )
)

server <- function(input, output) {

  data_processed <- eventReactive(input$run, {

    req(input$file)

    trip <- read.csv(
      input$file$datapath,
      check.names = TRUE
    )

    validate(
      need(
        "Average.fuel.consumption.in.l.100km" %in% names(trip),
        "Brak kolumny: Average.fuel.consumption.in.l.100km"
      ),
      need(
        "Mileage.in.km" %in% names(trip),
        "Brak kolumny: Mileage.in.km"
      ),
      need(
        "Average.electric.consumption.in.kWh.100km" %in% names(trip),
        "Brak kolumny: Average.electric.consumption.in.kWh.100km"
      )
    )

    trip$Mileage.in.km <-
      as.numeric(trip$Mileage.in.km)

    trip$Average.fuel.consumption.in.l.100km <-
      as.numeric(trip$Average.fuel.consumption.in.l.100km)

    trip$Average.electric.consumption.in.kWh.100km <-
      as.numeric(trip$Average.electric.consumption.in.kWh.100km)


    # ------------------------------------------------------
    # Dotychczasowa metoda liczenia przebiegu ICE / EV
    # ------------------------------------------------------

    trip$ICE_share <-
      trip$Average.fuel.consumption.in.l.100km / input$b

    trip$ICE_share <-
      pmax(0, pmin(1, trip$ICE_share))

    trip$ICE_km <-
      trip$Mileage.in.km * trip$ICE_share

    trip$EV_km <-
      trip$Mileage.in.km - trip$ICE_km


    # ------------------------------------------------------
    # Klasyfikacja rodzaju przejazdu
    # ------------------------------------------------------

    trip$drive_type <- ifelse(
      trip$Average.fuel.consumption.in.l.100km == 0,
      "EV",
      ifelse(
        trip$Average.electric.consumption.in.kWh.100km <= input$ev_threshold,
        "ICE",
        "Mixed"
      )
    )

    trip
  })


  # --------------------------------------------------------
  # Podsumowanie kilometrów
  # --------------------------------------------------------

  output$summary <- renderText({

    trip <- data_processed()

    total_km <- sum(
      trip$Mileage.in.km,
      na.rm = TRUE
    )

    total_ice <- sum(
      trip$ICE_km,
      na.rm = TRUE
    )

    total_ev <- sum(
      trip$EV_km,
      na.rm = TRUE
    )

    paste0(
      "Całkowity przebieg: ",
      round(total_km, 1),
      " km\n",

      "ICE: ",
      round(total_ice, 1),
      " km (",
      round(total_ice / total_km * 100, 1),
      "%)\n",

      "EV: ",
      round(total_ev, 1),
      " km (",
      round(total_ev / total_km * 100, 1),
      "%)"
    )
  })


  # --------------------------------------------------------
  # Liczba przejazdów EV / ICE / Mixed
  # --------------------------------------------------------

  output$trip_counts <- renderText({

    trip <- data_processed()

    n_ev <- sum(
      trip$drive_type == "EV",
      na.rm = TRUE
    )

    n_ice <- sum(
      trip$drive_type == "ICE",
      na.rm = TRUE
    )

    n_mixed <- sum(
      trip$drive_type == "Mixed",
      na.rm = TRUE
    )

    n_total <- sum(
      !is.na(trip$drive_type)
    )

    paste0(
      "Wszystkie przejazdy: ", n_total, "\n",
      "Tylko EV: ", n_ev, "\n",
      "Tylko ICE: ", n_ice, "\n",
      "Mieszane: ", n_mixed
    )
  })


  # --------------------------------------------------------
  # Wykres
  # --------------------------------------------------------

  output$donut <- renderPlot({

    trip <- data_processed()

    total_ice <- sum(
      trip$ICE_km,
      na.rm = TRUE
    )

    total_ev <- sum(
      trip$EV_km,
      na.rm = TRUE
    )

    df <- data.frame(
      type = c("ICE", "EV"),
      value = c(total_ice, total_ev)
    )

    df$percent <-
      df$value / sum(df$value) * 100

    ggplot(
      df,
      aes(
        x = 2,
        y = value,
        fill = type
      )
    ) +

      geom_bar(
        stat = "identity",
        width = 1,
        color = "white"
      ) +

      coord_polar(
        theta = "y"
      ) +

      xlim(
        0.5,
        2.5
      ) +

      theme_void() +

      geom_text(
        aes(
          label = paste0(
            round(percent, 1),
            "%"
          )
        ),
        position = position_stack(
          vjust = 0.5
        )
      ) +

      scale_fill_manual(
        values = c(
          "ICE" = "orange",
          "EV" = "green"
        )
      ) +

      ggtitle(
        "Udział przebiegu ICE vs EV"
      )
  })


  # --------------------------------------------------------
  # Podgląd danych
  # --------------------------------------------------------

  output$table <- renderTable({

    trip <- data_processed()

    head(
      trip,
      20
    )
  })


  # --------------------------------------------------------
  # Pobieranie CSV
  # --------------------------------------------------------

  output$download <- downloadHandler(

    filename = function() {
      "wynik_EV_ICE.csv"
    },

    content = function(file) {

      write.csv(
        data_processed(),
        file,
        row.names = FALSE
      )
    }
  )
}

shinyApp(
  ui = ui,
  server = server
)
