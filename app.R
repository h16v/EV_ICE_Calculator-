library(shiny)
library(ggplot2)

ui <- fluidPage(

  titlePanel(
    "Udział jazdy spalinowej i elektrycznej - Skoda Kodiaq iV PHEV"
  ),

  sidebarLayout(

    sidebarPanel(

      fileInput(
        "file",
        "Wgraj plik CSV z aplikacji MySkoda"
      ),

      numericInput(
        "b",
        "Referencyjne spalanie benzyny przy 0 kWh/100 km (l/100 km)",
        value = 7.3,
        min = 0.1,
        step = 0.1
      ),

      actionButton(
        "run",
        "Oblicz"
      ),

      hr(),

      downloadButton(
        "download",
        "Pobierz wynik CSV"
      )
    ),

    mainPanel(

      h4("Podsumowanie przebiegu"),
      verbatimTextOutput("summary"),

      hr(),

      h4("Udział przebiegu"),
      plotOutput("donut"),

      hr(),

      h4("Liczba przejazdów według rodzaju napędu"),
      verbatimTextOutput("trip_counts"),

      plotOutput("trip_bar"),

      hr(),

      h4("Podgląd danych"),
      tableOutput("table")
    )
  )
)


server <- function(input, output, session) {

  data_processed <- eventReactive(input$run, {

    req(input$file)

    trip <- read.csv(
      input$file$datapath,
      check.names = TRUE
    )


    # ------------------------------------------------------
    # Kontrola wymaganych kolumn
    # ------------------------------------------------------

    validate(

      need(
        "Average.fuel.consumption.in.l.100km" %in% names(trip),
        "Brak kolumny dotyczącej zużycia benzyny."
      ),

      need(
        "Average.electric.consumption.in.kWh.100km" %in% names(trip),
        "Brak kolumny dotyczącej zużycia energii elektrycznej."
      ),

      need(
        "Mileage.in.km" %in% names(trip),
        "Brak kolumny dotyczącej długości przejazdu."
      )
    )


    # ------------------------------------------------------
    # Konwersja danych na wartości liczbowe
    # ------------------------------------------------------

    trip$Mileage.in.km <-
      as.numeric(trip$Mileage.in.km)

    trip$Average.fuel.consumption.in.l.100km <-
      as.numeric(trip$Average.fuel.consumption.in.l.100km)

    trip$Average.electric.consumption.in.kWh.100km <-
      as.numeric(trip$Average.electric.consumption.in.kWh.100km)


    # ------------------------------------------------------
    # Szacowanie udziału jazdy spalinowej
    # ------------------------------------------------------

    trip$ICE_share <-
      trip$Average.fuel.consumption.in.l.100km / input$b

    trip$ICE_share <-
      pmax(
        0,
        pmin(
          1,
          trip$ICE_share
        )
      )


    # ------------------------------------------------------
    # Szacowane kilometry
    # ------------------------------------------------------

    trip$ICE_km <-
      trip$Mileage.in.km * trip$ICE_share

    trip$EV_km <-
      trip$Mileage.in.km - trip$ICE_km


    # ------------------------------------------------------
    # Klasyfikacja całych przejazdów
    #
    # Elektryczny:
    # benzyna = 0
    #
    # Spalinowy:
    # benzyna > 0 i prąd = 0
    #
    # Mieszany:
    # benzyna > 0 i prąd > 0
    # ------------------------------------------------------

    trip$Typ_przejazdu <- ifelse(

      trip$Average.fuel.consumption.in.l.100km == 0,

      "Elektryczny",

      ifelse(

        trip$Average.electric.consumption.in.kWh.100km == 0,

        "Spalinowy",

        "Mieszany"
      )
    )


    trip
  })


  # ========================================================
  # PODSUMOWANIE PRZEBIEGU
  # ========================================================

  output$summary <- renderText({

    trip <- data_processed()

    total_km <- sum(
      trip$Mileage.in.km,
      na.rm = TRUE
    )

    total_spalinowe <- sum(
      trip$ICE_km,
      na.rm = TRUE
    )

    total_elektryczne <- sum(
      trip$EV_km,
      na.rm = TRUE
    )


    paste0(

      "Całkowity przebieg: ",
      round(total_km, 1),
      " km\n\n",

      "Szacowany przebieg na silniku spalinowym: ",
      round(total_spalinowe, 1),
      " km (",
      round(
        total_spalinowe / total_km * 100,
        1
      ),
      "%)\n",

      "Szacowany przebieg elektryczny: ",
      round(total_elektryczne, 1),
      " km (",
      round(
        total_elektryczne / total_km * 100,
        1
      ),
      "%)"
    )
  })


  # ========================================================
  # WYKRES KOŁOWY - PRZEBIEG
  # ========================================================

  output$donut <- renderPlot({

    trip <- data_processed()

    total_spalinowe <- sum(
      trip$ICE_km,
      na.rm = TRUE
    )

    total_elektryczne <- sum(
      trip$EV_km,
      na.rm = TRUE
    )


    df <- data.frame(

      type = c(
        "Silnik spalinowy",
        "Napęd elektryczny"
      ),

      value = c(
        total_spalinowe,
        total_elektryczne
      )
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

      geom_col(
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

      geom_text(

        aes(
          label = paste0(
            round(percent, 1),
            "%"
          )
        ),

        position = position_stack(
          vjust = 0.5
        ),

        size = 5
      ) +

      scale_fill_manual(

        values = c(
          "Silnik spalinowy" = "orange",
          "Napęd elektryczny" = "darkgreen"
        )
      ) +

      labs(
        title = "Szacowany udział przebiegu",
        fill = NULL
      ) +

      theme_void() +

      theme(

        plot.title = element_text(
          hjust = 0.5
        ),

        legend.position = "bottom"
      )
  })


  # ========================================================
  # LICZBA PRZEJAZDÓW
  # ========================================================

  output$trip_counts <- renderText({

    trip <- data_processed()


    n_elektryczny <- sum(
      trip$Typ_przejazdu == "Elektryczny",
      na.rm = TRUE
    )

    n_spalinowy <- sum(
      trip$Typ_przejazdu == "Spalinowy",
      na.rm = TRUE
    )

    n_mieszany <- sum(
      trip$Typ_przejazdu == "Mieszany",
      na.rm = TRUE
    )


    n_total <-
      n_elektryczny +
      n_spalinowy +
      n_mieszany


    paste0(

      "Wszystkie przejazdy: ",
      n_total,
      "\n\n",

      "Tylko elektryczne: ",
      n_elektryczny,
      "\n",

      "Tylko spalinowe: ",
      n_spalinowy,
      "\n",

      "Mieszane: ",
      n_mieszany
    )
  })


  # ========================================================
  # WYKRES SŁUPKOWY - TYPY PRZEJAZDÓW
  # ========================================================

  output$trip_bar <- renderPlot({

    trip <- data_processed()


    df_trips <- data.frame(

      Typ = c(
        "Elektryczne",
        "Spalinowe",
        "Mieszane"
      ),

      Liczba = c(

        sum(
          trip$Typ_przejazdu == "Elektryczny",
          na.rm = TRUE
        ),

        sum(
          trip$Typ_przejazdu == "Spalinowy",
          na.rm = TRUE
        ),

        sum(
          trip$Typ_przejazdu == "Mieszany",
          na.rm = TRUE
        )
      )
    )


    ggplot(
      df_trips,
      aes(
        x = Typ,
        y = Liczba,
        fill = Typ
      )
    ) +

      geom_col(
        width = 0.65
      ) +

      geom_text(
        aes(
          label = Liczba
        ),
        vjust = -0.5,
        size = 5
      ) +

      scale_fill_manual(

        values = c(
          "Elektryczne" = "darkgreen",
          "Spalinowe" = "orange",
          "Mieszane" = "steelblue"
        )
      ) +

      labs(
        title = "Liczba przejazdów według rodzaju napędu",
        x = NULL,
        y = "Liczba przejazdów"
      ) +

      theme_minimal() +

      theme(

        legend.position = "none",

        plot.title = element_text(
          hjust = 0.5
        )
      ) +

      expand_limits(
        y = max(df_trips$Liczba) * 1.1
      )
  })


  # ========================================================
  # PODGLĄD DANYCH
  # ========================================================

  output$table <- renderTable({

    trip <- data_processed()

    head(
      trip,
      20
    )
  })


  # ========================================================
  # ZAPIS CSV
  # ========================================================

  output$download <- downloadHandler(

    filename = function() {

      "wynik_EV_spalinowy.csv"
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
