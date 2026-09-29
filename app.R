library(shiny)
library(ggplot2)

ui <- fluidPage(

  titlePanel(
    "Analiza jazdy EV / benzyna - Skoda Kodiaq iV PHEV"
  ),

  sidebarLayout(

    sidebarPanel(

      fileInput(
        "file",
        "Wgraj plik CSV z aplikacji MySkoda"
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

      h4("Model dla przejazdów mieszanych"),
      verbatimTextOutput("model_summary"),

      plotOutput("hybrid_plot"),

      hr(),

      h4("Podsumowanie przebiegu"),
      verbatimTextOutput("summary"),

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

  # ========================================================
  # WCZYTANIE I PRZYGOTOWANIE DANYCH
  # ========================================================

  data_raw <- eventReactive(input$run, {

    req(input$file)

    trip <- read.csv(
      input$file$datapath,
      check.names = TRUE
    )

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

    # konwersja do wartości liczbowych
    trip$Mileage.in.km <-
      as.numeric(trip$Mileage.in.km)

    trip$Average.fuel.consumption.in.l.100km <-
      as.numeric(trip$Average.fuel.consumption.in.l.100km)

    trip$Average.electric.consumption.in.kWh.100km <-
      as.numeric(trip$Average.electric.consumption.in.kWh.100km)


    # ------------------------------------------------------
    # Klasyfikacja przejazdów
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
  # MODEL REGRESJI DLA PRZEJAZDÓW MIESZANYCH
  # ========================================================

  model_info <- reactive({

    trip <- data_raw()

    hybrid <- trip[
      trip$Typ_przejazdu == "Mieszany" &
        !is.na(trip$Average.fuel.consumption.in.l.100km) &
        !is.na(trip$Average.electric.consumption.in.kWh.100km),
    ]

    validate(
      need(
        nrow(hybrid) >= 3,
        "Za mało przejazdów mieszanych do utworzenia modelu."
      )
    )

    validate(
      need(
        length(unique(
          hybrid$Average.electric.consumption.in.kWh.100km
        )) > 1,
        "Brak zróżnicowania zużycia energii potrzebnego do regresji."
      )
    )

    model <- lm(
      Average.fuel.consumption.in.l.100km ~
        Average.electric.consumption.in.kWh.100km,
      data = hybrid
    )

    intercept <- coef(model)[1]
    slope <- coef(model)[2]

    r2 <- summary(model)$r.squared

    list(
      model = model,
      hybrid = hybrid,
      intercept = intercept,
      slope = slope,
      r2 = r2
    )
  })


  # ========================================================
  # OBLICZENIE PRZEBIEGU EV / BENZYNA
  # ========================================================

  data_processed <- reactive({

    trip <- data_raw()

    info <- model_info()

    # spalanie przy 0 kWh/100 km z modelu
    fuel_ref <- info$intercept

    validate(
      need(
        fuel_ref > 0,
        "Model zwrócił nieprawidłowe spalanie referencyjne."
      )
    )

    # udział jazdy spalinowej
    trip$Udzial_spalinowy <-
      trip$Average.fuel.consumption.in.l.100km / fuel_ref

    trip$Udzial_spalinowy <-
      pmax(
        0,
        pmin(
          1,
          trip$Udzial_spalinowy
        )
      )

    # szacowane km
    trip$Km_spalinowe <-
      trip$Mileage.in.km * trip$Udzial_spalinowy

    trip$Km_elektryczne <-
      trip$Mileage.in.km - trip$Km_spalinowe

    trip
  })


  # ========================================================
  # PODSUMOWANIE MODELU
  # ========================================================

  output$model_summary <- renderText({

    info <- model_info()

    paste0(

      "Liczba przejazdów mieszanych użytych do modelu: ",
      nrow(info$hybrid),
      "\n\n",

      "Równanie modelu:\n",

      "Spalanie = ",
      round(info$slope, 3),
      " × zużycie energii + ",
      round(info$intercept, 3),
      "\n\n",

      "R² = ",
      round(info$r2, 3),
      "\n\n",

      "Modelowane spalanie przy 0 kWh/100 km: ",
      round(info$intercept, 2),
      " l/100 km"
    )
  })


  # ========================================================
  # WYKRES REGRESJI DLA PRZEJAZDÓW MIESZANYCH
  # ========================================================

  output$hybrid_plot <- renderPlot({

    info <- model_info()

    hybrid <- info$hybrid

    label_model <- paste0(
      "Spalanie = ",
      round(info$slope, 3),
      " × kWh + ",
      round(info$intercept, 3),
      "\nR² = ",
      round(info$r2, 3)
    )

    ggplot(
      hybrid,
      aes(
        x = Average.electric.consumption.in.kWh.100km,
        y = Average.fuel.consumption.in.l.100km
      )
    ) +

      geom_point(
        size = 3,
        alpha = 0.7
      ) +

      geom_smooth(
        method = "lm",
        se = TRUE
      ) +

      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = label_model,
        hjust = 1.1,
        vjust = 1.5,
        size = 5
      ) +

      labs(
        title = "Przejazdy mieszane: zużycie energii a zużycie benzyny",
        subtitle = paste0(
          "Modelowane spalanie przy 0 kWh/100 km = ",
          round(info$intercept, 2),
          " l/100 km"
        ),
        x = "Zużycie energii [kWh/100 km]",
        y = "Zużycie benzyny [l/100 km]"
      ) +

      theme_minimal() +

      theme(
        plot.title = element_text(
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          hjust = 0.5
        )
      )
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
      trip$Km_spalinowe,
      na.rm = TRUE
    )

    total_elektryczne <- sum(
      trip$Km_elektryczne,
      na.rm = TRUE
    )

    paste0(

      "Całkowity przebieg: ",
      round(total_km, 1),
      " km\n\n",

      "Szacowany przebieg spalinowy: ",
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
  # WYKRES KOŁOWY
  # ========================================================

  output$donut <- renderPlot({

    trip <- data_processed()

    total_spalinowe <- sum(
      trip$Km_spalinowe,
      na.rm = TRUE
    )

    total_elektryczne <- sum(
      trip$Km_elektryczne,
      na.rm = TRUE
    )

    df <- data.frame(

      type = c(
        "Spalinowy",
        "Elektryczny"
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
          "Spalinowy" = "orange",
          "Elektryczny" = "darkgreen"
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

    n_ev <- sum(
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
      n_ev +
      n_spalinowy +
      n_mieszany

    paste0(

      "Wszystkie przejazdy: ",
      n_total,
      "\n\n",

      "Tylko elektryczne: ",
      n_ev,
      "\n",

      "Tylko spalinowe: ",
      n_spalinowy,
      "\n",

      "Mieszane: ",
      n_mieszany
    )
  })


  # ========================================================
  # WYKRES LICZBY PRZEJAZDÓW
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
  # PODGLĄD TABELI
  # ========================================================

  output$table <- renderTable({

    head(
      data_processed(),
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
