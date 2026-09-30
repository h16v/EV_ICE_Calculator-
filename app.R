library(shiny)
library(ggplot2)


# ==========================================================
# KOLORY / COLORS
# ==========================================================

COL_EV <- "darkgreen"
COL_MIESZANY <- "orange"
COL_SPALINOWY <- "red"

COL_EV_MEAN <- "forestgreen"
COL_FUEL_MEAN <- "darkorange4"

COL_DAILY <- "steelblue"


# ==========================================================
# UI
# ==========================================================

ui <- fluidPage(

  titlePanel(
    "Analiza wykorzystania napędu elektrycznego i spalinowego / Electric vs petrol driving share – Škoda Kodiaq iV PHEV"
  ),

  sidebarLayout(

    # ======================================================
    # PANEL LEWY / LEFT PANEL
    # ======================================================

    sidebarPanel(

      fileInput(
        "file",
        "Wgraj plik CSV z aplikacji MySkoda / Upload a CSV file from the MySkoda app"
      ),

      actionButton(
        "run",
        "Oblicz / Calculate"
      ),

      hr(),

      downloadButton(
        "download",
        "Pobierz wynik CSV / Download results CSV"
      ),

      hr(),

      h4(
        "Jak interpretować wyniki? / How to interpret the results?"
      ),


      tags$p(

        tags$b(
          "Model dla przejazdów mieszanych / Mixed-mode trip model"
        ),

        tags$br(),

        "Regresja liniowa opisująca zależność pomiędzy zużyciem energii elektrycznej i benzyny podczas przejazdów mieszanych. ",
        "Wartość R² określa stopień dopasowania modelu do obserwowanych danych. ",
        "Wyraz wolny równania określa modelowane spalanie przy zużyciu energii równym 0 kWh/100 km i jest wykorzystywany jako wartość referencyjna do oszacowania udziału przebiegu spalinowego.",

        tags$br(),
        tags$br(),

        tags$em(
          "The linear regression describes the relationship between electricity and fuel consumption during mixed-mode trips. ",
          "R² indicates how well the model fits the observed data. ",
          "The intercept represents the modelled fuel consumption at 0 kWh/100 km and is used as the reference value for estimating the combustion-engine share of the distance."
        )
      ),


      tags$p(

        tags$b(
          "Podsumowanie przebiegu / Distance summary"
        ),

        tags$br(),

        "Szacowany podział całkowitego przebiegu na jazdę elektryczną i spalinową.",

        tags$br(),

        tags$em(
          "Estimated division of the total distance into electric and combustion-engine driving."
        )
      ),


      tags$p(

        tags$b(
          "Liczba przejazdów / Number of trips"
        ),

        tags$br(),

        "Liczba przejazdów elektrycznych, mieszanych i spalinowych.",

        tags$br(),

        tags$em(
          "Number of electric, mixed-mode and combustion-only trips."
        )
      ),


      tags$p(

        tags$b(
          "Przejazdy w czasie / Trips over time"
        ),

        tags$br(),

        "Pokazuje, kiedy występowały przejazdy elektryczne, mieszane i spalinowe. ",
        "Linie przerywane przedstawiają średnie zużycie energii podczas jazdy elektrycznej oraz średnie spalanie w cyklu mieszanym.",

        tags$br(),

        tags$em(
          "Shows when electric, mixed-mode and combustion-only trips occurred. ",
          "Dashed lines indicate the average electricity consumption during electric trips and the average fuel consumption during mixed-mode trips."
        )
      ),


      tags$p(

        tags$b(
          "Dzienny przebieg / Daily distance"
        ),

        tags$br(),

        "Łączny przebieg wykonany każdego dnia na podstawie wszystkich przejazdów zapisanych w pliku CSV.",

        tags$br(),

        tags$em(
          "Total distance travelled each day based on all trips recorded in the CSV file."
        )
      )
    ),


    # ======================================================
    # PANEL GŁÓWNY / MAIN PANEL
    # ======================================================

    mainPanel(

      h4(
        "Model dla przejazdów mieszanych / Mixed-mode trip model"
      ),

      verbatimTextOutput(
        "model_summary"
      ),

      plotOutput(
        "hybrid_plot"
      ),

      hr(),


      h4(
        "Podsumowanie przebiegu / Distance summary"
      ),

      verbatimTextOutput(
        "summary"
      ),

      plotOutput(
        "donut"
      ),

      hr(),


      h4(
        "Liczba przejazdów według rodzaju napędu / Number of trips by powertrain mode"
      ),

      verbatimTextOutput(
        "trip_counts"
      ),

      plotOutput(
        "trip_bar"
      ),

      hr(),


      h4(
        "Przejazdy w czasie / Trips over time"
      ),

      plotOutput(
        "timeline_plot",
        height = "500px"
      ),

      hr(),


      h4(
        "Dzienny przebieg / Daily distance"
      ),

      plotOutput(
        "daily_distance_plot",
        height = "450px"
      ),

      hr(),


      h4(
        "Podgląd danych / Data preview"
      ),

      tableOutput(
        "table"
      )
    )
  )
)


# ==========================================================
# SERVER
# ==========================================================

server <- function(input, output, session) {


  # ========================================================
  # WCZYTANIE I PRZYGOTOWANIE DANYCH
  # DATA IMPORT AND PREPARATION
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
        "Brak kolumny zużycia benzyny. / Fuel consumption column is missing."
      ),

      need(
        "Average.electric.consumption.in.kWh.100km" %in% names(trip),
        "Brak kolumny zużycia energii elektrycznej. / Electricity consumption column is missing."
      ),

      need(
        "Mileage.in.km" %in% names(trip),
        "Brak kolumny długości przejazdu. / Trip distance column is missing."
      ),

      need(
        "End.of.trip" %in% names(trip),
        "Brak kolumny End.of.trip. / End.of.trip column is missing."
      )
    )


    # ------------------------------------------------------
    # KONWERSJA WARTOŚCI / VALUE CONVERSION
    # ------------------------------------------------------

    trip$Mileage.in.km <-
      as.numeric(
        trip$Mileage.in.km
      )


    trip$Average.fuel.consumption.in.l.100km <-
      as.numeric(
        trip$Average.fuel.consumption.in.l.100km
      )


    trip$Average.electric.consumption.in.kWh.100km <-
      as.numeric(
        trip$Average.electric.consumption.in.kWh.100km
      )


    # ------------------------------------------------------
    # DATA / DATE
    # ------------------------------------------------------

    trip$Data_przejazdu <- as.POSIXct(
      trip$End.of.trip,
      format = "%d.%m.%Y %H:%M"
    )


    trip$Dzien <- as.Date(
      trip$Data_przejazdu
    )


    # ------------------------------------------------------
    # KLASYFIKACJA PRZEJAZDÓW / TRIP CLASSIFICATION
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
  # REGRESSION MODEL FOR MIXED-MODE TRIPS
  # ========================================================

  model_info <- reactive({

    trip <- data_raw()


    hybrid <- trip[

      trip$Typ_przejazdu == "Mieszany" &

        !is.na(
          trip$Average.fuel.consumption.in.l.100km
        ) &

        !is.na(
          trip$Average.electric.consumption.in.kWh.100km
        ),

    ]


    validate(

      need(
        nrow(hybrid) >= 3,
        "Za mało przejazdów mieszanych do utworzenia modelu. / Too few mixed-mode trips to fit the model."
      )
    )


    validate(

      need(

        length(
          unique(
            hybrid$Average.electric.consumption.in.kWh.100km
          )
        ) > 1,

        "Brak zróżnicowania zużycia energii potrzebnego do regresji. / Insufficient variation in electricity consumption for regression."
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
  # OBLICZENIE PRZEBIEGU ELEKTRYCZNEGO I SPALINOWEGO
  # ESTIMATION OF ELECTRIC AND COMBUSTION DISTANCE
  # ========================================================

  data_processed <- reactive({

    trip <- data_raw()

    info <- model_info()


    fuel_ref <- info$intercept


    validate(

      need(
        fuel_ref > 0,
        "Model zwrócił nieprawidłowe spalanie referencyjne. / The model returned an invalid reference fuel consumption."
      )
    )


    trip$Udzial_spalinowy <-

      trip$Average.fuel.consumption.in.l.100km /

      fuel_ref


    trip$Udzial_spalinowy <-

      pmax(

        0,

        pmin(
          1,
          trip$Udzial_spalinowy
        )
      )


    trip$Km_spalinowe <-

      trip$Mileage.in.km *

      trip$Udzial_spalinowy


    trip$Km_elektryczne <-

      trip$Mileage.in.km -

      trip$Km_spalinowe


    trip
  })


  # ========================================================
  # PODSUMOWANIE MODELU / MODEL SUMMARY
  # ========================================================

  output$model_summary <- renderText({

    info <- model_info()


    paste0(

      "Liczba przejazdów mieszanych użytych do modelu / ",
      "Number of mixed-mode trips used in the model: ",
      nrow(info$hybrid),

      "\n\n",

      "Równanie modelu / Model equation:\n",

      "Spalanie / Fuel consumption = ",
      round(info$slope, 3),
      " × zużycie energii / electricity consumption + ",
      round(info$intercept, 3),

      "\n\n",

      "R² = ",
      round(info$r2, 3),

      "\n\n",

      "Modelowane spalanie przy 0 kWh/100 km / ",
      "Modelled fuel consumption at 0 kWh/100 km: ",
      round(info$intercept, 2),
      " l/100 km"
    )
  })


  # ========================================================
  # WYKRES REGRESJI / REGRESSION PLOT
  # ========================================================

  output$hybrid_plot <- renderPlot({

    info <- model_info()

    hybrid <- info$hybrid


label_model <- paste0(
  "Spalanie / Fuel = ",
  round(info$slope, 3),
  " × kWh + ",
  round(info$intercept, 3),
  "\n",
  "R² = ",
  round(info$r2, 3)
)


    ggplot(

      hybrid,

      aes(

        x =
          Average.electric.consumption.in.kWh.100km,

        y =
          Average.fuel.consumption.in.l.100km
      )

    ) +

      geom_point(

        color =
          COL_MIESZANY,

        size = 3,

        alpha = 0.7
      ) +

      geom_smooth(

        method = "lm",

        se = TRUE,

        color =
          COL_MIESZANY
      ) +

annotate(
  "label",

  x = min(
    hybrid$Average.electric.consumption.in.kWh.100km,
    na.rm = TRUE
  ) + 0.5,

  y = max(
    hybrid$Average.fuel.consumption.in.l.100km,
    na.rm = TRUE
  ) * 0.98,

  label = label_model,

  hjust = 0,
  vjust = 1,

  size = 5,

  fill = "white",

  label.size = 0.35
)+

      labs(

        title =
          "Przejazdy mieszane: zużycie energii a zużycie benzyny / Mixed-mode trips: electricity vs fuel consumption",

        subtitle =
          paste0(

            "Modelowane spalanie przy 0 kWh/100 km / Modelled fuel consumption at 0 kWh/100 km = ",

            round(
              info$intercept,
              2
            ),

            " l/100 km"
          ),

        x =
          "Zużycie energii / Electricity consumption [kWh/100 km]",

        y =
          "Zużycie benzyny / Fuel consumption [l/100 km]"
      ) +

      theme_minimal() +

      theme(

        plot.title =
          element_text(
            hjust = 0.5
          ),

        plot.subtitle =
          element_text(
            hjust = 0.5
          )
      )
  })


  # ========================================================
  # PODSUMOWANIE PRZEBIEGU / DISTANCE SUMMARY
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

      "Całkowity przebieg / Total distance: ",
      round(total_km, 1),
      " km",

      "\n\n",

      "Szacowany przebieg spalinowy / Estimated combustion distance: ",
      round(total_spalinowe, 1),
      " km (",
      round(
        total_spalinowe /
          total_km *
          100,
        1
      ),
      "%)",

      "\n",

      "Szacowany przebieg elektryczny / Estimated electric distance: ",
      round(total_elektryczne, 1),
      " km (",
      round(
        total_elektryczne /
          total_km *
          100,
        1
      ),
      "%)"
    )
  })


  # ========================================================
  # WYKRES KOŁOWY / DONUT CHART
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

        "Przebieg spalinowy / Combustion distance",

        "Przebieg elektryczny / Electric distance"
      ),

      value = c(

        total_spalinowe,

        total_elektryczne
      )
    )


    df$percent <-

      df$value /

      sum(df$value) *

      100


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

          label =
            paste0(
              round(
                percent,
                1
              ),
              "%"
            )
        ),

        position =
          position_stack(
            vjust = 0.5
          ),

        size = 5
      ) +

      scale_fill_manual(

        values = c(

          "Przebieg spalinowy / Combustion distance" =
            COL_SPALINOWY,

          "Przebieg elektryczny / Electric distance" =
            COL_EV
        )
      ) +

      labs(

        title =
          "Szacowany udział przebiegu / Estimated distance share",

        fill = NULL
      ) +

      theme_void() +

      theme(

        plot.title =
          element_text(
            hjust = 0.5
          ),

        legend.position =
          "bottom"
      )
  })


  # ========================================================
  # LICZBA PRZEJAZDÓW / NUMBER OF TRIPS
  # ========================================================

  output$trip_counts <- renderText({

    trip <- data_processed()


    n_ev <- sum(
      trip$Typ_przejazdu == "Elektryczny",
      na.rm = TRUE
    )


    n_mieszany <- sum(
      trip$Typ_przejazdu == "Mieszany",
      na.rm = TRUE
    )


    n_spalinowy <- sum(
      trip$Typ_przejazdu == "Spalinowy",
      na.rm = TRUE
    )


    n_total <-

      n_ev +

      n_mieszany +

      n_spalinowy


    paste0(

      "Wszystkie przejazdy / All trips: ",
      n_total,

      "\n\n",

      "Tylko elektryczne / Electric only: ",
      n_ev,

      "\n",

      "Mieszane / Mixed-mode: ",
      n_mieszany,

      "\n",

      "Tylko spalinowe / Combustion only: ",
      n_spalinowy
    )
  })


  # ========================================================
  # WYKRES LICZBY PRZEJAZDÓW
  # NUMBER OF TRIPS CHART
  # ========================================================

  output$trip_bar <- renderPlot({

    trip <- data_processed()


    df_trips <- data.frame(

      Typ = factor(

        c(
          "Elektryczne\nElectric",
          "Mieszane\nMixed-mode",
          "Spalinowe\nCombustion"
        ),

        levels = c(
          "Elektryczne\nElectric",
          "Mieszane\nMixed-mode",
          "Spalinowe\nCombustion"
        )
      ),

      Liczba = c(

        sum(
          trip$Typ_przejazdu ==
            "Elektryczny",
          na.rm = TRUE
        ),

        sum(
          trip$Typ_przejazdu ==
            "Mieszany",
          na.rm = TRUE
        ),

        sum(
          trip$Typ_przejazdu ==
            "Spalinowy",
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

          "Elektryczne\nElectric" =
            COL_EV,

          "Mieszane\nMixed-mode" =
            COL_MIESZANY,

          "Spalinowe\nCombustion" =
            COL_SPALINOWY
        )
      ) +

      labs(

        title =
          "Liczba przejazdów według rodzaju napędu / Number of trips by powertrain mode",

        x = NULL,

        y =
          "Liczba przejazdów / Number of trips"
      ) +

      theme_minimal() +

      theme(

        legend.position =
          "none",

        plot.title =
          element_text(
            hjust = 0.5
          )
      ) +

      expand_limits(

        y =
          max(
            df_trips$Liczba
          ) *
          1.12
      )
  })


  # ========================================================
  # WYKRES PRZEJAZDÓW W CZASIE
  # TRIPS OVER TIME
  # ========================================================

  output$timeline_plot <- renderPlot({

    trip <- data_processed()


    trip <- trip[
      order(
        trip$Data_przejazdu
      ),
    ]


    plot_data <- trip[

      trip$Typ_przejazdu %in%

        c(
          "Elektryczny",
          "Mieszany",
          "Spalinowy"
        ) &

        !is.na(
          trip$Data_przejazdu
        ),

    ]


    validate(

      need(
        nrow(plot_data) > 0,
        "Brak przejazdów do wyświetlenia. / No trips available for plotting."
      )
    )


    # ------------------------------------------------------
    # MAKSYMALNE WARTOŚCI / MAXIMUM VALUES
    # ------------------------------------------------------

    max_kwh <- max(

      plot_data$Average.electric.consumption.in.kWh.100km[
        plot_data$Typ_przejazdu ==
          "Elektryczny"
      ],

      na.rm = TRUE
    )


    max_fuel <- max(

      plot_data$Average.fuel.consumption.in.l.100km[
        plot_data$Typ_przejazdu %in%
          c(
            "Mieszany",
            "Spalinowy"
          )
      ],

      na.rm = TRUE
    )


    validate(

      need(

        is.finite(max_kwh) &&
          max_kwh > 0,

        "Brak poprawnych danych zużycia energii. / No valid electricity consumption data."
      ),

      need(

        is.finite(max_fuel) &&
          max_fuel > 0,

        "Brak poprawnych danych zużycia benzyny. / No valid fuel consumption data."
      )
    )


    # ------------------------------------------------------
    # SKALOWANIE PRAWEJ OSI / RIGHT AXIS SCALING
    # ------------------------------------------------------

    scale_factor <-

      max_kwh /

      max_fuel


    # ------------------------------------------------------
    # ŚREDNIE / AVERAGES
    # ------------------------------------------------------

    mean_kwh <- mean(

      plot_data$Average.electric.consumption.in.kWh.100km[
        plot_data$Typ_przejazdu ==
          "Elektryczny"
      ],

      na.rm = TRUE
    )


    mean_fuel <- mean(

      plot_data$Average.fuel.consumption.in.l.100km[
        plot_data$Typ_przejazdu ==
          "Mieszany"
      ],

      na.rm = TRUE
    )


    mean_fuel_scaled <-

      mean_fuel *

      scale_factor


    # ------------------------------------------------------
    # POŁOŻENIE ETYKIET / LABEL POSITION
    # ------------------------------------------------------

    x_min <- min(
      plot_data$Data_przejazdu,
      na.rm = TRUE
    )


    x_max <- max(
      plot_data$Data_przejazdu,
      na.rm = TRUE
    )


    x_label <-

      x_min +

      (x_max - x_min) *

      0.02


    # ======================================================
    # WYKRES / PLOT
    # ======================================================

    ggplot(

      plot_data,

      aes(
        x = Data_przejazdu
      )

    ) +

      # ----------------------------------------------------
      # ELEKTRYCZNE / ELECTRIC
      # ----------------------------------------------------

      geom_col(

        data = subset(

          plot_data,

          Typ_przejazdu ==
            "Elektryczny"
        ),

        aes(

          y =
            Average.electric.consumption.in.kWh.100km
        ),

        fill =
          COL_EV,

        width =
          20 * 60 * 60,

        alpha =
          0.85
      ) +


      # ----------------------------------------------------
      # MIESZANE / MIXED-MODE
      # ----------------------------------------------------

      geom_col(

        data = subset(

          plot_data,

          Typ_przejazdu ==
            "Mieszany"
        ),

        aes(

          y =

            Average.fuel.consumption.in.l.100km *

            scale_factor
        ),

        fill =
          COL_MIESZANY,

        width =
          20 * 60 * 60,

        alpha =
          0.85
      ) +


      # ----------------------------------------------------
      # SPALINOWE / COMBUSTION
      # ----------------------------------------------------

      geom_col(

        data = subset(

          plot_data,

          Typ_przejazdu ==
            "Spalinowy"
        ),

        aes(

          y =

            Average.fuel.consumption.in.l.100km *

            scale_factor
        ),

        fill =
          COL_SPALINOWY,

        width =
          20 * 60 * 60,

        alpha =
          0.85
      ) +


      # ----------------------------------------------------
      # ŚREDNIE ZUŻYCIE ENERGII
      # AVERAGE ELECTRICITY CONSUMPTION
      # ----------------------------------------------------

      geom_hline(

        yintercept =
          mean_kwh,

        color =
          COL_EV_MEAN,

        linetype =
          "dashed",

        linewidth =
          1.15
      ) +


      annotate(

        "label",

        x =
          x_label,

        y =
          mean_kwh,

        label =
          paste0(

            "Średnie zużycie energii / Average electricity consumption: ",

            round(
              mean_kwh,
              1
            ),

            " kWh/100 km"
          ),

        color =
          COL_EV_MEAN,

        fill =
          "white",

        label.size =
          0.35,

        hjust =
          0,

        vjust =
          -0.6,

        size =
          4.0
      ) +


      # ----------------------------------------------------
      # ŚREDNIE SPALANIE W CYKLU MIESZANYM
      # AVERAGE FUEL CONSUMPTION IN MIXED MODE
      # ----------------------------------------------------

      geom_hline(

        yintercept =
          mean_fuel_scaled,

        color =
          COL_FUEL_MEAN,

        linetype =
          "dashed",

        linewidth =
          1.15
      ) +


      annotate(

        "label",

        x =
          x_label,

        y =
          mean_fuel_scaled,

        label =
          paste0(

            "Średnie spalanie w cyklu mieszanym / Average mixed-mode fuel consumption: ",

            round(
              mean_fuel,
              1
            ),

            " l/100 km"
          ),

        color =
          COL_FUEL_MEAN,

        fill =
          "white",

        label.size =
          0.35,

        hjust =
          0,

        vjust =
          1.4,

        size =
          4.0
      ) +


      # ----------------------------------------------------
      # DWIE OSIE Y / TWO Y-AXES
      # ----------------------------------------------------

      scale_y_continuous(

        name =
          "Średnie zużycie energii / Average electricity consumption [kWh/100 km]",

        sec.axis =

          sec_axis(

            ~ . /
              scale_factor,

            name =
              "Zużycie benzyny / Fuel consumption [l/100 km]"
          )
      ) +


      # ----------------------------------------------------
      # OŚ CZASU / TIME AXIS
      # ----------------------------------------------------

      scale_x_datetime(

        date_breaks =
          "1 month",

        date_labels =
          "%m.%Y"
      ) +


      labs(

        title =
          "Przejazdy według rodzaju napędu w czasie / Trips by powertrain mode over time",

        subtitle =
          "Zielony = elektryczny / Green = electric | Pomarańczowy = mieszany / Orange = mixed-mode | Czerwony = spalinowy / Red = combustion",

        x =
          "Data przejazdu / Trip date"
      ) +


      theme_minimal() +

      theme(

        plot.title =
          element_text(
            hjust = 0.5
          ),

        plot.subtitle =
          element_text(
            hjust = 0.5,
            size = 10
          ),

        axis.text.x =
          element_text(

            angle = 45,

            hjust = 1
          ),

        axis.title.y.left =
          element_text(

            color =
              COL_EV,

            face =
              "bold"
          ),

        axis.text.y.left =
          element_text(
            color =
              COL_EV
          ),

        axis.title.y.right =
          element_text(

            color =
              COL_FUEL_MEAN,

            face =
              "bold"
          ),

        axis.text.y.right =
          element_text(

            color =
              COL_FUEL_MEAN
          )
      )
  })


  # ========================================================
  # DZIENNY PRZEBIEG
  # DAILY DISTANCE
  # ========================================================

  output$daily_distance_plot <- renderPlot({

    trip <- data_processed()


    daily_source <- trip[
      !is.na(trip$Dzien) &
        !is.na(trip$Mileage.in.km),
    ]


    validate(

      need(
        nrow(daily_source) > 0,
        "Brak danych do obliczenia dziennego przebiegu. / No data available to calculate daily distance."
      )
    )


    # ------------------------------------------------------
    # SUMA KILOMETRÓW DLA KAŻDEGO DNIA
    # TOTAL DISTANCE FOR EACH DAY
    # ------------------------------------------------------

    daily <- aggregate(

      Mileage.in.km ~ Dzien,

      data = daily_source,

      FUN = sum,

      na.rm = TRUE
    )


    names(daily)[2] <-
      "Dzienny_przebieg_km"


    # ------------------------------------------------------
    # PEŁNY ZAKRES DNI
    # również dni z przebiegiem 0 km
    #
    # COMPLETE DATE RANGE
    # including days with 0 km
    # ------------------------------------------------------

    wszystkie_dni <- data.frame(

      Dzien = seq(

        min(
          daily$Dzien,
          na.rm = TRUE
        ),

        max(
          daily$Dzien,
          na.rm = TRUE
        ),

        by = "day"
      )
    )


    daily <- merge(

      wszystkie_dni,

      daily,

      by = "Dzien",

      all.x = TRUE
    )


    daily$Dzienny_przebieg_km[
      is.na(
        daily$Dzienny_przebieg_km
      )
    ] <- 0


    # ------------------------------------------------------
    # ŚREDNI DZIENNY PRZEBIEG
    # AVERAGE DAILY DISTANCE
    # ------------------------------------------------------

    mean_daily <- mean(
      daily$Dzienny_przebieg_km,
      na.rm = TRUE
    )


    # ======================================================
    # WYKRES / PLOT
    # ======================================================

    ggplot(

      daily,

      aes(

        x = Dzien,

        y = Dzienny_przebieg_km
      )

    ) +

      geom_col(

        fill =
          COL_DAILY,

        width =
          0.8,

        alpha =
          0.85
      ) +

      geom_hline(

        yintercept =
          mean_daily,

        linetype =
          "dashed",

        color =
          "steelblue4",

        linewidth =
          1.1
      ) +

      annotate(

        "label",

        x =
          min(
            daily$Dzien,
            na.rm = TRUE
          ) +

          as.integer(

            (
              max(
                daily$Dzien,
                na.rm = TRUE
              ) -

              min(
                daily$Dzien,
                na.rm = TRUE
              )
            ) *

              0.02
          ),

        y =
          mean_daily,

        label =
          paste0(

            "Średni dzienny przebieg / Average daily distance: ",

            round(
              mean_daily,
              1
            ),

            " km"
          ),

        color =
          "steelblue4",

        fill =
          "white",

        label.size =
          0.35,

        hjust =
          0,

        vjust =
          -0.6,

        size =
          4.2
      ) +

      scale_x_date(

        date_breaks =
          "1 month",

        date_labels =
          "%m.%Y",

        expand =
          expansion(
            mult = c(
              0.01,
              0.01
            )
          )
      ) +

      labs(

        title =
          "Dzienny przebieg / Daily distance",

        subtitle =
          "Łączna liczba kilometrów przejechanych każdego dnia / Total distance travelled each day",

        x =
          "Data / Date",

        y =
          "Dzienny przebieg / Daily distance [km]"
      ) +

      theme_minimal() +

      theme(

        plot.title =
          element_text(
            hjust = 0.5
          ),

        plot.subtitle =
          element_text(
            hjust = 0.5
          ),

        axis.text.x =
          element_text(

            angle = 45,

            hjust = 1
          ),

        axis.title.y =
          element_text(
            face = "bold"
          )
      )
  })


  # ========================================================
  # PODGLĄD TABELI / DATA PREVIEW
  # ========================================================

  output$table <- renderTable({

    head(
      data_processed(),
      20
    )
  })


  # ========================================================
  # ZAPIS CSV / CSV EXPORT
  # ========================================================

  output$download <- downloadHandler(

    filename = function() {

      "wynik_EV_spalinowy_EV_combustion_results.csv"
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


# ==========================================================
# START APLIKACJI / START APPLICATION
# ==========================================================

shinyApp(
  ui = ui,
  server = server
)
