library(shiny)
library(ggplot2)

ui <- fluidPage(

  titlePanel(
    "Proporcja użycia silnika spalinowego i elektrycznego dla Skoda Kodiaq iV (PHEV)"
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

      h4("Podsumowanie"),
      verbatimTextOutput("summary"),

      hr(),

      h4("Wykres kołowy"),
      plotOutput("donut"),

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

    validate(

      need(
        "Average.fuel.consumption.in.l.100km" %in% names(trip),
        "Brak kolumny: Average.fuel.consumption.in.l.100km"
      ),

      need(
        "Mileage.in.km" %in% names(trip),
        "Brak kolumny: Mileage.in.km"
      )
    )

    trip$Average.fuel.consumption.in.l.100km <-
      as.numeric(trip$Average.fuel.consumption.in.l.100km)

    trip$Mileage.in.km <-
      as.numeric(trip$Mileage.in.km)

    # Udział ICE oszacowany na podstawie spalania benzyny
    trip$ICE_share <-
      trip$Average.fuel.consumption.in.l.100km / input$b

    # Ograniczenie wyniku do zakresu 0-1
    trip$ICE_share <-
      pmax(0, pmin(1, trip$ICE_share))

    # Szacowany ekwiwalent kilometrów ICE
    trip$ICE_km <-
      trip$Mileage.in.km * trip$ICE_share

    # Pozostała część dystansu
    trip$EV_km <-
      trip$Mileage.in.km - trip$ICE_km

    trip
  })


  output$summary <- renderText({

    trip <- data_processed()

    total_km <-
      sum(trip$Mileage.in.km, na.rm = TRUE)

    total_ice <-
      sum(trip$ICE_km, na.rm = TRUE)

    total_ev <-
      sum(trip$EV_km, na.rm = TRUE)

    ice_percent <-
      total_ice / total_km * 100

    ev_percent <-
      total_ev / total_km * 100

    paste0(
      "Całkowity przebieg: ",
      round(total_km, 1),
      " km\n\n",

      "ICE: ",
      round(total_ice, 1),
      " km (",
      round(ice_percent, 1),
      "%)\n",

      "EV: ",
      round(total_ev, 1),
      " km (",
      round(ev_percent, 1),
      "%)"
    )
  })


  output$donut <- renderPlot({

    trip <- data_processed()

    total_ice <-
      sum(trip$ICE_km, na.rm = TRUE)

    total_ev <-
      sum(trip$EV_km, na.rm = TRUE)

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
          "ICE" = "orange",
          "EV" = "darkgreen"
        ),
        labels = c(
          "ICE" = "Benzyna",
          "EV" = "Elektryczny"
        )
      ) +

      labs(
        title = "Szacowany udział przebiegu ICE vs EV",
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


  output$table <- renderTable({

    trip <- data_processed()

    head(
      trip,
      20
    )
  })


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
