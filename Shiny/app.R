# Loading --------
# App
library(shiny) # Shiny application
library(shinyWidgets) # Upper widgets
# Plot
library(ggplot2) # Plot
library(plotly) # Interactive plot
# Data manipulation
library(tidyverse) # Data manipulation
# Data table
library(DT) # Interactive table
# Data base
library(DBI) # Data base management
library(RSQLite) # SQLite

# Pre-process ----------
conn <- dbConnect(SQLite(), "tabelog.db")
tbls <- dbListTables(conn)
statement <- sqlInterpolate(
  conn,
  "
      select prefecture, mean
      from 'prefectures'
      "
)
res <- dbSendQuery(conn,statement)
prefs <- dbFetch(res)
dbClearResult(res)

# ui -----------
ui <- navbarPage(
  "TABELOG OVERVIEW",

  tabPanel(
    "PLOT",
    sidebarPanel(
      pickerInput(
        "plt.pref.slc",
        label = "PREFECTURES",
        choices = prefs$prefecture,
        selected = prefs$prefecture,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      sliderInput(
        "plt.mean.sldr",
        label = "MEAN RATE",
        min = min(prefs$mean),
        max = max(prefs$mean),
        value = c(min(prefs$mean), max(prefs$mean)),
        round = -2
      )
    ),
    mainPanel(
      plotlyOutput("plot",height = "1000px")
    )
  ),

  tabPanel(
    "TABLE",
    tabsetPanel(

      tabPanel(
        "PLACES",
        dataTableOutput("tbl.rates")
      ),

      tabPanel(
        "PREFECTURES",
        dataTableOutput("tbl.prefs")
      )
    )
  )
)

# server --------
server <- function(input, output, session) {

  output$plot <- renderPlotly({

    # Fetch the rating
    statement <- sqlInterpolate(
      conn,
      "
      select rate, prefecture
      from 'all'
      where
        rate is not null
      "
    )
    res <- dbSendQuery(conn, statement)
    df.this <- dbFetch(res)
    dbClearResult(res)

    # Fetch the prefecture
    statement <- sqlInterpolate(
      conn,
      "
      select prefecture, mean
      from 'prefectures'
      where
        mean between ?min and ?max
      ",
      min = input$plt.mean.sldr[1],
      max = input$plt.mean.sldr[2]
    )
    res <- dbSendQuery(conn,statement)
    prefs.this <- dbFetch(res)
    dbClearResult(res)

    # Arrange the data
    prefs.selected <- input$plt.pref.slc
    prefs.this <- prefs.this %>%
      filter(
        prefecture %in% prefs.selected
      )
    df.this <- df.this %>%
      filter(
        prefecture %in% prefs.this$prefecture
      )

    # factorise
    df.this$prefecture <- factor(df.this$prefecture, prefs.this$prefecture[order(-prefs.this$mean)])


    # Get the length
    lvls <- levels(df.this$prefecture)
    len <- length(lvls)
    top.prefs <- c("tochigi", "tokyo", lvls[1:min(5,len)], "others", lvls[max(len-4,1):len]) %>%
      .[!duplicated(.)]

    # Plotting
    plot <- df.this %>%
      mutate(
        pref.edge  = ifelse(prefecture %in% top.prefs, as.character(prefecture), "others") %>%
          factor(levels = top.prefs )
      ) %>%
      ggplot(aes(prefecture, rate)) +
      geom_boxplot(aes(color = pref.edge)) +
      scale_color_manual(
        name = "Prefectures",
        values = c(
          "tokyo" = "darkblue",
          "tochigi" = "darkred",
          "others" = "gray"
        )
      ) +
      geom_hline(yintercept =  mean(input$plt.mean.sldr)) +
      coord_flip()

    # Return plot
    ggplotly(plot)
  })

  output$tbl.rates <- renderDataTable({

    # Fetch the rating
    statement <- sqlInterpolate(
      conn,
      "
      select prefecture, place, rate, price_dinner, price_lunch, comments, bookmarks, url
      from 'all'
      "
    )
    res <- dbSendQuery(conn, statement)
    df.this <- dbFetch(res)
    dbClearResult(res)

    datatable(df.this, filter = "top")
  })

  output$tbl.prefs  <- renderDataTable({

    # Fetch the prefecture
    statement <- sqlInterpolate(
      conn,
      "
      select prefecture, restaurants, mean as rate_mean
      from 'prefectures'
      "
    )
    res <- dbSendQuery(conn,statement)
    prefs.this <- dbFetch(res)
    dbClearResult(res)

    datatable(prefs.this, filter = "top")
  })
}

# run -------------
shinyApp(ui, server)
