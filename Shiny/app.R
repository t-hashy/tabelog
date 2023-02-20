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

# Connect a data base
conn <- dbConnect(SQLite(), "tabelog.db")
tbls <- dbListTables(conn)

# Fetch the prefectures
statement <- sqlInterpolate(
  conn,
  "
      select prefecture, mean, restaurants
      from 'prefectures'
      "
)
res <- dbSendQuery(conn,statement)
prefs <- dbFetch(res)
dbClearResult(res)

# Set levels
lvls <- prefs$prefecture[order(-prefs$mean)]
prefs$prefecture <- factor(prefs$prefecture, lvls)

# Fetch the rating
statement <- sqlInterpolate(
  conn,
  "
      select prefecture, place, rate, price_dinner, price_lunch, comments, bookmarks, url
      from 'all'
      where
        rate is not null
      "
)
res <- dbSendQuery(conn, statement)
df.this <- dbFetch(res) %>%
  mutate(
    prefecture = factor(prefecture, lvls),
    price_dinner = factor(price_dinner, unique(price_dinner)),
    price_lunch = factor(price_lunch, unique(price_lunch))
  )
dbClearResult(res)
##

# ui -----------
ui <- navbarPage(
  "TABELOG OVERVIEW",

  tabPanel(
    "PLOT",
    sidebarPanel(
      pickerInput(
        "plt.pref.slc",
        label = "PREFECTURES",
        choices = as.character(prefs$prefecture),
        selected = as.character(prefs$prefecture),
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

    # Fetch the prefecture
    prefs.this <- prefs %>%
      filter(
        mean >= input$plt.mean.sldr[1],
        mean <= input$plt.mean.sldr[2],
        prefecture %in% input$plt.pref.slc
      )

    # Fetch the data
    df.this <- df.this %>%
      filter(
        prefecture %in% prefs.this$prefecture
      )

    # Get the length
    len <- length(prefs.this$prefecture)
    top.prefs <- c("tochigi", "tokyo", lvls[1:min(5,len)], "others", lvls[max(len-4,1):len]) %>%
      .[!duplicated(.)]

    # Plotting
    plot <- df.this %>%
      mutate(
        pref.edge  = ifelse(prefecture %in% top.prefs, as.character(prefecture), "others") %>%
          factor(levels = top.prefs )
      ) %>%
      ggplot(aes(prefecture, rate)) +
      geom_boxplot(aes(color = pref.edge), notch = TRUE, varwidth = TRUE, show.legend = FALSE) +
      scale_color_manual(
        values = c(
          "tokyo" = "darkblue",
          "tochigi" = "darkred",
          "others" = "gray"
        )
      ) +
      xlab(NULL) +
      geom_hline(yintercept =  mean(input$plt.mean.sldr)) +
      coord_flip()

    # Return plot
    ggplotly(plot)
  })

  output$tbl.rates <- renderDataTable({
    datatable(df.this, filter = "top")
  })

  output$tbl.prefs  <- renderDataTable({
    datatable(prefs, filter = "top")
  })
}

# run -------------
shinyApp(ui, server)
