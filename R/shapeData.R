# Get configuration info ----
conf <- config::get()
# Load the database ----
## Database connection ----
con <- dbConnect(
  SQLite(),
  conf$SQLITE$PATH
)
tables <- dbListTables(con)

## Load the all data ----
data.raw <- dbReadTable(con, "all")

## Disconnect DB ----
dbDisconnect(con)

# Shape the data ----
data <- data.raw |>
  mutate(
    ts = as.POSIXct(ts, origin = "1970-01-01 00:00:00"),
    across(
      c(price.dinner, price.lunch),
      list(
        min = function(x) {
          stri_replace_all(x, "", regex = "[￥,]") %>%
            sub("～.*", "", .) %>%
            ifelse(. == "", 0, .) %>%
            ifelse(. == "-", NA, .) |>
            as.numeric()
        },
        max = function(x) {
          stri_replace_all(x, "", regex = "[￥,]") %>%
            sub(".*～", "", .) %>%
            ifelse(. == "", 0, .) %>%
            ifelse(. == "-", NA, .) |>
            as.numeric()
        }
      ),
      .names = "{.col}.{.fn}"
    ),
    price.dinner.range = ifelse(is.na(price.dinner.max), NA, paste(formatC(price.dinner.min, digits = 0, format = "f",big.mark = ","), "~", formatC(price.dinner.max, digits = 0, format = "f",  big.mark = ","))),
    price.lunch.range = ifelse(is.na(price.lunch.max), NA, paste(formatC(price.lunch.min, digits = 0, format = "f", big.mark = ","), "~", formatC(price.lunch.max, digits = 0, format = "f",  big.mark = ",")))
  ) |>
  arrange(price.dinner.max, price.lunch.max, prefecture) |>
  mutate(
    across(where(is.character), as.factor),
    across(c(uid, url), as.character)
  )

# Push to DB ----
## Convert the data ti SQLite data types ----
data.toWrite <- data |>
  mutate(
    ts = as.character(ts),
    across(where(is.factor), as.character)
  )

## Connect to DB ----
con <- dbConnect(
  SQLite(),
  conf$SQLITE$PATH
)

## Push the data ----
dbWriteTable(
  con,
  "all.shaped",
  data.toWrite,
  overwrite = TRUE
)

## Check the data
dbReadTable(con, "all.shaped") |> head()

## Disconnect DB ----
dbDisconnect(con)
