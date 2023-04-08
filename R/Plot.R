# Get configuration ----
conf <- config::get()

# Load the data ----
con <- dbConnect(
  SQLite(),
  conf$SQLITE$PATH
)
tables <- dbListTables(con)
data.raw <- dbReadTable(con, "all.shaped")
dbDisconnect(con)

# Shape the data ----

## Set levels ----
levels.range.dinner <- data.raw |>
  arrange(price.dinner.min) |>
  select(price.dinner.range) |>
  distinct() %>%
  .[,1]
levels.range.lunch <- data.raw |>
  arrange(price.lunch.min) |>
  select(price.lunch.range) |>
  distinct() %>%
  .[,1]

data <- data.raw |>
  mutate(
    ts = as.POSIXct(ts, orign = "1970-01-01"),
    price.dinner.range = factor(price.dinner.range, levels.range.dinner),
    price.lunch.range = factor(price.lunch.range, levels.range.lunch),
    across(where(is.character), as.factor),
    across(c(uid,url), as.character)
  )

# Plot ----
## Summarize the data ----
data.this <- data |>
  group_by(price.dinner.range, price.lunch.range) |>
  summarise(
    rate.min = min(rate, na.rm = TRUE),
    rate.q1 = quantile(rate, c(0.25, 0.5, 0.75), na.rm = TRUE)[1],
    rate.med = quantile(rate, c(0.25, 0.5, 0.75), na.rm = TRUE)[2],
    rate.mean = mean(rate, na.rm = TRUE),
    rate.q3 = quantile(rate, c(0.25, 0.5, 0.75), na.rm = TRUE)[3],
    rate.max = max(rate, na.rm = TRUE),
    .groups = "drop"
  )
## Tile plot ----
### Generte plots ----
plot.base <- ggplot(data.this, aes(x = price.lunch.range, y = price.dinner.range)) +
  scale_fill_gradient(low = "#ffeeeeee", high = "darkred") +
  theme_classic(base_size = 8)  +
  theme(
    legend.position = c(0.9 ,0.2),
    legend.background = element_rect(fill = rgb(1, 1, 1, 0.6)),
    axis.text.x = element_text(
      angle = 15,
      vjust = 1,
      hjust = 1
    )
  ) +
  labs(x = "lunch price", y = "dinner price")
# plot.min <- plot.base + geom_tile(aes(fill = rate.min)) + labs(fill = "rate (min)")
# plot.q1 <- plot.base + geom_tile(aes(fill = rate.q1)) + labs(fill = "rate (q1)")
# plot.med <- plot.base + geom_tile(aes(fill = rate.med)) + labs(fill = "rate (median)")
plot.mean <- plot.base + geom_tile(aes(fill = rate.mean)) + labs(fill = "rate (mean)")
# plot.q3 <- plot.base + geom_tile(aes(fill = rate.q3)) + labs(fill = "rate (q3)")
# plot.max <- plot.base + geom_tile(aes(fill = rate.max)) + labs(fill = "rate (max)")
# plots <- list(min = plot.min, median = plot.med, mean = plot.mean, max = plot.max)

### Show -----
plot.mean +
  labs(title = "TABLELOG RATE") +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 12,
      hjust = 0.5,
      vjust = 0.5,
      margin = margin(t = 10, b = 10, unit = "pt")
    )
  )
ggarrange(
  plotlist = plots,
  labels = c("min", "median", "mean", "max"),
  ncol = 2,
  nrow = 2
)

## Contour plot ----

### Modeling ----
#### Simple OLS ----
model <- lm(rate ~ price.dinner.range + price.lunch.range + prefecture, data)
summary(model)
plot(model)
#### Check correlations ----
data.fcts <- data |>
  select_if(function(x)  is.numeric(x) || is.factor(x)) |>
  select(!place)
chisq.test(data.fcts$price.dinner.range, data.fcts$price.lunch.range)

### Plot Contour ----
data.fcts <- data.fcts |>
  mutate(
    price.dinner.mid =  round((price.dinner.max + price.dinner.min) / 2, digits = -2) ,
    price.lunch.mid = round((price.lunch.max + price.lunch.min) / 2, digits = -2)
  )
data.this <- data.fcts|>
  group_by(price.dinner.mid, price.lunch.mid) |>
  summarise(
    n = n(),
    rate.mean = round(mean(rate, na.rm = TRUE), digits = 2),
    comments.mean = round(mean(comments, na.rm = TRUE)),
    bookmarks.mean = round(mean(bookmarks, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  distinct() |>
  mutate(
    freq = n/sum(n)
  )

model <- loess(rate.mean ~ price.dinner.mid + price.lunch.mid, data = data.this)
new.df <- expand.grid(price.dinner.mid = data.this$price.dinner.mid, price.lunch.mid = data.this$price.lunch.mid)
rate.mean <- predict(model, newdata = new.df) |> as.vector()
df <- cbind(new.df, rate)

ggplot(df, aes(x = price.lunch.mid, y = price.dinner.mid, z = rate.mean)) +
  labs(
    x = "lunch price",
    y = "dinner price",
    fill = "rate (mean)",
    title = "TABELOG RATING MAP",
    caption = "Results has been predicated by tabelog site data."
  ) +
  theme_classic(base_size = 8) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 12,
      hjust = 0.5,
      vjust = 0.5
    ),
    legend.position = c(0.9, 0.5),
    legend.background = element_rect(fill = rgb(1,1,1,0.6)),
    axis.text.x = element_text(
      hjust = 0.5,
      vjust = 1
    )
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_contour_filled(na.rm = TRUE, binwidth = 0.1)
