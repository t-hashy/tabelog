# Loading --------
source("./R/global.R")


# List of prefectures -------------
prefectures <- read.csv("./DB/prefectures.csv") %>%
  reframe(
    prefecture = stri_match_first(prefecture_en, regex =  "\\w+") %>% tolower() %>% iconv(to ='ASCII//TRANSLIT')
  )

time.before <- NULL
conf <- config::get()
pb <- create_new_pb(length(prefectures$prefecture))
for(prefecture.this in prefectures$prefecture){

  # Progress bar
  pb$tick()

  # Get top page contents
  url <- paste(conf$URL_BASE, prefecture.this, "/", conf$URL_LIST, sep = "")
  time.after <- Sys.time()
  html.this <-  read_html(url)
  time.diff <- ifelse(is.null(time.before), 5, difftime(time.after, time.before, units = "secs")) %>%
    as.numeric()
  time.sleep <- ifelse(time.diff > 5, 0, 5 - time.diff)
  Sys.sleep(time.sleep)
  time.before <- Sys.time()

  # Get possible page number
  prefectures$restaurants[prefectures$prefecture == prefecture.this] <- html.this %>%
    html_nodes(".c-page-count__num strong") %>%
    html_text() %>%
    .[3] %>%
    as.numeric()

  # Set page number to crawl
  prefectures$pages[prefectures$prefecture == prefecture.this] <- ceiling(rests.num.this/rests.num.per.page) %>%
    min(., conf$MAX_PAGE_NUM)
}
# Save the data
conn <- dbConnect(SQLite(), "./DB/tabelog.db")
dbWriteTable(conn,"prefectures", prefectures, overwrite = TRUE)

# Generate function per prefecture ---------------
get_tablelog.data <- function(pref.this = "all", rests.num.this = NULL,  conf = config::get(), rests.num.per.page = 20){

  # Get top page contents
  url.this <- ifelse(
    pref.this == "all",
    paste(conf$URL_BASE,conf$URL_LIST, sep = ""),
    paste(conf$URL_BASE, pref.this, "/", conf$URL_LIST, sep = "")
  )

  # Get possible page number
  pages.this <- ifelse(rests.num.this > conf$MAX_PAGE_NUM, conf$MAX_PAGE_NUM, rests.num.this)

  print(paste("Prefecture:", pref.this, "Restaurants:", rests.num.this, "| Pages:", pages.this))

  # Crawling each pages

  # Set basics
  df <- data.frame()

  # Crawling
  pb <- create_new_pb(pages.this)
  time.before <- NULL
  for(page.this in 1:pages.this){

    pb$tick()

    # Get html
    tryCatch(
      {
        time.after <- Sys.time()
        html.this <- read_html(paste(url.this, page.this, sep = ""))
        time.diff <- ifelse(is.null(time.before), 5, difftime(time.after, time.before, units = "secs")) %>%
          as.numeric()
        time.sleep <- ifelse(time.diff > 5, 0, 5 - time.diff)
        Sys.sleep(time.sleep)
        time.before <- Sys.time()
      },
      error = function(e) {
        print(paste("ERROR in top:",e))
      }
    )

    # Get restaurants
    nodes.rests <- html.this %>%
      html_nodes("div .list-rst")

    # With each restaurant
    for(i in 1:length(nodes.rests)){

      # Get node
      nodes.rest <- nodes.rests[i]

      # Get contents
      rest.name.this <- nodes.rest  %>%
        html_nodes(".list-rst__rst-name-target") %>%
        html_text() %>%
        as.character()
      rest.url.this <- nodes.rest %>%
        html_nodes(".list-rst__rst-name-target") %>%
        html_attr(name = "href") %>%
        as.character()
      rest.pref.this <- pref.this
      if(pref.this == "all"){
        rest.pref.this <- nodes.rest %>%
          html_nodes(".list-rst__area-genre") %>%
          html_text() %>%
          stri_match(regex = "\\[\\w+\\]") %>%
          stri_match(regex = "\\w+") %>%
          as.character()
      }
      rest.rating.this<- nodes.rest %>%
        html_nodes(".c-rating__val") %>%
        html_text() %>%
        as.numeric()
      rest.comments.num.this <- nodes.rest %>%
        html_nodes(".list-rst__rvw-count-target") %>%
        html_text() %>%
        stri_extract(regex = "\\d+") %>%
        as.numeric()
      if(length(rest.comments.num.this) == 0){
        rest.comments.num.this <- 0
      }
      rest.booked.num.this <- nodes.rest %>%
        html_nodes(".list-rst__save-count-num") %>%
        html_text() %>%
        as.numeric()
      rest.price.ranges.this <- nodes.rest %>%
        html_nodes(".c-rating-v3__val") %>%
        html_text() %>%
        as.character()
      rest.price.ranges.dinner.this <- rest.price.ranges.this[1]
      rest.price.ranges.lunch.this <- rest.price.ranges.this[2]

      # Set uids
      uids <- list()
      if(length(df) > 0){
        uids <- df$uid
      }

      # Make df temp
      tryCatch(
        {
          df.this <- data.frame(
            ts = time.after,
            uid = generate_uid(uids),
            page = page.this,
            order = i,
            place = rest.name.this %>% ifelse(length(.) == 0, "",.),
            price.dinner = rest.price.ranges.dinner.this %>% ifelse(length(.) == 0, "",.),
            price.lunch = rest.price.ranges.lunch.this %>% ifelse(length(.) == 0, "", .),
            rate = rest.rating.this %>% ifelse(length(.) == 0, NA, .),
            comments = rest.comments.num.this %>% ifelse(length(.) == 0, 0, .),
            bookmarks = rest.booked.num.this %>% ifelse(length(.) == 0, 0, .),
            prefecture = rest.pref.this %>% ifelse(length(.) == 0, "", .),
            url = rest.url.this %>% ifelse(length(.) == 0, "", .)
          )

          # Push to the main df
          if(length(df) == 0){
            df <- df.this
          }else{
            df <- bind_rows(df, df.this)
          }
        },
        error = function(e) {
          print(paste("ERROR in page:",e))
          print(
            c(
              rest.name.this,
              rest.price.ranges.dinner.this,
              rest.price.ranges.lunch.this,
              rest.rating.this,
              rest.comments.num.this,
              rest.booked.num.this,
              rest.pref.this,
              rest.url.this
            )
          )
        }
      )
    }
  }

  return(df)
}

# Fetch the data ----------------

# Get data for each prefecture
df.all <- data.frame()
conn <- dbConnect(SQLite(), "./DB/tabelog.db")
for(pref.this in prefectures$prefecture){
  df.this <- get_tablelog.data(pref.this, rests.num.this = prefectures$restaurants[prefectures$prefecture == pref.this])
  dbWriteTable(conn,"all",df.this, append = TRUE)
  df.all <- rbind(df.all,df.this)
}
##
# Shape the data --------

# # Add prefecture column
# df.shaped <- df.all %>%
#   mutate(
#     prefecture = rep(prefectures[,1], each = conf$MAX_PAGE_NUM, length.out = length(df.all$prefecture))
#   )

# Duplicatoin check
df <- df.all[!duplicated(df.all %>% select(!uid)),] %>%
  mutate(
    price.dinner = as.factor(price.dinner),
    price.lunch = as.factor(price.lunch),
    prefecture = as.factor(prefecture)
  )

# Slect only for stats
df.toStats <- df %>%
  select(!c(uid, ts, place, url))

# Make price ranges as ordered factor


# Make aggregated factor
df.toStats <- df.shaped %>%
  select(!c(ts, uid, place, url)) %>%
  mutate(
    pref.aggr = case_when(
      prefecture == "tokyo" ~ "tokyo",
      prefecture == "osaka" ~ "osaka",
      TRUE ~ "others")
  ) %>%
  mutate(
    pref.aggr = factor(pref.aggr, c("tokyo", "osaka", "others"))
  )


# Stats --------------

# Check the basic Stats
summary(df.toStats)
glimpse(df.toStats)

# Create report
create_report(df.toStats, output_file = paste("report_",format(as.Date(), "%Y%m%d"), ".html", sep = ""))

# Variables
ggpairs(
  df.toStats %>% select(rate, comments, bookmarks, page, pref.aggr),
  aes(color = pref.aggr, alpha = 0.5),
  lower = list(continuous = "smooth", combo = "facetdensity"),
  diag = list(continuous = "density")
)

# Histogram
plot <- ggplot(df.toStats)+
  geom_histogram(
    aes(rate),
    binwidth = 0.01
  ) +
  facet_wrap(pref.agggr)
plotly(plot)


