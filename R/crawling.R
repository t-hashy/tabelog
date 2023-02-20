# Loading --------
source("global.R")

# Generate function ---------------
get_tablelog.data <- function(pref = "all"){
  
  # Set environment --------
  conf <- config::get()
  
  # Get numbers of the restaurants and restaurants per page--------
  
  # Get top page contents
  url <- ifelse(
    pref == "all",
    paste(conf$URL_BASE,conf$URL_LIST, sep = ""),
    paste(conf$URL_BASE, pref, "/", conf$URL_LIST, sep = "")
  )
  html.top <-  read_html(url)
  
  # Get restaurants number
  rests.num <- html.top %>%
    html_nodes(".c-page-count__num strong") %>%
    html_text() %>%
    .[3] %>%
    as.numeric()
  
  # Get possible page number
  rests.num.per.page <- html.top %>%
    html_nodes(".c-page-count__num strong") %>%
    html_text() %>%
    .[2] %>%
    as.numeric()
  
  # Set page number to crawl
  pages <- ceiling(rests.num/rests.num.per.page)
  if(pages > conf$MAX_PAGE_NUM){
    pages <- 20
  }
  
  print(paste("Prefecture:", pref, "Restaurants:", rests.num, "| Pages:", pages))
  
  # Crawling each pages ------------
  
  # Set basics
  df <- data.frame()
  
  # Crawling
  pb <- create_new_pb(pages)
  for(page.this in 1:pages){
    
    pb$tick()
        
    # Get html
    tryCatch(
      {
        Sys.sleep(5)
        html.this <- read_html(paste(url, page.this, sep = ""))
        ts.this <- Sys.time()
      },
      error = function(e) {
        paste("ERROR:",e)
        next
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
      rest.pref.this <- pref
      if(pref == "all"){
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
            ts = ts.this,
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
          print(paste("ERROR:",e))
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

# List of prefectures in Japan
prefectures <- read.csv("./DB/prefectures.csv") %>%
  mutate(
    pref = stri_match_first(prefecture_en, regex =  "\\w+") %>% tolower() %>% iconv(to ='ASCII//TRANSLIT')
  ) %>%
  select(pref)

# Get data for each prefecture
df.all <- data.frame()
conn <- dbConnect(SQLite(), "./DB/tabelog.db")
for(pref.this in prefectures[,1]){
  df.this <- get_tablelog.data(pref.this)
  dbWriteTable(conn,"all",df.this, append = TRUE)
  df.all <- rbind(df.all,df.this)
}

# Shape the data --------

# # Add prefecture column
# df.shaped <- df.all %>%
#   mutate(
#     prefecture = rep(prefectures[,1], each = conf$MAX_PAGE_NUM, length.out = length(df.all$prefecture))
#   )

# Duplicatoin check
df.shaped <- df.shaped[!duplicated(df.shaped %>% select(!uid)),] %>%
  mutate(
    rate = ifelse(rate == 0, NA, rate),
    prefecture = as.factor(prefecture)
  )
 df.shaped %>%
  group_by(prefecture) %>%
  summarise(
    pref.count = n()
  ) %>%
   order_by(pref.count, )
  mutate(
    dist = cum_dist(pref.count)
  )

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


# Recover -----------------

