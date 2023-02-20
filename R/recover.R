# Check current data --------------

# Set basics
conn <- dbConnect(SQLite(), "./DB/tabelog.db")

# Check the table
tbls <- dbListTables(conn)

# Read the data
statement <- sqlInterpolate(
  conn,
  "
  select *
  from ?tbl
  ",
  tbl = tbls[1]
)
res <- dbSendQuery(conn,statement)
df.all <- dbFetch(res)
dbClearResult(res)

# Read the data ------------
statement <- sqlInterpolate(
  conn,
  "
  select *
  from ?tbl
  ",
  tbl = tbls[1]
)
res <- dbSendQuery(conn,statement)
df <- dbFetch(res)
dbClearResult(res)

# See the data -------------
length(df$uid[!is.na(df$prefecture)])

# Delete duplicates -----------
statement <- sqlInterpolate(
  conn,
  "
  delete from ?tbl
  where
    prefecture is not null
  ",
  tbl = tbls[1]
)
dbSendStatement(conn,statement)

# Check the prefecture --------------
target.index <- floor(length(df$uid)/(conf$MAX_PAGE_NUM*20))
target.pref <- prefectures$pref[floor(length(df$uid)/(conf$MAX_PAGE_NUM*20))]

# Add prefectures --------------------
head(df[is.na(df$prefecture),])

# Recover --------
