#Part 1 CSS Selector
library(rvest)
url1 <- 'https://en.wikipedia.org/wiki/Depreciation'
html_path <- '#mw-content-text > div.mw-parser-output > table:nth-child(36)'
uf <- read_html(url1) %>% html_nodes(html_path) %>% html_table() %>% '[['(1)
write.csv(uf, file = "dep1.csv")

url2 <- 'https://en.wikipedia.org/wiki/Depreciation'
html_path1 <- '#mw-content-text > div.mw-parser-output > table:nth-child(40)'
uf1 <- read_html(url2) %>% html_nodes(html_path1) %>% html_table() %>% '[['(1)

write.csv(uf1, file = "dep2.csv")


#Part 2 API
library(httr)
library(jsonlite)
library(dplyr)


token <- "7b27fcb1fd8bb89633b50d890970ca7155de27cd"

ticker <- "AAPL"
start_date <- "2022-01-01"
end_date <- "2022-02-28"

url <- paste0("https://api.tiingo.com/tiingo/daily/", ticker,
              "/prices?startDate=", start_date,
              "&endDate=", end_date, "&token=", token)

response <- GET(url)
df2 <- fromJSON(content(response, as = "text")) %>%
  select(date, close)

print(df2)




