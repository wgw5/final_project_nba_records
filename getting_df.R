library(rvest)

base_url = "http://espn.go.com/nba/standings/_/season/"
year = 2016:2003
yr_url = sapply(seq_along(year), function(x) (paste0(base_url, year[x])))
ugh = read_html(yr_url[1])
ugh
str(ugh)
read_url = sapply(seq_along(year), function(x) read_html(yr_url[x]))
str(read_url)
read_url['node',] %>% html_nodes(".team-names") %>% html_text()
read_url[1] %>% html_nodes(".team-names") %>% html_text()
yr_url
try = read_html(full_url)
try
teams = try %>% html_nodes(".team-names") %>% html_text()
wins = try %>% html_nodes(".team+ td")%>% html_text() %>% as.numeric()
loss = try %>% html_nodes("td:nth-child(3)") %>% html_text() %>% as.numeric()
ppg = try %>% html_nodes("td:nth-child(10)") %>% html_text() %>% as.numeric()
opg = try %>% html_nodes("td:nth-child(11)") %>% html_text() %>% as.numeric()

NBA = lapply(seq_along(year), function(x) data.frame(
  Year = rep(year[x], length(read_html(yr_url[x])%>% html_nodes(".team-names") %>% html_text())),
  Team =  read_html(yr_url[x])%>% html_nodes(".team-names") %>% html_text(),
  Wins = read_html(yr_url[x]) %>% html_nodes(".team+ td")%>% html_text() %>% as.numeric(),
  Losses = read_html(yr_url[x]) %>% html_nodes("td:nth-child(3)") %>% html_text() %>% as.numeric(),
  PPG = read_html(yr_url[x]) %>% html_nodes("td:nth-child(10)") %>% html_text() %>% as.numeric(),
  OPG = read_html(yr_url[x]) %>% html_nodes("td:nth-child(11)") %>% html_text() %>% as.numeric(),
  
  stringsAsFactors = FALSE
))
df = do.call(rbind, NBA)
View(df)
