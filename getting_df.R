library(rvest)
library(dplyr)

#
base_url = "http://espn.go.com/nba/standings/_/season/"
year = 2016:2003
yr_url = sapply(seq_along(year), function(x) (paste0(base_url, year[x])))

htmls = lapply(seq_along(yr_url), function(x) read_html(yr_url[x]))

NBA = lapply(seq_along(year), function(x) data.frame(
  Season = rep(paste0(year[x]-1, "-",year[x]), length(htmls[[x]]%>% html_nodes(".team-names") %>% html_text())),
  Team =  htmls[[x]] %>% html_nodes(".team-names") %>% html_text(),
  Wins = htmls[[x]] %>% html_nodes(".team+ td")%>% html_text() %>% as.numeric(),
  Losses = htmls[[x]] %>% html_nodes("td:nth-child(3)") %>% html_text() %>% as.numeric(),
  PPG = htmls[[x]] %>% html_nodes("td:nth-child(10)") %>% html_text() %>% as.numeric(),
  Opp_PPG = htmls[[x]] %>% html_nodes("td:nth-child(11)") %>% html_text() %>% as.numeric(),
  
  stringsAsFactors = FALSE
))

NBA.df = do.call(rbind, NBA)
View(NBA.df)

NBA.df = NBA.df %>% mutate(Exp_W1 = round((NBA.df$PPG^13.91/(NBA.df$PPG^13.91 + NBA.df$Opp_PPG^13.91))*(NBA.df$Wins+NBA.df$Losses)),
                         Exp_L1 = (NBA.df$Wins + NBA.df$Losses) - round((NBA.df$PPG^13.91/(NBA.df$PPG^13.91 + NBA.df$Opp_PPG^13.91))*(NBA.df$Wins+NBA.df$Losses)),
                         Exp_W2 = round((NBA.df$PPG^16.5/(NBA.df$PPG^16.5 + NBA.df$Opp_PPG^16.5))*(NBA.df$Wins + NBA.df$Losses)),
                         Exp_L2 = (NBA.df$Wins + NBA.df$Losses)- round((NBA.df$PPG^16.5/(NBA.df$PPG^16.5 + NBA.df$Opp_PPG^16.5))*(NBA.df$Wins + NBA.df$Losses)))
NBA.df = NBA.df %>% mutate(err1 = abs(NBA.df$Wins - NBA.df$Exp_W1), err2 = abs(NBA.df$Wins - NBA.df$Exp_W2))

save(NBA.df, file="NBA.Rdata")

