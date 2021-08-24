source('C:/rprojects/alberlet/R/functions.R')

type <- c(
  "lakas", "haz", "szobat-kinalo-hirdetesek"
  
)

url_total <- c()

for (type in c("lakas", "haz", "szobat-kinalo-hirdetesek")) {
  url_total_type <- c()
  go <- TRUE
  page_n <- 1
  print(str_c("# ", type, " ----------------------------------"))
  while (go) {
    url <-  str_c("https://alberlet.hu/kiado_alberlet/page:", page_n, "/ingatlan-tipus:", type, "/keres:normal/limit:24")
    page <- SafelyRead(url)
    url_room <- GetURL(page, "div div div div div div div div div a") %>% 
      unique() %>% 
      keep(~ str_detect(., "https")) %>% 
      keep(~ !(. %in% url_total_type))
    
    if (length(url_room) == 0) {
      go = F
    } else {
      
      message(str_c("PAGE: ", page_n))
      url_total_type <- c(url_total_type, url_room)
      
    }
    page_n <- page_n + 1
  }
  url_total <- c(url_total, url_total_type)
}

url <- "https://www.alberlet.hu/kiado_alberlet/budapest-12-kerulet-tusnadi-utca-48m2-2-szoba_851916"

room_data <- map(unique(url_total), function(url) {
page <- SafelyRead(url) 
page %>% 
  html_table(fill = T) %>% 
  keep(~ ncol(.) == 2) %>% 
  map(~ mutate_all(set_names(., "x", url), as.character)) %>% 
  reduce(rbind) %>% 
  mutate(
    x = ifelse(str_detect(x, "Közös költségben"), "közös költség", x)
  ) %>% 
  rbind(
    page %>% 
      GetHTMLText("p:nth-child(1)") %>% 
      str_c(collapse = " ") %>% 
      {data.frame(x = "comment", y = .)} %>% 
      set_names("x", url)
  ) %>% 
  rbind(
    page %>% 
      GetHTMLText("#listing-850571 > div:nth-child(5) > div.col-md-8.profile__content-col > div > div.profile__description > div") %>% 
      str_c(collapse = " ") %>% 
      {data.frame(x = "profile_text", y = .)} %>% 
      set_names("x", url)
  )


})

room_data <- room_data %>% 
  reduce(full_join) %>% 
  t() %>% 
  data.frame() %>% 
  {set_names(.[-1, ], .[1, ])}

saveRDS(room_data, str_c("c:/rprojects/alberlet/data/alberlet_data_", Sys.Date(), ".RDS"))
