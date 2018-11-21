library(tidyverse)
library(tidytext)

screenplay <- readLines("'PULP FICTION' -- by Quentin Tarantino & Roger Avary.txt")

screenplay <- 
  screenplay %>% 
  keep(~ grepl(x = ., pattern = "                       ")) %>% 
  discard(~ grepl(x = ., pattern = "                                                                "))

screenplay_df <- data_frame(lines = screenplay)

screenplay_df <- 
  screenplay_df %>% 
  mutate(isName = grepl(x = lines, pattern = "                                    "))# %>% 
  # filter(isName = isName& !lead(isName))



screenplay_df %>% filter(isName, !lead(isName, n = 1))%>% distinct(lines) %>% unlist(use.names = F)
entity <- c(1, rep(0, nrow(screenplay_df) -1))

for(i in seq_along(screenplay_df$isName)[-1]){
  entity[i] <- entity[i-1] + screenplay_df$isName[i]
}

screenplay_df <- mutate(screenplay_df,
                        entity = entity)
#                         isName = if_else(isName, "name", "line"))

names <- screenplay_df %>% filter(isName) %>% select(-isName, name = lines)
lines <- screenplay_df %>% filter(!isName) %>% select(-isName)

screenplay_df <- left_join(lines, names, by = "entity")

screenplay_df <- 
  screenplay_df %>% 
  mutate(lines = gsub(x = lines, pattern = "\\s+", replacement = " "),
         name = gsub(x = name, pattern = "\\s+", replacement = " "),
         name = gsub(x = name, pattern = "(O.S.)|(V.O.)", replacement = ""),
         name = gsub(x = name, pattern = "\\(\\)", replacement = ""),
         name = case_when(name == " YOUNG MAN" ~ "RONDO",
                          name == " YOUNG WOMAN" ~ "YOLANDA",
                          name == " PUMPKIN" ~ "RONDO",
                          name == " HONEY BUNNY" ~ "YOLANDA",
                          name == " (PAUSE)" ~ "BUTCH",
                          name == " (TO JULES)" ~ "THE WOLF",
                          name == " WINSTON" ~ "THE WOLF",
                          TRUE ~ name),
         name = trimws(name, "both"),
         lines = trimws(lines, "both")) %>% 
  filter(name != "FADE TO BLACK") %>% 
  filter(!grepl(x = lines, "\\("), !grepl(x = lines, "\\)"))

screenplay_df %>%  distinct(name) %>% unlist(use.names = F)

screenplay_tokenized <- 
  screenplay_df %>% 
  unnest_tokens("word", "lines")
