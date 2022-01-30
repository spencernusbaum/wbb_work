#Spencer Nusbaum

# Load Data & Libraries ----

library(shiny)
library(ggplot2)
library(tidyverse)
library(broom)
library(purrr)
library(bslib)
library(dplyr)
library(stringr)
library(shinyjs)
library(plotly)
library(ggiraph)
library(formula.tools)
library(ggpmisc)
library(ggpubr)

# scrape_year <- function(x) {
#     if (str_detect(string = x, pattern = "per_game")) {
#         html_object <-
#             read_html (x)
#         html_object
#         
#         ranking_elements <- html_nodes(html_object,
#                                        css = ".full_table .left , .full_table .center, .full_table .right, #per_game .poptip")
#         ranking_text <- html_text(ranking_elements)
#         
#         wnba <- tibble(text = ranking_text)
#         
#         wnba %>%
#             mutate(rownum = row_number(),
#                    group1 = cumsum(key = rownum %% 28 == 1)) %>%
#             group_by(group1) %>%
#             mutate(group2 = row_number()) %>%
#             mutate(
#                 key = case_when(
#                     group2 == 1 ~ "Player",
#                     group2 == 2 ~ "Team",
#                     group2 == 3 ~ "Pos",
#                     group2 == 4 ~ "G",
#                     group2 == 5 ~ "MP",
#                     group2 == 6 ~ "G",
#                     group2 == 7 ~ "GS",
#                     group2 == 8 ~ "MP",
#                     group2 == 9 ~ "FG",
#                     group2 == 10 ~ "FGA",
#                     group2 == 11 ~ "FG_pct",
#                     group2 == 12 ~ "3P",
#                     group2 == 13 ~ "3PA",
#                     group2 == 14 ~ "3P_pct",
#                     group2 == 15 ~ "2P",
#                     group2 == 16 ~ "2PA",
#                     group2 == 17 ~ "2P_pct",
#                     group2 == 18 ~ "FT",
#                     group2 == 19 ~ "FTA",
#                     group2 == 20 ~ "FT_pct",
#                     group2 == 21 ~ "ORB",
#                     group2 == 22 ~ "TRB",
#                     group2 == 23 ~ "AST",
#                     group2 == 24 ~ "STL",
#                     group2 == 25 ~ "BLK",
#                     group2 == 26 ~ "TOV",
#                     group2 == 27 ~ "PF",
#                     group2 == 28 ~ "PTS"
#                 )
#             ) %>%
#             tail(-28) %>%
#             filter(group2 != 4, group2 != 5) %>%
#             select(text, key) %>%
#             pivot_wider(names_from = key, values_from = text) %>%
#             mutate(year = str_extract(string = x, pattern = "\\d\\d\\d\\d"),
#                    per_link = x) -> per_game
#         
#         x_new <- str_replace(string = x, pattern = "per_game", replacement = "advanced")
#         html_object_adv <- read_html (x_new)
#         ranking_elements_adv <- html_nodes(html_object_adv,
#                                            css = ".full_table .left , .full_table .center, .full_table .right, #advanced .poptip")
#         ranking_text_adv <- html_text(ranking_elements_adv)
#         wnba_adv <- tibble(text = ranking_text_adv)
#         wnba_adv %>%
#             mutate(rownum = row_number(),
#                    group1 = cumsum(key = rownum %% 26 == 1)) %>%
#             group_by(group1) %>%
#             mutate(group2 = row_number()) %>%
#             mutate(key = case_when(
#                 group2 == 1 ~ "Player",
#                 group2 == 2 ~ "Team",
#                 group2 == 3 ~ "Pos",
#                 group2 == 4 ~ "G",
#                 group2 == 5 ~ "MP",
#                 group2 == 6 ~ "G",
#                 group2 == 7 ~ "MP",
#                 group2 == 8 ~ "PER",
#                 group2 == 9 ~ "TS_pct",
#                 group2 == 10 ~ "eFG_pct",
#                 group2 == 11 ~ "3PAr",
#                 group2 == 12 ~ "FTr",
#                 group2 == 13 ~ "ORB_pct",
#                 group2 == 14 ~ "TRB_pct",
#                 group2 == 15 ~ "AST_pct",
#                 group2 == 16 ~ "STL_pct",
#                 group2 == 17 ~ "BLK_pct",
#                 group2 == 18 ~ "TOV_pct",
#                 group2 == 19 ~ "USG_pct",
#                 group2 == 20 ~ "ORtg",
#                 group2 == 21 ~ "DRtg",
#                 group2 == 22 ~ "Blank",
#                 group2 == 23 ~ "OWS",
#                 group2 == 24 ~ "DWS",
#                 group2 == 25 ~ "WS",
#                 group2 == 26 ~ "WS_40")) %>%
#             filter(group2 != 2, group2 != 3, group2 != 4, group2 != 5, group2 != 6, group2 != 7, group2 != 22) %>%
#             tail(-19) %>%
#             select(text, key) %>%
#             pivot_wider(names_from = key, values_from = text) %>%
#             mutate(adv_link = x_new) -> advanced
#         advanced %>%
#             select(-group1) ->
#             advanced
#         left_join(x = per_game, y = advanced, by = "Player") ->
#             total_data
#         total_data %>%
#             select(-1, -30) ->
#             total_data
#         
#         x_total = str_replace(string = x, pattern = "per_game", replacement = "totals")
#         
#         #INSERT HERE
#         
#         
#         if (str_detect(string = x, pattern = "(2018)|(2019)|(2020)|(2021)")) {
#             x_pbp <- str_replace(string = x, pattern = "per_game", replacement = "play_by_play")
#             
#             html_object_pbp <- read_html (x_pbp)
#             ranking_elements_pbp <- html_nodes(html_object_pbp,
#                                                css = ".full_table .right , .full_table .center, #pbp .poptip, #pbp .poptip, .full_table .left")
#             ranking_text_pbp <- html_text(ranking_elements_pbp)
#             wnba_pbp <- tibble(text = ranking_text_pbp)
#             wnba_pbp
#             
#             wnba_pbp %>%
#                 mutate(rownum = row_number(),
#                        group1 = cumsum(key = rownum %% 23 == 1)) %>%
#                 group_by(group1) %>%
#                 mutate(group2 = row_number()) %>%
#                 mutate(key = case_when(
#                     group2 == 1 ~ "Player",
#                     group2 == 2 ~ "Team",
#                     group2 == 3 ~ "Pos",
#                     group2 == 4 ~ "G",
#                     group2 == 5 ~ "MP",
#                     group2 == 6 ~ "G",
#                     group2 == 7 ~ "MP",
#                     group2 == 8 ~ "Blank1",
#                     group2 == 9 ~ "Plus_Minus",
#                     group2 == 10 ~ "On_Off",
#                     group2 == 11 ~ "Blank2",
#                     group2 == 12 ~ "Bad_Pass_TOV",
#                     group2 == 13 ~ "Lost_Ball_TOV",
#                     group2 == 14 ~ "Blank3",
#                     group2 == 15 ~ "Shooting_Fouls",
#                     group2 == 16 ~ "Offensive_Fouls",
#                     group2 == 17 ~ "Blank4",
#                     group2 == 18 ~ "Shooting_Fouls_Drawn",
#                     group2 == 19 ~ "Blank5",
#                     group2 == 20 ~ "Blank6",
#                     group2 == 21 ~ "Points_Generated",
#                     group2 == 22 ~ "AND1s",
#                     group2 == 23 ~ "Shots_Blocked")) %>%
#                 filter(group2 != 2, group2 != 3, group2 != 4, group2 != 5, group2 != 6, group2 != 7, group2 != 8, group2 != 11, group2 != 14, group2 != 17, group2 != 19, group2 != 20) %>%
#                 tail(-11) %>%
#                 select(text, key) %>%
#                 pivot_wider(names_from = key, values_from = text) ->
#                 pbp
#             
#             left_join(x = total_data, y = pbp, by = "Player") ->
#                 basic_adv_pbp
#             basic_adv_pbp %>%
#                 select(-48) ->
#                 basic_adv_pbp
#             
#             x_shooting <- str_replace(string = x, pattern = "per_game", replacement = "shooting")
#             
#             html_object_s <- read_html (x_shooting)
#             ranking_elements_s <- html_nodes(html_object_s, ".full_table .center , .full_table .right, .full_table .left, #shooting .poptip")
#             ranking_text_s <- html_text(ranking_elements_s)
#             wnba_s <- tibble(text = ranking_text_s)
#             
#             wnba_s %>%
#                 mutate(rownum = row_number(),
#                        group1 = cumsum(key = rownum %% 29 == 1)) %>%
#                 group_by(group1) %>%
#                 mutate(group2 = row_number()) %>%
#                 mutate(
#                     key = case_when(
#                         group2 == 1 ~ "Player",
#                         group2 == 2 ~ "Team",
#                         group2 == 3 ~ "Pos",
#                         group2 == 4 ~ "G",
#                         group2 == 5 ~ "MP",
#                         group2 == 6 ~ "G",
#                         group2 == 7 ~ "MP",
#                         group2 == 8 ~ "FG_pct",
#                         group2 == 9 ~ "Avg_Distance_FGA",
#                         group2 == 10 ~ "blank1",
#                         group2 == 11 ~ "Proportion_FGA_2P",
#                         group2 == 12 ~ "Proportion_FGA_0-3",
#                         group2 == 13 ~ "Proportion_FGA_3-10",
#                         group2 == 14 ~ "Proportion_FGA_10-16",
#                         group2 == 15 ~ "Proportion_FGA_16-3P",
#                         group2 == 16 ~ "Proportion_FGA_3P",
#                         group2 == 17 ~ "blank2",
#                         group2 == 18 ~ "FG_pct_2P",
#                         group2 == 19 ~ "FG_pct_0-3",
#                         group2 == 20 ~ "FG_pct_3-10",
#                         group2 == 21 ~ "FG_pct_10-16",
#                         group2 == 22 ~ "FG_pct_16-3P",
#                         group2 == 23 ~ "FG_pct_3P",
#                         group2 == 24 ~ "blank3",
#                         group2 == 25 ~ "Prop_2P_Assisted",
#                         group2 == 26 ~ "Prop_3P_Assisted",
#                         group2 == 27 ~ "blank6",
#                         group2 == 28 ~ "blank7",
#                         group2 == 29 ~ "blank8"
#                     )) %>%
#                 tail(-29) %>%
#                 filter(group2 != 2, group2 != 3, group2 != 4, group2 != 5, group2 != 6, group2 != 7, group2 != 8, group2 != 10, group2 != 17,group2 != 24,group2 != 27,group2 != 28,group2 != 29) %>%
#                 select(text, key) %>%
#                 pivot_wider(names_from = key, values_from = text) %>%
#                 ungroup() ->
#                 wnba_shooting
#             
#             left_join(x = basic_adv_pbp, y = wnba_shooting, by = "Player") ->
#                 basic_adv_pbp_shooting
#             basic_adv_pbp_shooting
#             
#         }
#         else {
#             total_data
#         }
#     }
# }
# 
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2021_per_game.html") -> wnba2021
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2020_per_game.html") -> wnba2020
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2019_per_game.html") -> wnba2019
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2018_per_game.html") -> wnba2018
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2017_per_game.html") -> wnba2017
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2016_per_game.html") -> wnba2016
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2015_per_game.html") -> wnba2015
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2014_per_game.html") -> wnba2014
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2013_per_game.html") -> wnba2013
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2012_per_game.html") -> wnba2012
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2011_per_game.html") -> wnba2011
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2010_per_game.html") -> wnba2010
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2009_per_game.html") -> wnba2009
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2008_per_game.html") -> wnba2008
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2007_per_game.html") -> wnba2007
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2006_per_game.html") -> wnba2006
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2005_per_game.html") -> wnba2005
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2004_per_game.html") -> wnba2004
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2003_per_game.html") -> wnba2003
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2002_per_game.html") -> wnba2002
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2001_per_game.html") -> wnba2001
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/2000_per_game.html") -> wnba2000
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/1999_per_game.html") -> wnba1999
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/1998_per_game.html") -> wnba1998
# scrape_year(x = "https://www.basketball-reference.com/wnba/years/1997_per_game.html") -> wnba1997
# 
# full_join(x = wnba2021, y = wnba2020) -> w1
# full_join(x = w1, y = wnba2019) -> w2
# full_join(x = w2, y = wnba2018) -> w3
# full_join(x = w3, y = wnba2017) -> w4
# full_join(x = w4, y = wnba2016) -> w5
# full_join(x = w5, y = wnba2015) -> w6
# full_join(x = w6, y = wnba2014) -> w7
# full_join(x = w7, y = wnba2013) -> w8
# full_join(x = w8, y = wnba2012) -> w9
# full_join(x = w9, y = wnba2011) -> w10
# full_join(x = w10, y = wnba2010) -> w11
# full_join(x = w11, y = wnba2009) -> w12
# full_join(x = w12, y = wnba2008) -> w13
# full_join(x = w13, y = wnba2007) -> w14
# full_join(x = w14, y = wnba2006) -> w15
# full_join(x = w15, y = wnba2005) -> w16
# full_join(x = w16, y = wnba2004) -> w17
# full_join(x = w17, y = wnba2003) -> w18
# full_join(x = w18, y = wnba2002) -> w19
# full_join(x = w19, y = wnba2001) -> w20
# full_join(x = w20, y = wnba2000) -> w21
# full_join(x = w21, y = wnba1999) -> w22
# full_join(x = w22, y = wnba1998) -> w23
# full_join(x = w23, y = wnba1997) -> w24
# w24
# 
# scrape_drafts <- function(x) {
#     html_object_d <- read_html (x)
#     ranking_elements_d <- html_nodes(html_object_d,
#                                      css = ".left , tbody .center:nth-child(1)")
#     ranking_text_d <- html_text(ranking_elements_d)
#     wnba_d <- tibble(text = ranking_text_d)
#     wnba_d
#     
#     wnba_d %>%
#         mutate(rownum = row_number()) %>%
#         filter(!str_detect(string = text, pattern = "Round"),
#                !str_detect(string = text, pattern = "Pk")) %>%
#         mutate(rownum = row_number()) %>%
#         mutate(group1 = cumsum(key = rownum %% 5 == 1)) %>%
#         group_by(group1) %>%
#         mutate(group2 = row_number()) %>%
#         mutate(key = case_when(
#             group2 == 1 ~ "Pick",
#             group2 == 2 ~ "Drafted_By",
#             group2 == 3 ~ "Player",
#             group2 == 4 ~ "Country",
#             group2 == 5 ~ "College")) %>%
#         select(text, key) %>%
#         head(-1) %>%
#         pivot_wider(names_from = key, values_from = text) %>%
#         ungroup() %>%
#         mutate(College = case_when(str_detect(string = Country, pattern = ".") == TRUE ~ "International",
#                                    is.na(College) == FALSE ~ College)) %>%
#         mutate(Country = case_when(str_detect(string = Country, pattern = ".") == TRUE ~ Country,
#                                    is.na(College) == FALSE ~ "USA")) %>%
#         select(-group1) %>%
#         mutate(year_drafted = str_extract(string = x, pattern = "\\d\\d\\d\\d"),
#                draft_link = x)->
#         draft_test
#     draft_test
# }
# 
# 
# 
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2021.html") -> wnbadraft2021
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2020.html") -> wnbadraft2020
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2019.html") -> wnbadraft2019
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2018.html") -> wnbadraft2018
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2017.html") -> wnbadraft2017
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2016.html") -> wnbadraft2016
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2015.html") -> wnbadraft2015
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2014.html") -> wnbadraft2014
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2013.html") -> wnbadraft2013
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2012.html") -> wnbadraft2012
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2011.html") -> wnbadraft2011
# wnbadraft2011 %>%
#     filter(Player != "Angel Robinson") ->
#     wnbadraft2011
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2010.html") -> wnbadraft2010
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2009.html") -> wnbadraft2009
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2008.html") -> wnbadraft2008
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2007.html") -> wnbadraft2007
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2006.html") -> wnbadraft2006
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2005.html") -> wnbadraft2005
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2004.html") -> wnbadraft2004
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2003.html") -> wnbadraft2003
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2002.html") -> wnbadraft2002
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2001.html") -> wnbadraft2001
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/2000.html") -> wnbadraft2000
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/1999.html") -> wnbadraft1999
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/1998.html") -> wnbadraft1998
# scrape_drafts(x = "https://www.basketball-reference.com/wnba/draft/1997.html") -> wnbadraft1997
# 
# 
# full_join(x = wnbadraft2021, y = wnbadraft2020) -> d1
# full_join(x = d1, y = wnbadraft2019) -> d2
# full_join(x = d2, y = wnbadraft2018) -> d3
# full_join(x = d3, y = wnbadraft2017) -> d4
# full_join(x = d4, y = wnbadraft2016) -> d5
# full_join(x = d5, y = wnbadraft2015) -> d6
# full_join(x = d6, y = wnbadraft2014) -> d7
# full_join(x = d7, y = wnbadraft2013) -> d8
# full_join(x = d8, y = wnbadraft2012) -> d9
# full_join(x = d9, y = wnbadraft2011) -> d10
# full_join(x = d10, y = wnbadraft2010) -> d11
# full_join(x = d11, y = wnbadraft2009) -> d12
# full_join(x = d12, y = wnbadraft2008) -> d13
# full_join(x = d13, y = wnbadraft2007) -> d14
# full_join(x = d14, y = wnbadraft2006) -> d15
# full_join(x = d15, y = wnbadraft2005) -> d16
# full_join(x = d16, y = wnbadraft2004) -> d17
# full_join(x = d17, y = wnbadraft2003) -> d18
# full_join(x = d18, y = wnbadraft2002) -> d19
# full_join(x = d19, y = wnbadraft2001) -> d20
# full_join(x = d20, y = wnbadraft2000) -> d21
# full_join(x = d21, y = wnbadraft1999) -> d22
# full_join(x = d22, y = wnbadraft1998) -> d23
# full_join(x = d23, y = wnbadraft1997) -> d24
# d24
# 
# left_join(x = w24, y = d24, by = "Player") ->
#     wd24
# 
# wd24 %>%
#     mutate(draft_link = replace_na(data = draft_link,replace = "Undrafted"),
#            Drafted_By = replace_na(data = Drafted_By,replace = "Undrafted"),
#            year_drafted = replace_na(data = year_drafted,replace = "Undrafted")) %>%
#     mutate(across((c(4:27, 29:46, 48:57, 59:74,79)),as.numeric)) ->
#     wd24
# 
# wd24 %>%
#     mutate(name_again = Player) %>%
#     separate(col = name_again, sep = " ", into = c("First", "Last")) %>%
#     mutate(Last = str_sub(string = Last, start = 1, end = 5),
#            First = str_sub(string = First, start = 1, end = 2),
#            linkname = str_c(Last, First, "01w"),
#            link1 = per_link) %>%
#     separate(col = link1, sep = "wnba", into = c("Real_Link", "disc")) %>%
#     mutate(disc = "req/202106291/images/wnba-players",
#            image_link = str_c(Real_Link,disc,"/",linkname,".jpg"),
#            image_link = str_to_lower(string = image_link)) %>%
#     mutate(across((c(75:77)),as.factor))->
#     wd24
# 
# scrape_career <- function(x) {
#     html_object_tot <- read_html (x)
#     ranking_elements_tot <- html_nodes(html_object_tot,
#                                        css = ".full_table .left , .full_table .center, .full_table .right, #advanced .poptip")
#     ranking_text_tot <- html_text(ranking_elements_tot)
#     wnba_tot <- tibble(text = ranking_text_tot)
#     
#     wnba_tot %>%
#         mutate(rownum = row_number(),
#                group1 = cumsum(key = rownum %% 28 == 1)) %>%
#         group_by(group1) %>%
#         mutate(group2 = row_number()) %>%
#         mutate(
#             key = case_when(
#                 group2 == 1 ~ "Player",
#                 group2 == 2 ~ "Team",
#                 group2 == 3 ~ "Pos",
#                 group2 == 4 ~ "G",
#                 group2 == 5 ~ "MP",
#                 group2 == 6 ~ "G",
#                 group2 == 7 ~ "GS",
#                 group2 == 8 ~ "MP",
#                 group2 == 9 ~ "FG",
#                 group2 == 10 ~ "FGA",
#                 group2 == 11 ~ "FG_pct",
#                 group2 == 12 ~ "3P",
#                 group2 == 13 ~ "3PA",
#                 group2 == 14 ~ "3P_pct",
#                 group2 == 15 ~ "2P",
#                 group2 == 16 ~ "2PA",
#                 group2 == 17 ~ "2P_pct",
#                 group2 == 18 ~ "FT",
#                 group2 == 19 ~ "FTA",
#                 group2 == 20 ~ "FT_pct",
#                 group2 == 21 ~ "ORB",
#                 group2 == 22 ~ "TRB",
#                 group2 == 23 ~ "AST",
#                 group2 == 24 ~ "STL",
#                 group2 == 25 ~ "BLK",
#                 group2 == 26 ~ "TOV",
#                 group2 == 27 ~ "PF",
#                 group2 == 28 ~ "PTS"
#             )
#         ) %>%
#         filter(group2 != 4, group2 != 5) %>%
#         select(text, key) %>%
#         pivot_wider(names_from = key, values_from = text) %>%
#         ungroup() %>%
#         mutate(year = str_extract(string = x, pattern = "\\d\\d\\d\\d")) %>%
#         select(-group1) %>%
#         mutate(link_i = x) %>%
#         mutate(name_again = Player) %>%
#         separate(col = name_again, sep = " ", into = c("First", "Last")) %>%
#         mutate(Last = str_sub(string = Last, start = 1, end = 5),
#                First = str_sub(string = First, start = 1, end = 2),
#                First = str_remove(string = First, pattern = "\\'"),
#                Last = str_remove(string = Last, pattern = "\\'"), 
#                linkname = str_c(Last, First, "01w"),
#                link_i1 = link_i) %>%
#         separate(col = link_i1, sep = "wnba", into = c("Real_Link", "disc")) %>%
#         mutate(disc = "req/202106291/images/wnba-players",
#                image_link = str_c(Real_Link,disc,"/",linkname,".jpg"),
#                image_link = str_to_lower(string = image_link)) %>%
#         select(-Last,-First,-linkname,-Real_Link,-disc)->
#         total_stats
#     total_stats
# }
# 
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2021_totals.html") -> wnba2021_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2020_totals.html") -> wnba2020_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2019_totals.html") -> wnba2019_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2018_totals.html") -> wnba2018_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2017_totals.html") -> wnba2017_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2016_totals.html") -> wnba2016_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2015_totals.html") -> wnba2015_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2014_totals.html") -> wnba2014_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2013_totals.html") -> wnba2013_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2012_totals.html") -> wnba2012_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2011_totals.html") -> wnba2011_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2010_totals.html") -> wnba2010_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2009_totals.html") -> wnba2009_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2008_totals.html") -> wnba2008_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2007_totals.html") -> wnba2007_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2006_totals.html") -> wnba2006_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2005_totals.html") -> wnba2005_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2004_totals.html") -> wnba2004_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2003_totals.html") -> wnba2003_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2002_totals.html") -> wnba2002_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2001_totals.html") -> wnba2001_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/2000_totals.html") -> wnba2000_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/1999_totals.html") -> wnba1999_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/1998_totals.html") -> wnba1998_totals
# scrape_career(x = "https://www.basketball-reference.com/wnba/years/1997_totals.html") -> wnba1997_totals
# 
# full_join(x = wnba2021_totals, y = wnba2020_totals) -> wt1
# full_join(x = wt1, y = wnba2019_totals) -> wt2
# full_join(x = wt2, y = wnba2018_totals) -> wt3
# full_join(x = wt3, y = wnba2017_totals) -> wt4
# full_join(x = wt4, y = wnba2016_totals) -> wt5
# full_join(x = wt5, y = wnba2015_totals) -> wt6
# full_join(x = wt6, y = wnba2014_totals) -> wt7
# full_join(x = wt7, y = wnba2013_totals) -> wt8
# full_join(x = wt8, y = wnba2012_totals) -> wt9
# full_join(x = wt9, y = wnba2011_totals) -> wt10
# full_join(x = wt10, y = wnba2010_totals) -> wt11
# full_join(x = wt11, y = wnba2009_totals) -> wt12
# full_join(x = wt12, y = wnba2008_totals) -> wt13
# full_join(x = wt13, y = wnba2007_totals) -> wt14
# full_join(x = wt14, y = wnba2006_totals) -> wt15
# full_join(x = wt15, y = wnba2005_totals) -> wt16
# full_join(x = wt16, y = wnba2004_totals) -> wt17
# full_join(x = wt17, y = wnba2003_totals) -> wt18
# full_join(x = wt18, y = wnba2002_totals) -> wt19
# full_join(x = wt19, y = wnba2001_totals) -> wt20
# full_join(x = wt20, y = wnba2000_totals) -> wt21
# full_join(x = wt21, y = wnba1999_totals) -> wt22
# full_join(x = wt22, y = wnba1998_totals) -> wt23
# full_join(x = wt23, y = wnba1997_totals) -> wt24
# wt24
# wt24
# 
# wt24 %>%
#     mutate(across((c(4:26)),as.numeric)) %>%
#     group_by(Player) %>%
#     arrange(Player) %>%
#     mutate(year = as.integer(year)) %>%
#     bind_rows(summarise(.,
#                         across(c(3:7,9,10,12,13,15,16,18:25),sum),
#                         across(where(is.character), ~ "Career"))) %>%
#     distinct() %>%
#     arrange(Player) %>%
#     mutate(year = as.character(year)) %>%
#     mutate(year = case_when(is.na(year) == FALSE ~ year,
#                             is.na(year) == TRUE ~ "Career")) %>%
#     filter(year == "Career") %>%
#     mutate(FG_pct = round(FG/FGA,3),
#            `3P_pct` = round(`3P`/`3PA`,3),
#            `2P_pct` = round(`2P`/`2PA`,3),
#            FT_pct = round(FT/FTA,3)) %>%
#     mutate(across((c(5:7,9,10,12,13,15,16,18:25)), ~round(.x/G,1))) %>%
#     select(-Team, -Pos, -year)->
#     wnba_careers
# 
# wt24 %>%
#     mutate(across((c(4:26)),as.numeric)) %>%
#     group_by(Player) %>%
#     arrange(Player) %>%
#     mutate(year = as.integer(year)) %>%
#     bind_rows(summarise(.,
#                         across(c(3:7,9,10,12,13,15,16,18:25),sum),
#                         across(where(is.character), ~ "Career"))) -> wt24_yearly 
# 
# wt24_yearly %>%
#     distinct() %>%
#     arrange(Player) %>%
#     mutate(year = as.character(year)) %>%
#     mutate(year = case_when(is.na(year) == FALSE ~ year,
#                             is.na(year) == TRUE ~ "Career")) %>%
#     filter(year == "Career") %>%
#     mutate(FG_pct = round(FG/FGA,3),
#            `3P_pct` = round(`3P`/`3PA`,3),
#            `2P_pct` = round(`2P`/`2PA`,3),
#            FT_pct = round(FT/FTA,3)) %>%
#     mutate(across((c(5:7,9,10,12,13,15,16,18:25)), ~round(.x/G,1))) ->
#     wnba_careers
# 
# semi_join(x = wt24_yearly, y = wnba_careers, by = "Player") %>%
#     arrange(Player) %>%
#     group_by(Player, year) %>%
#     mutate(year = as.character(year)) %>%
#     mutate(FG_pct = case_when(Team != "Career" ~ FG_pct,
#                               Team == "Career" ~ round(FG/FGA,3)),
#            `3P_pct` = case_when(Team != "Career" ~ `3P_pct`,
#                                 Team == "Career" ~ round(`3P`/`3PA`,3)),
#            `2P_pct` = case_when(Team != "Career" ~ `2P_pct`,
#                                 Team == "Career" ~ round(`2P`/`2PA`,3)),
#            FT_pct = case_when(Team != "Career" ~ FT_pct,
#                               Team == "Career" ~ round(FT/FTA,3)),
#            year = case_when(Team != "Career" ~ year,
#                             Team == "Career" ~ "Career")) %>%
#     mutate(across((c(5:7,9,10,12,13,15,16,18:25)), ~round(.x/G,1))) %>%
#     arrange(year)->
#     wnba_careers
# 
# wnba_careers %>%
#     ungroup() ->
#     wnba_careers
# 
# wnba_careers 
# 
# wd24 %>%
#     select(Player, College, Country, Pick, Drafted_By,year_drafted) ->
#     wd24_test
# 
# wd24
# 
# left_join(x = wnba_careers, y = wd24_test, by = "Player") %>%
#     distinct() %>%
#     arrange(Player) ->
#     wnba_careers
# 
# html_object_col <- read_html ("https://kenpom.com/")
# ranking_elements_col <- html_nodes(html_object_col,
#                                    css = "td a")
# ranking_text_col <- html_text(ranking_elements_col)
# wnba_col <- tibble(text = ranking_text_col)
# wnba_col %>%
#     mutate(rownum = row_number(),
#            group1 = cumsum(key = rownum %% 2 == 1)) %>%
#     group_by(group1) %>%
#     mutate(group2 = row_number()) %>%
#     mutate(
#         key = case_when(
#             group2 == 1 ~ "College",
#             group2 == 2 ~ "Conference")) %>%
#     select(text, key) %>%
#     pivot_wider(names_from = key, values_from = text) %>%
#     ungroup() %>%
#     mutate(College = str_replace(string = College, pattern = "Connecticut", "UConn")) %>%
#     mutate(College = str_replace(string = College, pattern = "(^North Carolina$)", "UNC")) %>%
#     mutate(College = str_replace(string = College, pattern = "(^Mississippi$)", "Ole Miss")) %>%
#     mutate(College = str_replace(string = College, pattern = "Charlotte", "UNC Charlotte")) %>%
#     select(-group1)->
#     college_conferences
# 
# wnba_careers %>%
#     #select(Player, College) %>%
#     mutate(College = str_replace(string = College, pattern = " University", "")) %>%
#     mutate(College = str_replace(string = College, pattern = "State", "St.")) %>%
#     mutate(College = str_replace(string = College, pattern = "University of ", "")) %>%
#     mutate(College = str_replace(string = College, pattern = "Florida International", "FIU")) %>%
#     mutate(College = str_replace(string = College, pattern = "Cal St. Long Beach", "Long Beach St.")) %>%
#     mutate(College = str_replace(string = College, pattern = "Iona College", "Iona")) %>%
#     mutate(College = str_replace(string = College, pattern = "Miami \\(FL\\)", "Miami FL")) %>%
#     mutate(College = str_replace(string = College, pattern = "NC St.", "N.C. State")) %>%
#     mutate(College = str_replace(string = College, pattern = "Pitt", "Pittsburgh")) %>%
#     mutate(College = str_replace(string = College, pattern = "Texas-El Paso", "UTEP")) %>%
#     mutate(College = str_replace(string = College, pattern = "Tennessee at Martin", "Tennessee Martin")) %>%
#     mutate(College = case_when(Player != "Jackie Stiles" ~ College,
#                                Player == "Jackie Stiles" ~ "Missouri St.")) %>%
#     arrange(College) ->
#     wnba_careers_test1
# 
# wnba_careers_test1
# 
# college_conferences %>%
#     arrange(College) %>%
#     add_row(College = "Master's College", Conference = "GSAC (NAIA)") %>%
#     add_row(College = "Southwest Tennessee Community College", Conference = "Community") ->
#     college_conferences
# college_conferences 
# 
# left_join(wnba_careers_test1, college_conferences, by = "College") %>%
#     arrange(Player) ->
#     wnba_careers
# 
# wnba_careers
# 
# wd24 %>%
#     mutate(Player = str_remove(string = Player, pattern = "\\*")) ->
#     wd24
# 
# scrape_awards <- function(x) {
#     html_object_aw <- read_html (x)
#     ranking_elements_aw <- html_nodes(html_object_aw,
#                                       css = "td:nth-child(2) , tbody th")
#     ranking_text_aw <- html_text(ranking_elements_aw)
#     wnba_aw <- tibble(text = ranking_text_aw)
#     wnba_aw
#     
#     wnba_aw %>%
#         mutate(rownum = row_number()) %>%
#         mutate(group1 = cumsum(key = rownum %% 2 == 1)) %>%
#         group_by(group1) %>%
#         mutate(group2 = row_number()) %>%
#         mutate(key = case_when(
#             group2 == 1 ~ "year",
#             group2 == 2 ~ "Player")) %>%
#         select(text, key) %>%
#         tail(-2) %>%
#         pivot_wider(names_from = key, values_from = text) %>%
#         ungroup() %>%
#         select(-group1) %>%
#         mutate(Player = str_remove(string = Player, pattern = "\\s\\(.\\)")) %>%
#         mutate(MVP = case_when(str_detect(string = x, pattern = "mvp") == TRUE ~ "Most Valuable Player"),
#                ROY = case_when(str_detect(string = x, pattern = "roy") == TRUE ~ "Rookie Of the Year"),
#                DPOY = case_when(str_detect(string = x, pattern = "dpoy") == TRUE ~ "Defensive Player of the Year"),
#                SWOY = case_when(str_detect(string = x, pattern = "swoy") == TRUE ~ "Sixth Woman of the Year"),
#                MIP = case_when(str_detect(string = x, pattern = "mip") == TRUE ~ "Most Improved Player"),
#                FMVP = case_when(str_detect(string = x, pattern = "fin_mvp") == TRUE ~ "Finals MVP"))
#     
# }
# 
# scrape_awards("https://www.basketball-reference.com/wnba/awards/mvp.html") -> wnbamvps
# scrape_awards("https://www.basketball-reference.com/wnba/awards/roy.html") -> wnbaroys
# scrape_awards("https://www.basketball-reference.com/wnba/awards/dpoy.html") -> wnbadpoys
# scrape_awards("https://www.basketball-reference.com/wnba/awards/swoy.html") -> wnbaswoys
# scrape_awards("https://www.basketball-reference.com/wnba/awards/mip.html") -> wnbamips
# scrape_awards("https://www.basketball-reference.com/wnba/awards/fin_mvp.html") -> wnbafinmvps
# 
# full_join(x = wnbamvps, y = wnbaroys) -> wa1
# full_join(x = wa1, y = wnbadpoys) -> wa2
# full_join(x = wa2, y = wnbaswoys) -> wa3
# full_join(x = wa3, y = wnbamips) -> wa4
# full_join(x = wa4, y = wnbafinmvps) -> wa5
# wa5 %>%
#     arrange(Player, year) %>%
#     group_by(year, Player) %>%
#     pivot_longer(cols = c(MVP, ROY, DPOY, SWOY, MIP, FMVP), values_to = "Award") %>%
#     select(-name) %>%
#     distinct() %>%
#     filter(is.na(Award) == FALSE) %>%
#     pivot_wider(names_from = Award, values_from = Award) %>%
#     arrange(Player) ->
#     wnba_ind_awards
# 
# scrape_award_teams <- function(x) {
#     html_object_awt <- read_html (x)
#     ranking_elements_awt <- html_nodes(html_object_awt,
#                                        css = ".left:nth-child(2) , .left:nth-child(3), .right:nth-child(1)")
#     ranking_text_awt <- html_text(ranking_elements_awt)
#     wnba_awt <- tibble(text = ranking_text_awt)
#     wnba_awt
#     
#     wnba_awt %>%
#         mutate(rownum = row_number()) %>%
#         mutate(group1 = cumsum(key = rownum %% 3 == 1)) %>%
#         group_by(group1) %>%
#         mutate(group2 = row_number()) %>%
#         mutate(key = case_when(
#             group2 == 1 ~ "year",
#             group2 == 2 ~ "Player",
#             group2 == 3 ~ "All_WNBA_Team")) %>%
#         select(text, key) %>%
#         pivot_wider(names_from = key, values_from = text) %>%
#         filter(year != "Year") %>%
#         ungroup() %>%
#         select(-group1) %>%
#         mutate(Player = str_remove(string = Player, pattern = "\\s\\(.\\)")) -> all_award_teams
#     if (str_detect(string = x, pattern = "all_wnba")) {
#         all_award_teams %>%
#             mutate(All_WNBA_Team = case_when(str_detect(string = All_WNBA_Team, pattern = "1st") == TRUE ~ "All-WNBA First Team",
#                                              str_detect(string = All_WNBA_Team, pattern = "2nd") == TRUE ~ "All-WNBA Second Team")) %>%
#             pivot_wider(names_from = All_WNBA_Team, values_from = All_WNBA_Team) %>%
#             select(-`NA`)
#     }
#     else if (str_detect(string = x, pattern = "all_rookie")) {
#         all_award_teams %>%
#             mutate(All_WNBA_Team = case_when(str_detect(string = All_WNBA_Team, pattern = "1st") == TRUE ~ "All-Rookie Team")) %>%
#             pivot_wider(names_from = All_WNBA_Team, values_from = All_WNBA_Team) %>%
#             select(-`NA`)
#     }
#     else if (str_detect(string = x, pattern = "all_defense")) {
#         all_award_teams %>%
#             mutate(All_WNBA_Team = case_when(str_detect(string = All_WNBA_Team, pattern = "1st") == TRUE ~ "All-Defensive First Team",
#                                              str_detect(string = All_WNBA_Team, pattern = "2nd") == TRUE ~ "All-Defensive Second Team")) %>%
#             pivot_wider(names_from = All_WNBA_Team, values_from = All_WNBA_Team) %>%
#             select(-`NA`)
#     }
#     
# }
# 
# scrape_award_teams("https://www.basketball-reference.com/wnba/awards/all_wnba.html") -> allwnbatm
# scrape_award_teams("https://www.basketball-reference.com/wnba/awards/all_rookie.html") -> allrookietm
# scrape_award_teams("https://www.basketball-reference.com/wnba/awards/all_defense.html") -> alldeftm
# 
# full_join(x = allwnbatm, y = allrookietm) -> wat1
# full_join(x = wat1, y = alldeftm) -> wat2
# wat2
# 
# wat2 %>%
#     arrange(Player, year) %>%
#     group_by(year, Player) %>%
#     pivot_longer(cols = c(`All-WNBA First Team`, `All-WNBA Second Team`,`All-Rookie Team`, `All-Defensive First Team`, `All-Defensive Second Team`), values_to = "Award") %>%
#     select(-name) %>%
#     distinct() %>%
#     filter(is.na(Award) == FALSE) %>%
#     pivot_wider(names_from = Award, values_from = Award) %>%
#     arrange(Player) ->
#     wnba_team_awards
# 
# full_join(x = wnba_team_awards, y = wnba_ind_awards) %>%
#     arrange(Player) ->
#     wnba_awards_full
# 
# wnba_awards_full %>%
#     mutate(player_year_id = str_c(Player, year)) ->
#     wnba_awards_full
# wd24 %>%
#     mutate(player_year_id = str_c(Player, year)) ->
#     wd24
# 
# full_join(wd24, wnba_awards_full, by = "player_year_id") %>%
#     select(-year.y, -Player.y) %>%
#     rename(Player = Player.x) %>%
#     rename(year = year.x) %>%
#     arrange(Player) ->
#     wd24
# 
# 
# wd24 %>%
#     select(-per_link, -adv_link,-draft_link,-First,-Last,-linkname,-Real_Link,-disc) %>%
#     select(year,everything(.)) %>%
#     rename(Year = year) ->
#     wd24
# 
# wnba_careers %>%
#     select(-link_i) %>%
#     select(year, everything(.)) %>%
#     relocate(Conference, .after = College) %>%
#     rename(Year = year) ->
#     wnba_careers
# 
# wd24 %>%
#     mutate(across((c(79:89)),as.factor)) ->
#     wd24
# 
# wd24 %>%
#     mutate(Player = str_remove(string = Player, pattern = "\\'")) ->
#     wd24
# 
# left_join(wd24, college_conferences, by = "College") ->
#     wd24
# 
# wd24 %>%
#     mutate(Conference = as.factor(Conference)) ->
#     wd24
# 
# wd24 %>%
#     rename(Position = Pos) %>%
#     rename(Games = G) %>%
#     rename(`Games Started` = GS) %>%
#     rename(`Minutes Played` = MP) %>%
#     rename(`Field Goals Made` = FG) %>%
#     rename(`Field Goals Attempted` = FGA) %>%
#     rename(`Field Goal %` = FG_pct) %>%
#     rename(`3-Pointers Made` = `3P`) %>%
#     rename(`3-Pointers Attempted` = `3PA`) %>%
#     rename(`3-Point %` = `3P_pct`) %>%
#     rename(`2-Pointers Made` = `2P`) %>%
#     rename(`2-Pointers Attempted` = `2PA`) %>%
#     rename(`2-Point %` = `2P_pct`) %>%
#     rename(`Free Throws Made` = FT) %>%
#     rename(`Free Throws Attempted` = FTA) %>%
#     rename(`Free Throw %` = FT_pct) %>%
#     rename(`Offesnive Rebounds` = ORB) %>%
#     rename(`Total Rebounds` = TRB) %>%
#     rename(Assists = AST) %>%
#     rename(Steals = STL) %>%
#     rename(Blocks = BLK) %>%
#     rename(Turnovers = TOV) %>%
#     rename(`Personal Fouls` = PF) %>%
#     rename(Points = PTS) %>%
#     rename(`Player Efficiency Rating` = PER) %>%
#     rename(`True Shooting %` = TS_pct) %>%
#     rename(`Effective Field Goal %` = eFG_pct) %>%
#     rename(`3-Point Attempt Rate` = `3PAr`) %>%
#     rename(`Free Throw Attempt Rate` = FTr) %>%
#     rename(`Offensive Rebounding %` = ORB_pct) %>%
#     rename(`Total Rebounding %` = TRB_pct) %>%
#     rename(`Assist %` = AST_pct) %>%
#     rename(`Steal %` = STL_pct) %>%
#     rename(`Block %` = BLK_pct) %>%
#     rename(`Turnover %` = TOV_pct) %>%
#     rename(`Usage Rate` = USG_pct) %>%
#     rename(`Offensive Rating` = ORtg) %>%
#     rename(`Defensive Rating` = DRtg) %>%
#     rename(`Offensive Win Shares` = OWS) %>%
#     rename(`Defensive Win Shares` = DWS) %>%
#     rename(`Win Shares` = WS) %>%
#     rename(`Plus-Minus` = Plus_Minus) %>%
#     rename(`On-Off` = On_Off) %>%
#     rename(`Bad Pass Turnovers` = Bad_Pass_TOV) %>%
#     rename(`Lost Ball Turnovers` = Lost_Ball_TOV) %>%
#     rename(`Shooting Fouls Committed` = Shooting_Fouls) %>%
#     rename(`Offensive Fouls Committed` = Offensive_Fouls) %>%
#     rename(`Shooting Fouls Drawn` = Shooting_Fouls_Drawn) %>%
#     rename(`Shots Blocked` = Shots_Blocked) %>%
#     rename(`Average Distance of Field Goal Attempts` = Avg_Distance_FGA) %>%
#     rename(`Proportion of Field Goal Attempts from 2-Pointers` = Proportion_FGA_2P) %>%
#     rename(`Proportion of Field Goal Attempts from 0-3 feet` = `Proportion_FGA_0-3`) %>%
#     rename(`Proportion of Field Goal Attempts from 3-10 feet` = `Proportion_FGA_3-10`) %>%
#     rename(`Proportion of Field Goal Attempts from 10-16 feet` = `Proportion_FGA_10-16`) %>%
#     rename(`Proportion of Field Goal Attempts from 16 feet-3-Point Line` = `Proportion_FGA_16-3P`) %>%
#     rename(`Proportion of Field Goal Attempts from 3-Pointers` = Proportion_FGA_3P) %>%
#     rename(`Field Goal % 2-Pointers` = FG_pct_2P) %>%
#     rename(`Field Goal % 0-3 feet` = `FG_pct_0-3`) %>%
#     rename(`Field Goal % 3-10 feet` = `FG_pct_3-10`) %>%
#     rename(`Field Goal % 10-16 feet` = `FG_pct_10-16`) %>%
#     rename(`Field Goal % 16 feet-3-Point Line` = `FG_pct_16-3P`) %>%
#     rename(`Field Goal % 3-Pointers` = FG_pct_3P) %>% 
#     rename(`Proportion of 2-Pointers Assisted` = Prop_2P_Assisted) %>%
#     rename(`Proportion of 3-Pointers Assisted` = Prop_3P_Assisted) %>%
#     rename(`Drafted By` = Drafted_By) %>%
#     rename(`Year Drafted` = year_drafted) ->
#     wd24


wnba_careers
wd24
wnba_league_data

ui <-
    fluidPage(
        useShinyjs(),
        theme = bslib::bs_theme(version = 4, bootswatch = "yeti"), 
        titlePanel("WNBA Shiny App",
                   windowTitle = "WNBA Shiny App"),
        mainPanel(tabsetPanel(
            type = "pills",
            # UI TAB 1 ----
            
            # UI TAB 2 ----
            tabPanel("Player Analysis",
                     sidebarLayout(
                         sidebarPanel(
                             div(
                                 id = "tab2",
                                 varSelectInput("varx", "X Variable",
                                                data = wd24[c(5:76, 79:96)],
                                                selected = "Minutes Played"),
                                 checkboxInput("logx", "Log Transform the X Variable"),
                                 # checkboxGroupInput("xvar_checkboxes", "Stats",
                                 #                    c("Per Game", "Advanced Stats", "Shot Distribution","Draft Data", "Award Data")),
                                 varSelectInput("vary", "Y Variable",
                                                data = wd24[c(5:76, 79:96)],
                                                selected = "Points"),
                                 
                                 checkboxInput("logy", "Log Transform the Y Variable"),
                                 sliderInput(
                                     inputId = "year_tab2",
                                     label = "Seasons",
                                     min = min(wd24$Year),
                                     max = max(wd24$Year),
                                     value = c(2020, 2021),
                                     sep = "",
                                 ),
                                 selectInput(
                                     inputId = "team_tab2",
                                     label = "Team",
                                     choices = c("All", sort(unique(
                                         wd24$Team
                                     ))),
                                     selected = "All"
                                 ),
                                 selectInput(
                                     inputId = "player_tab2",
                                     label = "Player",
                                     choices = c("All", sort(unique(
                                         wd24$Player
                                     ))),
                                     selected = "All"
                                 ),),
                             actionButton("resetAll", "Reset all"),
                             actionButton("setteam", "Compare to League"),
                             actionButton("analysis","Analyze"),
                             verbatimTextOutput("modelSummary"),
                         ),
                         mainPanel(fluidRow(
                             column(
                                 12,
                                 ggiraphOutput("plot2"),
                                 textOutput("regression"),
                                 dataTableOutput("table")
                             )
                         ))
                     )),
            tabPanel("Individual Player Tab",
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(                                       #YEARS INPUT TAB 1
                                 inputId = "year_tab1",
                                 label = "Year",
                                 choices = c("All", sort(unique(
                                     wnba_careers$Year
                                 ))),
                                 selected = "All"
                             ),
                             selectInput(                                       #TEAMS INPUT TAB 1
                                 inputId = "team_tab1",
                                 label = "Team",
                                 choices = c("All", sort(unique(
                                     wnba_careers$Team
                                 ))),
                                 selected = "All"
                             ),
                             selectInput(                                       #PLAYERS INPUT TAB 1
                                 inputId = "player_tab1",
                                 label = "Player",
                                 choices = c("All", sort(unique(
                                     wnba_careers$Player
                                 ))),
                                 selected = "All"
                             ),
                         ),
                         mainPanel(fluidRow(
                             column(
                                 12,
                                 htmlOutput("picture_tab1"),
                                 textOutput("personality"),
                                 dataTableOutput("plot_tab1")))))),
            # UI TAB 3 ----
            tabPanel("League Trend Analysis",
                     sidebarLayout(
                         sidebarPanel(
                             varSelectInput("varcareer", "Variable",
                                            data = wnba_league_data[c(2:40)],
                                            selected = "Points")
                             #,
                     # selectInput(
                     #     inputId = "team_tab2",
                     #     label = "Team",
                     #     choices = c("All", sort(unique(
                     #         wd24$Team
                     #     ))),
                     #     selected = "All"
                     # )
                     ),
                     mainPanel(fluidRow(
                         column(
                             12,
                             ggiraphOutput("plot3"),
                         )
                     )))),

            # UI TAB 4 ----
            tabPanel("Spreadsheet",
                     dataTableOutput("plot4"))
        ))
    )

#  ___________________________________________ ----

server <- function(input, output) {
    
    #  _________________________________________ ----
    
    # OBTAIN ----
    
    # TAB 2 >>> Player ----
    
    wnba_careers_player_filter <- reactive({
        wnba_careers %>%
            filter(Player == input$player_tab1)
    })
    
    # TAB 2 >>> Player image ----
    
    wnba_careers_image_filter <- reactive({
        wnba_careers_player_filter() %>%
            filter(image_link != "Career") %>%
            head(1) %>%
            pull(image_link)
    })
    
    #  _________________________________________ ----
    
    # UPDATE ----
    
    # TAB 2 >>> Team selection with right years ----
    
    observe({
        updateSelectInput(
            inputId = "team_tab1",
            choices = c("All", wnba_careers %>%
                            filter(Year == input$year_tab1) %>%
                            pull(Team))
        )
    })
    
    #  TAB 2 >>> Player selection with right teams ----
    
    observe({
        updateSelectInput(
            inputId = "player_tab1",
            choices = wnba_careers %>%
                filter(Year == input$year_tab1) %>%
                filter(Team == input$team_tab1) %>%
                pull(Player)
        )
    })
    
    # _________________________________________ ----
    
    # DISPLAY ----
    
    # TAB 2 >>> Picture ----
    output$picture_tab1<-renderText({c('<img src="',wnba_careers_image_filter(),'">')})
    #output$personality<-renderText({c('<img src="',wnba_careers_image_filter(),'">')})
    
    # TAB 2 >>> Data Table ----
    output$plot_tab1 <- renderDataTable({
        wnba_careers_player_filter() %>%
            select(-image_link)
    })
    
    #  _________________________________________ ----
    
    # OBTAIN ----
    
    # TAB 1 >>> Class of variables ----
    
    x3 <- reactive({
        wd24[[input$varx]]
    })
    x4 <- reactive({
        wd24[[input$vary]]
    })
    
    # TAB 1 >>> Year ----
    
    wd24_year_filter <- reactive({
        wd24 %>%
            filter(Year >= input$year_tab2[1] &
                       Year <= input$year_tab2[2])
    })
    
    # TAB 1 >>> Team ----
    
    wd24_team_filter <- reactive({
        if (input$team_tab2 == "All") {                                         #IF SELECTING ALL TEAMS, JUST USE THE YEAR FILTER
            wd24_year_filter()
        } else if (input$team_tab2 != "All") {                                  #IF SELECTING ONE TEAM, USE THE YEAR FILTER, THEN FILTER BY TEAM
            wd24_year_filter() %>%
                filter(Team == input$team_tab2)
        }
    })
    
    # TAB 1 >>> Player ----
    
    div(
        id = "tab2",
        wd24_player_filter <- reactive({
            if (input$player_tab2 == "All" &                                    #IF PLAYER AND TEAM ARE "ALL" USE TEAM FILTER
                input$team_tab2 == "All") {                                     #PULL COMES LATER
                wd24_team_filter() 
            } else if (input$team_tab2 == "All" &                               #IF TEAM IS "ALL" AND PLAYER IS NOT, FILTER BY TEAM & PLAYER
                       input$player_tab2 != "All") {
                wd24_team_filter() %>%
                    filter(Player == input$player_tab2)
            }
            else if (input$team_tab2 != "All") {                                #IF PLAYER IS SELECTED, FILTER BY TEAM & PLAYER
                wd24_team_filter() %>%
                    filter(Player == input$player_tab2)
                
            }
        }))
    
    # TAB 1 >>> Name of variables ----
    
    x_variable_name_reactive <- reactive({
        as.character(paste(input$varx))
    })
    
    y_variable_name_reactive <- reactive({
        as.character(paste(input$vary))
    })
    
    # TAB 1 >>> Name of Years ----
    
    year_start_name_reactive <- reactive({
        as.character(paste(input$year_tab2[1]))
    })
    
    year_end_name_reactive <- reactive({
        as.character(paste(input$year_tab2[2]))
    })
    
    # TAB 1 >>> Tooltip and Data ID ----
    
    wd24_tooltip_filter <- reactive({
        if (input$team_tab2 == "All") {                                         #IF ALL TEAMS SELECTED
            wd24_player_filter() %>%
                mutate(tool_read = paste0(wd24_player_filter()$Player, " (",wd24_player_filter()$Year,")", "\n ", x_variable_name_reactive(), ": ", !!input$varx, "\n ", y_variable_name_reactive(), ": ", !!input$vary)) ->
                wd24_reactive2
            tooltip = wd24_reactive2 %>%
                pull(tool_read)
        } else if (input$team_tab2 != "All") {
            wd24_player_filter() %>%
                mutate(tool_read = paste0(wd24_player_filter()$Player, " (",wd24_player_filter()$Year,")", "\n ", x_variable_name_reactive(), ": ", !!input$varx, "\n ", y_variable_name_reactive(), ": ", !!input$vary)) ->
                wd24_player_filter2
            tooltip = wd24_player_filter2 %>%
                pull(tool_read)
        }
    })
    
    wd24_tooltip_filter2 <- reactive({
        if (input$team_tab2 == "All") {                                         #IF ALL TEAMS SELECTED
            wd24_reactive() %>%
                mutate(tool_read = paste0(wd24_reactive()$Player, " (",wd24_reactive()$Year,")", "\n ", x_variable_name_reactive(), ": ", !!input$varx, "\n ", y_variable_name_reactive(), ": ", !!input$vary)) ->
                wd24_reactive2
            tooltip = wd24_reactive2 %>%
                pull(tool_read)
        } else if (input$team_tab2 != "All") {
            wd24_reactive() %>%
                mutate(tool_read = paste0(wd24_reactive()$Player, " (",wd24_reactive()$Year,")", "\n ", x_variable_name_reactive(), ": ", !!input$varx, "\n ", y_variable_name_reactive(), ": ", !!input$vary)) ->
                wd24_player_filter2
            tooltip = wd24_player_filter2 %>%
                pull(tool_read)
        }
    })
    
    wd24_data_id_filter <- reactive({
        if (input$team_tab2 == "All") {
            wd24_player_filter() %>%
                mutate(tool_read = paste0(wd24_player_filter()$Player, " (",wd24_player_filter()$Year,")", "\n ", x_variable_name_reactive(), ": ", !!input$varx, "\n ", y_variable_name_reactive(), ": ", !!input$vary)) ->
                wd24_reactive2
            data_id = wd24_reactive2 %>%
                pull(tool_read)
        } else if (input$team_tab2 != "All") {
            wd24_player_filter() %>%
                mutate(tool_read = paste0(wd24_player_filter()$Player, " (",wd24_player_filter()$Year,")", "\n ", x_variable_name_reactive(), ": ", !!input$varx, "\n ", y_variable_name_reactive(), ": ", !!input$vary)) ->
                wd24_player_filter2
            data_id = wd24_player_filter2 %>%
                pull(tool_read)
        }
    })
    
    wd24_data_id_filter2 <- reactive({
        if (input$team_tab2 == "All") {
            wd24_reactive() %>%
                mutate(tool_read = paste0(wd24_reactive()$Player, " (",wd24_reactive()$Year,")", "\n ", x_variable_name_reactive(), ": ", !!input$varx, "\n ", y_variable_name_reactive(), ": ", !!input$vary)) ->
                wd24_reactive2
            data_id = wd24_reactive2 %>%
                pull(tool_read)
        } else if (input$team_tab2 != "All") {
            wd24_reactive() %>%
                mutate(tool_read = paste0(wd24_reactive()$Player, " (",wd24_reactive()$Year,")", "\n ", x_variable_name_reactive(), ": ", !!input$varx, "\n ", y_variable_name_reactive(), ": ", !!input$vary)) ->
                wd24_player_filter2
            data_id = wd24_player_filter2 %>%
                pull(tool_read)
        }
    })
    
    # TAB 1 >>> Plot title, subtitle, equation ----
    
    wd24_title_filter <- reactive({
            wd24_player_filter() %>%
                mutate(title_read = paste0("\n", x_variable_name_reactive(), " vs. ", y_variable_name_reactive()," (", year_start_name_reactive(), " to ", year_end_name_reactive(), ")")) ->
                wd24_reactive2
            tooltip = wd24_reactive2 %>%
                pull(title_read)
    })
    
    wd24_regression_equation <- reactive({
        #mod <- 
        #lm(formula(input$vary ~ input$varx), data = wd24_player_filter())
        #s <- summary(mod)
        #s$r.squared # how to get the model's r^2
        #plot(mpg ~ wt, data = mtcars)
        #abline(mod)
         
    })
    
    # wd24_subtitle_filter <- reactive({
    #     wd24_player_filter() %>%
    #         mutate(title_read = paste0("\n", x_variable_name_reactive(), " vs. ", y_variable_name_reactive()," (", year_start_name_reactive(), " to ", year_end_name_reactive(), ")")) ->
    #         wd24_reactive2
    #     tooltip = wd24_reactive2 %>%
    #         pull(title_read)
    # })
    
    
    # TAB 1 >>> League ----
    
    wd24_reactive <- reactive({
        wd24_team_filter()
    })
    
    # TAB 1 >>> Average values x & y ----
    
    x_variable_mean <- reactive({
        wd24_reactive() %>%
            summarise(Value = round(quantile(input$varx, c(.50, .75,.90,.95,1)),1)) %>%
            mutate(Quantiles = c("50th Percentile","75th Percentile","90th Percentile","95th Percentile","100th Percentile")) %>%
            select(Quantiles, Value) ->
            wd24_quantiles
        wd24_quantiles
    })
    
    # TAB 3 >>> League ----
    
    wnba_league_reactive <- reactive({
        wnba_league_data
    })
    
    #  _________________________________________ ----
    
    # UPDATE ----
    
    # TAB 1 >>> Team selection with right years ----
    
    observe({
        updateSelectInput(
            inputId = "team_tab2",
            choices = c("All", wd24 %>%
                            filter(Year >= input$year_tab2[1] &
                                       Year <= input$year_tab2[2]) %>%
                            pull(Team))
        )
    })
    
    # TAB 1 >>> Testing out type of stats ----
    
    # observe({
    #     x <- input$xvar_checkboxes
    #     if (input$pergamestats == TRUE) {
    #     updateVarSelectInput(
    #         inputId = "varx",
    #             data = wd24[c(5:26)]
    #         
    #         
    #         # choices = c("All", wd24 %>%
    #         #                 filter(Year >= input$year_tab2[1] &
    #         #                            Year <= input$year_tab2[2]) %>%
    #         #                 pull(Team))
    #     )}
    # })
    
    # TAB 1 >>> Player selection with right teams ----
    
    observe({
        updateSelectInput(
            inputId = "player_tab2",
            choices = wd24 %>%
                filter(Year >= input$year_tab2[1] &
                           Year <= input$year_tab2[2]) %>%
                filter(Team == input$team_tab2) %>%
                pull(Player)
        )
    })
    
    #  _________________________________________ ----
    
    # RESET ----
    
    # TAB 1 >>> Entire sidebar ----
    
    observeEvent(input$resetAll, {
        reset("tab2")
        updateSelectInput(
            inputId = "player_tab2",
            label = "Player",
            choices = c("All", sort(unique(
                wd24$Player
            ))),
            selected = "All"
        )
    })
    
    # TAB 1 >>> One team becomes entire league ----
    observeEvent(input$setteam, {
        updateSelectInput(
            inputId = "team_tab2",
            label = "Team",
            choices = c("All", sort(unique(
                wd24$Team
            )))
        )
        
    })
    
    # _______________________________________ ----
    
    # PLOTS ----
    
    # TAB 3 >> Plot ----
    
    plot_league_data <- reactive({
        ggplot() +
        #     theme(axis.text = element_text(size = 10, face = "italic"),
        #           axis.title = element_text(size = 12, face = "italic"),
        #           plot.title = element_text(size = 20, face = "bold")) +
        geom_line_interactive(aes(x = Year, y = !!input$varcareer),
                              data = wnba_league_reactive()) ->
            plot_league_base
    })
        
    
        # TAB 1 >> Scatter Plot ----
        
    plot_scatter <- reactive({
        # mod <- lm(formula = !!input$vary ~ !!input$varx, data = wd24_player_filter())
        # s <- summary(mod)
        #s$r.squared # how to get the model's r^2
        #plot(mpg ~ wt, data = mtcars)
        #abline(mod)
        ggplot() +
            theme(axis.text = element_text(size = 10, face = "italic"),
                  axis.title = element_text(size = 12, face = "italic"),
                  plot.title = element_text(size = 16, face = "bold")) +
            geom_point_interactive(aes(x = !!input$varx, y = !!input$vary,
                                       tooltip = wd24_tooltip_filter2(), 
                                       data_id = wd24_data_id_filter2(),
                                       color = wd24_reactive()$Position), 
                                   size = 2,
                                   color = "black",
                                   data = wd24_reactive()) +
            geom_smooth_interactive(aes(x = !!input$varx, y = !!input$vary),
                                    #color = "black",
                                    se = FALSE, method = "lm", data = wd24_reactive()) +
            #geom_hline_interactive(aes(yintercept = mean(!!input$vary)),
            #                       linetype = "dashed", data = wd24_reactive()) +
            stat_cor(label.y = 300) +
            stat_regline_equation(label.y = 280) +
            ggtitle(wd24_title_filter(), 
                    subtitle = "Note: All basic statistics are on a per-game basis"
                    ) +
            geom_point_interactive(
                aes(x = !!input$varx, y = !!input$vary,
                    tooltip = wd24_tooltip_filter(), 
                    data_id = wd24_data_id_filter(),
                    color = wd24_player_filter()$Position
                    ),
                #color = "red",
                size = 3,
                data = wd24_player_filter()) +
            # geom_smooth_interactive(aes(x = !!input$varx, y = !!input$vary),
            #                         se = FALSE, method = "lm", 
            #                         #linetype = "dashed", 
            #                         #color = "black",
            #                         data = wd24_player_filter()) +
            labs(color='Position') +
            #geom_hline_interactive(aes(yintercept = mean(!!input$vary)),
            #                       linetype = "dashed", data = wd24_player_filter()) +
            stat_cor(label.y = 300) +
            stat_regline_equation(label.y = 280) -> #+
            #geom_hline_interactive(yintercept = 10) -> 
            plot_scatter_base
        
        if (input$logx == FALSE &
            input$logy == FALSE) {
            plot_scatter_base +
                geom_smooth_interactive(se = FALSE, method = lm) 
        }
        else if (input$logx == TRUE &
                 input$logy == FALSE) {
            plot_scatter_base +
                scale_x_log10()
        }
        else if (input$logx == FALSE &
                 input$logy == TRUE) {
            plot_scatter_base +
                scale_y_log10()
        }
        else if (input$logx == TRUE &
                 input$logy == TRUE) {
            plot_scatter_base +
                scale_x_log10() +
                scale_y_log10()
        }
    })
    # TAB 1 >> Box Plot ----
    plot_box_2 <- reactive({
        ggplot() +
            theme(axis.text = element_text(size = 10, face = "italic"),
                  axis.title = element_text(size = 12, face = "italic"),
                  plot.title = element_text(size = 12, face = "bold")) +
            ggtitle(wd24_title_filter(), subtitle = "Note: All basic statistics are on a per-game basis") +
            geom_boxplot_interactive(aes(x = !!input$varx, y = reorder(!!input$vary,!!input$varx), #color = !!input$vary,
                                         #tooltip = wd24_tooltip_filter(),  
                                         #data_id = wd24_data_id_filter()
                                         ),
                                     data = wd24_reactive()) +
            xlab(x_variable_name_reactive()) +
            ylab(y_variable_name_reactive()) +
            theme(axis.text = element_text(size = 10, face = "italic"),
                  axis.title = element_text(size = 12, face = "italic"),
                  plot.title = element_text(size = 12, face = "bold")) ->
            geom_box_base
        if (input$logx == FALSE & input$logy == FALSE) {
            geom_box_base
        } else if (input$logx == TRUE & input$logy == FALSE) {
            geom_box_base +
                scale_x_log10()
        } else if (input$logx == FALSE & input$logy == TRUE) {
            geom_box_base +
                scale_y_log10()
        } else if (input$logx == TRUE & input$logy == TRUE) {
            geom_box_base +
                scale_x_log10() +
                scale_y_log10()
        }
    })
    # TAB 1 >> Jitter Plot ----
    plot_jitter <- reactive({
        ggplot() +
            ggtitle(wd24_title_filter(), subtitle = "Note: All basic statistics are on a per-game basis") +
            geom_jitter_interactive(aes(x = !!input$varx, y = !!input$vary,
                                        tooltip = !!input$varx, 
                                        data_id = !!input$varx),  
                                    data = wd24_reactive()) +
            theme(axis.text = element_text(size = 10, face = "italic"),
                  axis.title = element_text(size = 12, face = "italic"),
                  plot.title = element_text(size = 12, face = "bold"))
    })
    
    # _______________________________________ ----
    
    # DISPLAY ----
    
    # TAB 3 >>> Plot ----
    output$plot3 <- renderggiraph({
            ggiraph(code = print(plot_league_data()),
                    width_svg = 8, height_svg = 5)
    })
    
    # TAB 1 >>> Plot ----
    
    #output$regression <- renderText(wd24_regression_equation())
    
    # observeEvent(input$analysis, {
    #     
    #     model=lm(formula = !!input$vary ~ !!input$varx, data = wd24_player_filter())
    # })
    # 
    #     output$modelSummary <- renderPrint({
    #         model=lm(formula = !!input$vary ~ !!input$varx, data = wd24_player_filter())
    #         summary(model)
    #     })
        
    
    output$plot2 <- renderggiraph({
        if (class(x3()) == "numeric" && class(x4()) == "numeric") {
            ggiraph(code = print(plot_scatter()),
                    width_svg = 8, height_svg = 5)
        }
        else if (class(x3()) == "numeric" &&
                 class(x4()) == "factor") {
            validate(
                need(
                    input$logy == FALSE,
                    "Error: Cannot log transform factor or variable with 1+ values of 0"
                )
            )
            ggiraph(code = print(plot_box_2()),
                    width_svg = 8, height_svg = 5)
        }
        else if (class(x3()) == "factor" &&
                 class(x4()) == "numeric") {
            validate(
                need(
                    input$logx == FALSE,
                    "Error: Cannot log transform factor or variable with 1+ values of 0"
                )
            )
            ggiraph(code = print(plot_box_2()),
                    width_svg = 8, height_svg = 5)
        }
        else if (class(x3()) == "factor" &&
                 class(x4()) == "factor") {
            validate(
                need(
                    input$logx == FALSE,
                    "Error: Cannot log transform factor or variable with 1+ values of 0"
                ),
                need(
                    input$logy == FALSE,
                    "Error: Cannot log transform factor or variable with 1+ values of 0"
                )
            )
            ggiraph(code = print(plot_jitter()))
        }
    })
    
    # TAB 1 >>> Table ----
    
    output$table <- renderDataTable({
        wd24_player_filter() %>%
            select(Year, Player, Team, Position, !!input$varx, !!input$vary) %>%
            arrange(desc(!!input$vary))
    })
    
    # _______________________________________ ----
    # DISPLAY ----
    # TAB 3 >>> Data Table ----
    output$plot4 <- renderDataTable({
        wd24
    })
}
# _________________________________________ ----
shinyApp(ui = ui, server = server)

# Current Errors ----
## TAB 1 ----

## TAB 2 ----
#If you change the year after selecting the player it does not work

#Tooltip for boxplots should include min, Q1, med/mean, Q3, max

# Deleted Code ----

## Code for adding robocoko link ----

# wnba_careers_personality_filter <- reactive({
#     wnba_careers_player_filter() %>%
#         filter(image_link != "Career") %>%
#         head(1) %>%
#         pull(image_link)
# })

## Code for filtering by position ----

# wd24_position_filter <- reactive({
#     if (input$position == "All") {
#         wd24_year_filter_p()
#     } else if (input$position != "All") {
#         wd24_year_filter_p() %>%
#             filter(Pos == input$position)
#     }
# })

## Code for displaying variable name ----

#output$var_name<-renderText({c('Variable name is',x_variable_name_reactive(),'">')})