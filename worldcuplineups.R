library(hockeyR)
library(dplyr)
library(tidyverse)
library(gt)
library(gtExtras)
library(rvest)
library(fuzzyjoin)
library(cowplot)
library(ggplot2)

nhl_stats <- get_skater_stats_hr(2024) 

centers <- nhl_stats %>% filter(position == "C" | position == "F")

stats_c <- centers %>%
  select(goals, assists, hr_point_shares, faceoff_win_percent)

stats_c$faceoff_win_percent[which(is.na(stats_c$faceoff_win_percent))] <- 0

pca_c <- prcomp(stats_c, scale = TRUE)
weights_c <- pca_c$rotation[,1]
weighted_avg_c <- rowSums(stats_c * abs(weights_c))
centers$metric <- weighted_avg_c

left_wingers <- nhl_stats %>%
  filter(position == "LW" | position == "F") 

stats_lw <- left_wingers %>%
  select(goals, assists, hr_point_shares, shooting_percent)

stats_lw$shooting_percent[which(is.na(stats_lw$shooting_percent))] <- 0

pca_lw <- prcomp(stats_lw, scale = TRUE)
weights_lw <- pca_lw$rotation[,1]
weighted_avg_lw <- rowSums(stats_lw * abs(weights_lw))
left_wingers$metric <- weighted_avg_lw

right_wingers <- nhl_stats %>%
  filter(position == "RW" | position == "F") 

stats_rw <- right_wingers %>%
  select(goals, assists, hr_point_shares, shooting_percent)

stats_rw$shooting_percent[which(is.na(stats_rw$shooting_percent))] <- 0

pca_rw <- prcomp(stats_rw, scale = TRUE)
weights_rw <- pca_rw$rotation[,1]
weighted_avg_rw <- rowSums(stats_rw * abs(weights_rw))
right_wingers$metric <- weighted_avg_rw

att_defense <- nhl_stats %>%
  filter(position == "D")

stats_ad <- att_defense %>%
  select(goals, assists, hr_point_shares)

pca_ad <- prcomp(stats_ad, scale = TRUE)
weights_ad <- pca_ad$rotation[,1]
weighted_avg_ad <- rowSums(stats_ad * abs(weights_ad))
att_defense$metric <- weighted_avg_ad

def_defense <- nhl_stats %>%
  filter(position == "D")

stats_dd <- def_defense %>%
  select(blocks, hits, time_on_ice)

pca_dd <- prcomp(stats_dd, scale = TRUE)
weights_dd <- pca_dd$rotation[,1]
weighted_avg_dd <- rowSums(stats_dd * abs(weights_dd))
def_defense$metric <- weighted_avg_dd

centers <- centers %>% group_by(player) %>% summarize(team_abbr = last(team_abbr), metric = sum(metric))
left_wingers <- left_wingers %>% group_by(player) %>% summarize(team_abbr = last(team_abbr), metric = sum(metric))
right_wingers <- right_wingers %>% group_by(player) %>% summarize(team_abbr = last(team_abbr), metric = sum(metric))
att_defense <- att_defense %>% group_by(player) %>% summarize(team_abbr = last(team_abbr), metric = sum(metric))
def_defense <- def_defense %>% group_by(player) %>% summarize(team_abbr = last(team_abbr), metric = sum(metric))

centers[,3] <- as.data.frame(apply(centers[,3], 2, function(x) rank(x) / length(x) * 100))
left_wingers[,3] <- as.data.frame(apply(left_wingers[,3], 2, function(x) rank(x) / length(x) * 100))
right_wingers[,3] <- as.data.frame(apply(right_wingers[,3], 2, function(x) rank(x) / length(x) * 100))
att_defense[,3] <- as.data.frame(apply(att_defense[,3], 2, function(x) rank(x) / length(x) * 100))
def_defense[,3] <- as.data.frame(apply(def_defense[,3], 2, function(x) rank(x) / length(x) * 100))

goalies <- get_goalie_stats_hr(2024) %>% filter(!is.na(goals_saved_above_average))

stats_g <- goalies %>%
  select(save_percent, goals_saved_above_average, hr_point_shares)

pca_g <- prcomp(stats_g, scale = TRUE)
weights_g <- pca_g$rotation[,1]
weighted_avg_g <- rowSums(stats_g * abs(weights_g))
goalies$metric <- weighted_avg_g

goalies <- goalies %>% group_by(player) %>% summarize(team_abbr = last(team_abbr), metric = sum(metric))

goalies[,3] <- as.data.frame(apply(goalies[,3], 2, function(x) rank(x) / length(x) * 100))

centers <- centers %>%
  arrange(-metric)

left_wingers <- left_wingers %>%
  arrange(-metric)

right_wingers <- right_wingers %>%
  arrange(-metric)

att_defense <- att_defense %>%
  arrange(-metric)

def_defense <- def_defense %>%
  arrange(-metric)

goalies <- goalies %>%
  arrange(-metric)

logos <- team_logos_colors %>%
  select(team_abbr, team_logo_espn) %>%
  head(32)

players <- data.frame()

teams <- get_team_records(2024)

for (team in teams$team_abbr) {
  url <- paste0("https://www.hockey-reference.com/teams/", team, "/")
  page <- read_html(url)
  table <- page %>% html_element("#roster") %>% html_table() %>% mutate(team = team)
  players <- rbind(players, table)
  Sys.sleep(3)
}

players$Player <- gsub("\\([^)]+\\)", "", players$Player)

total <- bind_rows(
  centers %>% mutate(pos = "C"),
  left_wingers %>% mutate(pos = "LW"),
  right_wingers %>% mutate(pos = "RW"),
  att_defense %>% mutate(pos = "AD"),
  def_defense %>% mutate(pos = "DD"),
  goalies %>% mutate(pos = "G")
)

players <- players %>% select(player = Player, team, nationality = Flag) %>% distinct(player, team, .keep_all = TRUE)

total_final <- stringdist_left_join(total, players, by = c("player", "team_abbr"="team"), method = "jw", max_dist = 1, distance_col = 'dist') %>%
  group_by(player.x, team_abbr, pos) %>%
  filter(player.dist == min(player.dist) & team_abbr.dist == min(team_abbr.dist)) 

total_final <- total_final %>% filter(!(player.x %in% c("Blake Wheeler", "Ben Meyers", "Robert Bortuzzo", "Taylor Hall")))

total_final <- total_final %>% ungroup() %>% select(player = player.x, team, nationality, pos, metric)

total_final$nationality[which(total_final$nationality == "su")] <- "ru"

total_final$nationality[which(total_final$player == "William Nylander")] <- "se"

nationality_dfs <- split(total_final, total_final$nationality)

nationality_dfs <- Filter(function(df) (sum(df$pos %in% c("C", "LW", "RW")) >= 14) & (sum(df$pos == "AD") >= 8) & (sum(df$pos == "G") >= 3), nationality_dfs)

select_lineup <- function(df) {
  df <- df %>% group_by(pos) %>% arrange(-metric) %>% ungroup()
  
  c <- df %>% filter(pos == "C") %>% slice_head(n = 5)
  
  winger_count <- 14 - nrow(c)
  indiv_winger_count_og <- as.integer(winger_count/2)
  
  lw <- df %>% filter(pos == "LW", !(player %in% c$player)) %>% slice_head(n = indiv_winger_count_og)
  rw <- df %>% filter(pos == "RW", !(player %in% c$player), !(player %in% lw$player)) %>% slice_head(n = indiv_winger_count_og)
  
  rest_of_wingers <- df %>% filter(pos == "LW" | pos == "RW") %>% filter(!(player %in% c$player | player %in% lw$player | player %in% rw$player)) %>% arrange(-metric)
  
  other_winger <- data.frame()
  
  if (winger_count - indiv_winger_count_og * 2 > 0) {
    other_winger <- rest_of_wingers %>% slice_head(n = 1)
  }
  
  ad <- df %>% filter(pos == "AD") %>% slice_head(n = 4)
  
  dd <- df %>% filter(pos == "DD", !(player %in% ad$player)) %>% slice_head(n = 4)
  
  g <- df %>% filter(pos == "G") %>% slice_head(n = 3)
  
  lineup <- rbind(c, lw, rw, other_winger, ad, dd, g) %>% arrange(match(pos, c("C", "LW", "RW", "AD", "DD", "G"))) %>% mutate(metric = round(metric)) %>% select(player, team, pos, metric)
  return(lineup)
}

lineups <- lapply(nationality_dfs, select_lineup)

pbp_24 <- load_pbp(2024) %>%
  select(name = event_player_1_name, id = event_player_1_id) %>%
  distinct(id, .keep_all = TRUE)

lineups <- lapply(lineups, function(df) {
  stringdist_left_join(df, pbp_24, by = c("player" = "name"), method = "jw", max_dist = 1, distance_col = 'dist') %>%
    group_by(player) %>%
    filter(dist == min(dist)) %>%
    select(id, player, team, pos, metric)
})

logos$team_abbr[which(logos$team_abbr == "VGK")] <- "VEG"

lineups <- lapply(lineups, function(df) {
  left_join(df, logos, by = c("team" = "team_abbr")) %>%
    mutate(team = ifelse(team == "VEG", "VGK", team)) %>%
    mutate(headshot_link = paste0("https://assets.nhle.com/mugs/nhl/20232024/", team, "/", id, ".png")) %>%
    filter(!((team == "NYI" & id == "8478427") | (team == "CAR" & id == "8480222"))) %>%
    select(headshot_link, player, team_logo_espn, pos, metric) %>%
    ungroup()
})


gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>hockeyR</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

apply_gt <- function(df, code) {
  df_1 <- df %>% filter(pos %in% c("C", "AD", "G"))
  df_2 <- df %>% filter(pos %in% c("LW", "RW", "DD"))
  df_1$pos[which(df_1$pos == "C")] <- "Centers"
  df_1$pos[which(df_1$pos == "AD")] <- "Attacking Defensemen"
  df_1$pos[which(df_1$pos == "G")] <- "Goalies"
  df_2$pos[which(df_2$pos == "LW")] <- "Left Wingers"
  df_2$pos[which(df_2$pos == "RW")] <- "Right Wingers"
  df_2$pos[which(df_2$pos == "DD")] <- "Defending Defensemen"
  df_1 <- df_1 %>% group_by(pos)
  df_2 <- df_2 %>% group_by(pos)
  table_1 <- df_1 %>% gt() %>% 
    gt_img_rows(columns = team_logo_espn) %>%
    gt_img_rows(columns = headshot_link) %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c(headshot_link, player, team_logo_espn, pos, metric)
    ) %>%
    gt_hulk_col_numeric(metric) %>%
    cols_label(
      headshot_link = md(""),
      player = md("**Player**"),
      team_logo_espn = md("**Team**"),
      pos = md("**Position**"),
      metric = md("**Metric**")
    ) %>%
    tab_header(
      title = add_text_img(url = paste0("https://www.worldatlas.com/r/w236/img/flag/", code, "-flag.jpg"), "", left = TRUE),
      subtitle = md("**2025 Hockey World Cup** - *Based on 23-24 NHL Stats*")
    ) %>%
    opt_align_table_header(align = "center") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = player
      )
    ) %>%
    tab_options(
      row_group.background.color = "black",
      row_group.font.weight = "bold",
      row_group.text_transform = "capitalize"
    ) %>%
    tab_style(
      style = cell_text(align = "center", style = "italic"),
      locations = cells_row_groups(everything()))
  table_2 <- df_2 %>% gt() %>% 
    gt_img_rows(columns = team_logo_espn) %>%
    gt_img_rows(columns = headshot_link) %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c(headshot_link, player, team_logo_espn, pos, metric)
    ) %>%
    gt_hulk_col_numeric(metric) %>%
    cols_label(
      headshot_link = md(""),
      player = md("**Player**"),
      team_logo_espn = md("**Team**"),
      pos = md("**Position**"),
      metric = md("**Metric**")
    ) %>%
    opt_align_table_header(align = "center") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = player
      )
    ) %>%
    tab_options(
      row_group.background.color = "black",
      row_group.font.weight = "bold",
      row_group.text_transform = "capitalize"
    ) %>%
    tab_style(
      style = cell_text(align = "center", style = "italic"),
      locations = cells_row_groups(everything())
    ) %>%
    tab_source_note(html(caption)) %>%
    tab_options(source_notes.font.size = 12)
  gt_two_column_layout(list(table_1, table_2), "viewer")
}

gt_tables <- Map(apply_gt, lineups, names(lineups))

