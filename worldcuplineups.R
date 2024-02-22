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

centers <- nhl_stats %>% filter(position == "C")

stats_c <- centers %>%
  select(goals, assists, hr_point_shares, faceoff_win_percent)

stats_c$faceoff_win_percent[which(is.na(stats_c$faceoff_win_percent))] <- 0

pca_c <- prcomp(stats_c, scale = TRUE)
weights_c <- pca_c$rotation[,1]
weighted_avg_c <- rowSums(stats_c * weights_c)
centers$metric <- weighted_avg_c

left_wingers <- nhl_stats %>%
  filter(position == "LW") 

stats_lw <- left_wingers %>%
  select(goals, assists, hr_point_shares, shooting_percent)

stats_lw$shooting_percent[which(is.na(stats_lw$shooting_percent))] <- 0

pca_lw <- prcomp(stats_lw, scale = TRUE)
weights_lw <- pca_lw$rotation[,1]
weighted_avg_lw <- rowSums(stats_lw * weights_lw)
left_wingers$metric <- weighted_avg_lw

right_wingers <- nhl_stats %>%
  filter(position == "RW") 

stats_rw <- right_wingers %>%
  select(goals, assists, hr_point_shares, shooting_percent)

stats_rw$shooting_percent[which(is.na(stats_rw$shooting_percent))] <- 0

stats_rw <- scale(stats_rw)

pca_rw <- prcomp(stats_rw, scale = TRUE)
weights_rw <- pca_rw$rotation[,1]
weighted_avg_rw <- rowSums(stats_rw * weights_rw)
right_wingers$metric <- weighted_avg_rw

att_defense <- nhl_stats %>%
  filter(position == "D")

stats_ad <- att_defense %>%
  select(goals, assists, hr_point_shares)

stats_ad <- scale(stats_ad)

pca_ad <- prcomp(stats_ad, scale = TRUE)
weights_ad <- pca_ad$rotation[,1]
weighted_avg_ad <- rowSums(stats_ad * weights_ad)
att_defense$metric <- weighted_avg_ad

def_defense <- nhl_stats %>%
  filter(position == "D")

stats_dd <- def_defense %>%
  select(blocks, hits, time_on_ice)

stats_dd <- scale(stats_dd)

pca_dd <- prcomp(stats_dd, scale = TRUE)
weights_dd <- pca_dd$rotation[,1]
weighted_avg_dd <- rowSums(stats_dd * weights_dd)
def_defense$metric <- weighted_avg_dd

centers[,31] <- as.data.frame(apply(centers[,31], 2, function(x) rank(x) / length(x) * 100))
left_wingers[,31] <- as.data.frame(apply(left_wingers[,31], 2, function(x) rank(x) / length(x) * 100))
right_wingers[,31] <- as.data.frame(apply(right_wingers[,31], 2, function(x) rank(x) / length(x) * 100))
att_defense[,31] <- as.data.frame(apply(att_defense[,31], 2, function(x) rank(x) / length(x) * 100))
def_defense[,31] <- as.data.frame(apply(def_defense[,31], 2, function(x) rank(x) / length(x) * 100))

goalies <- get_goalie_stats_hr(2024) %>% filter(!is.na(goals_saved_above_average))

stats_g <- goalies %>%
  select(save_percent, goals_saved_above_average, hr_point_shares)

pca_g <- prcomp(stats_g, scale = TRUE)
weights_g <- pca_g$rotation[,1]
weighted_avg_g <- rowSums(stats_g * weights_g)
goalies$metric <- weighted_avg_g

goalies[,29] <- as.data.frame(apply(goalies[,29], 2, function(x) rank(x) / length(x) * 100))

centers <- centers %>%
  select(player, team_abbr, metric) %>%
  arrange(-metric)

left_wingers <- left_wingers %>%
  select(player, team_abbr, metric) %>%
  arrange(-metric)

right_wingers <- right_wingers %>%
  select(player, team_abbr, metric) %>%
  arrange(-metric)

att_defense <- att_defense %>%
  select(player, team_abbr, metric) %>%
  arrange(-metric)

def_defense <- def_defense %>%
  select(player, team_abbr, metric) %>%
  arrange(-metric)

goalies <- goalies %>%
  select(player, team_abbr, metric) %>%
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

duplicated_nondefense <- total_final[total_final$player.y %in% names(table(total_final$player.y)[table(total_final$player.y) >= 2]), ] %>% filter(pos != "AD" & pos != "DD")
duplicated_defense <- total_final[total_final$player.y %in% names(table(total_final$player.y)[table(total_final$player.y) >= 3]), ]

duplicated_mishaps <- rbind(duplicated_nondefense, duplicated_defense) %>% filter(player.dist != 0)

total_final <- anti_join(total_final, duplicated_mishaps)

total_final <- total_final %>% select(player = player.x, team, nationality, pos, metric)

total_final$nationality[which(total_final$nationality == "su")] <- "ru"

nationality_dfs <- split(total_final, total_final$nationality)

nationality_dfs <- Filter(function(df) (sum(df$pos %in% c("C", "LW", "RW")) >= 14) & (sum(df$pos == "AD") >= 8) & (sum(df$pos == "G") >= 3), nationality_dfs)

select_lineup <- function(df) {
  df <- df %>% group_by(pos) %>% arrange(-metric) %>% ungroup()
  
  c <- df %>% filter(pos == "C") %>% slice_head(n = 5)
  
  winger_count <- 14 - nrow(c)
  indiv_winger_count_og <- as.integer(winger_count/2)
  
  lw <- df %>% filter(pos == "LW") %>% slice_head(n = indiv_winger_count_og)
  rw <- df %>% filter(pos == "RW") %>% slice_head(n = indiv_winger_count_og)
  
  rest_of_wingers <- df %>% filter(pos == "LW" | pos == "RW") %>% filter(!(player %in% lw$player | player %in% rw$player)) %>% arrange(-metric)
  
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
    mutate(headshot_link = paste0("http://nhl.bamcontent.com/images/headshots/current/168x168/", id, ".jpg")) %>%
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

caption_1 = gt_align_caption("<b>AD</b>: Attacking Defenseman", "<b>DD</b>: Defending Defenseman")
caption_2 = gt_align_caption("Data from <b>hockeyR</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

lineups <- lapply(lineups, function(df) {
  transform(df, code = deparse(substitute(df)))
})

apply_gt <- function(df, code) {
  df %>% gt() %>% 
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
    tab_source_note(html(caption_1)) %>%
    tab_source_note(html(caption_2)) %>%
    tab_options(source_notes.font.size = 12)
}

gt_tables <- Map(apply_gt, lineups, names(lineups))

for (i in 1:length(gt_tables)) {
  gtsave(gt_tables[[i]], paste0(names(lineups)[i], ".png"), vwidth = 1000, vheight = 2500)
}

ca <- ggdraw() + draw_image("ca.png")
us <- ggdraw() + draw_image("us.png")
se <- ggdraw() + draw_image("se.png")
ru <- ggdraw() + draw_image("ru.png")
fi <- ggdraw() + draw_image("fi.png")

grid <- plot_grid(ca, us, se, ru, fi, nrow = 1, rel_widths = c(1, 1, 1, 1, 1), rel_heights = c(1, 1, 1, 1, 1))
ggsave("all.png", grid)