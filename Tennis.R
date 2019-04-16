library(tidyverse)
library(gganimate)
library(emoGG)
library(plotly)

#setwd("2019/W15-Tennis-GS/")
extrafont::loadfonts(device="win")
player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

head(player_dob)
head(grand_slams)
head(grand_slam_timeline)


# Seeing top winners ------------------------------------------------------

grand_slams %>% count(name, sort=TRUE) %>% top_n(10) %>% 
  mutate(name=fct_reorder(name,n)) %>% 
  ggplot(aes(name, n)) + geom_col() +coord_flip()


age_slams_comb <- left_join(grand_slams, player_dob, by = c("name")) %>% 
  mutate(age = tournament_date - date_of_birth) %>% # needs to be datetime
  group_by(name, age, gender) %>% 
  summarize(counts = n()) %>% 
  group_by(name) %>% 
  mutate(total_wins = cumsum(counts)) %>% 
  arrange(desc(total_wins))

min(age_slams_comb$age) # 5946
max(age_slams_comb$age) # 13583

numdaysold <- seq(5900, 13600, by = 36)
allwinners <- unique(age_slams_comb$name) 
complete.df <- expand.grid(numdaysold, allwinners, stringsAsFactors = FALSE)
names(complete.df) <- c("agedays","player")

system.time(
  complete.df$numTitles <- unlist(complete.df %>% purrr::pmap(function(agedays,player) {
    t = age_slams_comb %>% 
      ungroup() %>% 
      filter(as.numeric(age) < agedays) %>% 
      filter(name == player) %>% 
      arrange(desc(total_wins)) %>% 
      slice(1) %>% 
      select(total_wins) %>% 
      pull()
    if (length(t) == 0) return(0)
    else return(t)
  }))
)

# 67.46 seconds
# - Certainly a more optimised way to do this, perhaps something to work on.
















######################################################
# GIF 1 - Get the top 10 GS winners of all time and barplot race those 10 players
######################################################

top.players <- complete.df %>% 
  group_by(player) %>% 
  summarise(numTitles = max(numTitles)) %>% 
  ungroup() %>% 
  arrange(desc(numTitles)) %>% 
  top_n(10) %>% 
  select(player)

overall.top.df <- filter(complete.df, player %in% top.players$player) %>% 
  mutate(yadj = ifelse(numTitles == 0, 0, -1))

plt <- ggplot(data = overall.top.df,
              aes(x = player, y = numTitles)) + 
  geom_bar(stat = "identity", color = "#bfe567", fill = "#bfe567") + 
  geom_text(aes(label = numTitles, y = numTitles + yadj), color = "black", family="Comfortaa") + 
  geom_text(aes(x = 1, y = 21, label = paste0("Age: ",as.character(round(agedays/365,1)))), size = 10, family = "Comfortaa", color = "#ffec1b") + 
  ylim(c(0,27)) + 
  coord_flip() + 
  labs(
    title = "Who is the G.O.A.T?",
    subtitle = "Open-Era Grand-Slam Victories (at each age)",
    x = "Player",
    y = NULL
  ) + 
  hrbrthemes::theme_ft_rc() + 
  theme(
    text = element_text(family = "Comfortaa"),
    axis.text.x = element_blank(),
    axis.title.x = element_text(family = "Comfortaa", size = 14),
    axis.title.y = element_text(family = "Comfortaa", size = 14),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 16)
  ) + 
  transition_states(agedays)

anim.plt <- animate(plt, nframes = 624, fps = 10)

anim_save(filename = "tt-tennis.gif",
          animation = anim.plt)







######################################################################
# GIF 2 - Plot top 10 at each age rather than picking overall top 10 first and plotting
# - Also style it with an advancing tennis ball as number of GS wins increases
################################################################

t10_per_age <- complete.df %>% 
  filter(numTitles > 0) %>% 
  group_by(agedays) %>% 
  top_n(10, wt = numTitles) %>% 
  slice(1:10) %>% 
  arrange(desc(numTitles)) %>% 
  mutate(order = 10:(11-dplyr::n())) %>% 
  mutate(rank = 1:dplyr::n()) %>% 
  ungroup() %>% 
  arrange(agedays)

for (i in unique(t10_per_age$agedays)) {
  #print(i)
  howmany <- t10_per_age %>% 
    filter(agedays == i) %>% 
    nrow()
  if (!howmany == 10) {
    toadd <- 10 - howmany 
    t10_per_age <- rbind(t10_per_age,
                         data.frame(
                           agedays = rep(i, toadd),
                           player = rep("", toadd),
                           numTitles = rep(0, toadd),
                           order = (10-howmany):1,
                           rank = rep(NA, toadd)
                         )
    )
  }
}

t10_per_age <- t10_per_age %>% arrange(agedays)

plt <- ggplot(data = t10_per_age,
              aes(x = order)) + 
  #geom_bar(aes(y = numTitles), stat="identity", color = "#bfe567", fill = "#bfe567") + 
  geom_segment(aes(y=0, yend=numTitles, x = order, xend = order), color = "#bfe567") + 
  emoGG::geom_emoji(aes(y = numTitles), emoji = "1f3be") + 
  geom_text(aes(label = numTitles, y = numTitles),
            color = "black", family="Comfortaa") + 
  geom_text(aes(label = player, y = numTitles), hjust="left", 
            color = "white", family = "Comfortaa", size = 8, nudge_y = 1) + 
  geom_text(aes(x = 1, y = 28, label = paste0("Age: ",as.character(round(agedays/365,1)))), 
            size = 12, family = "Comfortaa", color = "white") + 
  ylim(c(0,32)) + 
  coord_flip() + 
  labs(
    title = "Number of grand slam victories",
    subtitle = "Top 10 by age",
    x = NULL,
    y = NULL
  ) + 
  hrbrthemes::theme_ft_rc() + 
  theme(
    text = element_text(family = "Comfortaa"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(family = "Comfortaa", size = 14),
    axis.title.y = element_text(family = "Comfortaa", size = 14),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 16),
    panel.grid = element_blank()
  ) + 
  transition_states(agedays,
                    transition_length = 4, 
                    state_length = 3) + 
  ease_aes('cubic-in-out')

animate(plt, nframes = 624, fps = 10, width = 800, height = 600, end_pause = 200)

anim_save("tt-tennis-t10age.gif")


#######################################################################
# GIF 3 - Stack tennis balls to express number of GS wins
##########################################################

expand_t10 <- function(r) {
  r <- r[rep(row.names(r), r$numTitles),]
  tmp <- r %>% 
    rownames_to_column(var = "numTitles_expanded") %>% 
    mutate(numTitles_expanded = as.integer(numTitles_expanded))
  return(tmp)
}

t10_per_age_expanded <- tibble()

for( i in 1:nrow(t10_per_age) ) {
  print(i)
  t10_per_age_expanded <- rbind(t10_per_age_expanded, expand_t10(t10_per_age[i,]))
}


plt <- ggplot(data = t10_per_age_expanded,
              aes(x = order, y = numTitles_expanded)) + 
  emoGG::geom_emoji(emoji = "1f3be") + 
  geom_text(aes(label = player, y = numTitles), hjust="left", 
            color = "white", family = "Comfortaa", size = 8, nudge_y = 1) + 
  geom_text(aes(x = 1, y = 28, label = paste0("Age: ",as.character(round(agedays/365,1)))), 
            size = 12, family = "Comfortaa", color = "white") + 
  ylim(c(0,32)) + 
  coord_flip() + 
  labs(
    title = "Grand Slam Victories",
    subtitle = "\U0001f3be one ball = one grand slam!",
    x = NULL,
    y = NULL
  ) + 
  hrbrthemes::theme_ft_rc() + 
  theme(
    text = element_text(family = "Comfortaa"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(family = "Comfortaa", size = 14),
    axis.title.y = element_text(family = "Comfortaa", size = 14),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 16),
    panel.grid = element_blank()
  ) + 
  transition_states(agedays,
                    transition_length = 4, 
                    state_length = 3) + 
  ease_aes('cubic-in-out')

animate(plt, nframes = 624, fps = 10, width = 800, height = 600, end_pause = 200)

anim_save("tt-tennis-t10age-emoji.gif")

###########################################################################
#