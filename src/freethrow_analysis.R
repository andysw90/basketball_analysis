# Load libraries
library(cowplot)
library(ggforce)
library(ggplot2)

# Load data
free_throws <- read.csv("data/free_throws.csv",stringsAsFactors = FALSE)

min_n_shots <- 20
min_score_difference <- 10
final_x_minutes <- 5

# # If testing mode
# unique_ids <- unique(free_throws$game_id)
# free_throws_sampled_games <- sample(x = unique_ids,
#                                     size = length(unique_ids)*0.1,replace = FALSE)
# free_throws_sample_indx <- which(free_throws$game_id %in% free_throws_sampled_games)
# free_throws <- free_throws[free_throws_sample_indx,]

# Convert time to seconds, find overall game time
time_split_raw <- unlist(strsplit(free_throws$time,split = ":"))
time_split <- matrix(as.numeric(time_split_raw),
                     ncol = 2,byrow = TRUE)
free_throws$time_seconds <- (time_split[,1] * 60) + time_split[,2]
free_throws$time_seconds_overall <- 
  (free_throws$period - 1) * (12 * 60) + # Time playing in previous quarters
  free_throws$time_seconds # Time played in this quarter

# Find the difference between the scores of the two teams
get_score_differential <- function(scores_in){
  split_scores <- strsplit(scores_in,split = " - ")
  split_scores_matrix <- matrix(as.numeric(unlist(split_scores)),ncol = 2,byrow = TRUE)
  split_scores_diff <- apply(split_scores_matrix,1,function(x) abs(x[1] - x[2]))
  return(split_scores_diff)
}
free_throws$score_differential <- get_score_differential(free_throws$score)


# Extract only data where the throw is missed or made, exclude other stuff
makes_indx <- grep("makes free throw",free_throws$play)
misses_indx <- grep("misses free throw",free_throws$play)
of_indx <- grep("of",free_throws$play)
makes_indx <- intersect(makes_indx,of_indx)
misses_indx <- intersect(misses_indx,of_indx)

free_throws_mm <- free_throws[unique(c(makes_indx,misses_indx)),]
free_throws_mm$play <- gsub(pattern = "clear path ",
                            replacement = "",
                            x = free_throws_mm$play)


# players <- unique(free_throws_mm$player)
# player_indices <- sapply(players,function(x) which(free_throws_mm$player==x))
# min_n_shots <- 100
# player_n_shots <- sapply(player_indices,length)

# Find the "tense" moments of the game -- those where the score is close, and the time is nearly up
final_seconds <- 60 * ((12*4)-final_x_minutes)

tense_indices <- intersect(which(free_throws_mm$time_seconds_overall>final_seconds),
                           which(free_throws_mm$score_differential<min_score_difference))

# Split the data into tense moments and normal (chill) moments
tense_data <- free_throws_mm[tense_indices,]
chill_data <- free_throws_mm[-tense_indices,]

# Make a data frame that has, for each player, the proportion of shots that they make in chill/tense moments
get_player_shots <- function(data_in){
  taken <- aggregate(data_in$shot_made,
                     by = list(player = data_in$player),
                     FUN = length)
  made <- aggregate(data_in$shot_made,
                    by = list(player = data_in$player),
                    FUN = sum)
  data.frame(player = taken$player,
             made = made$x,
             taken = taken$x,
             propn = made$x / taken$x,stringsAsFactors = FALSE)
}
chilled_shots <- get_player_shots(chill_data)
tense_shots <- get_player_shots(tense_data)

# Remove players that have taken less than min_tense_shots tense shots

tense_shots <- tense_shots[which(tense_shots$taken>min_n_shots),]
chilled_shots <- chilled_shots[which(chilled_shots$taken>min_n_shots),]

# Extract only the players that have take tense shots, map them to the chilled shots
tense_players <- unique(tense_shots$player)
chilled_shots <- chilled_shots[match(tense_players,chilled_shots$player),]
all_shots <- data.frame(player = chilled_shots$player,
                        chilled_propn = chilled_shots$propn,
                        tense_propn = tense_shots$propn)
all_shots$diff <- (all_shots$chilled_propn - all_shots$tense_propn)

# Find the players that have a differential between their chilled and tense shots -- selected to give sensible N
all_shots_differential <- all_shots[which(abs(all_shots$diff)>0.12),]
all_shots_differential <- all_shots_differential[order(all_shots_differential$diff,decreasing = FALSE),]
all_shots_differential$player <- factor(all_shots_differential$player,
                                        levels = as.character(all_shots_differential$player))
all_shots_differential$sign <- sign(all_shots_differential$diff)
max_diff <- max(abs(all_shots_differential$diff))

first_negative_player <- which(all_shots_differential$diff>0)[1]

# Set up colour schemes according to Mango branding
dot_cols <- c("Tense" = rgb(100/255,100/255,255/255),"Normal" = rgb(255/255,116/255,0)) # Cols from Mango branding
bar_cols <- c(rgb(132/255,140/255,156/255),
              rgb(51/255,112/255,119/255))

# Create a dot-line-dot plot showing the players with some differential
performance_label_y <- max(c(all_shots_differential$tense_propn,all_shots_differential$chilled_propn)-0.03)
g_dotplot <- 
  ggplot(all_shots_differential,aes(x = player)) + 
  geom_segment(aes(y = chilled_propn,yend = tense_propn,xend = player)) +      # Add lines between high and tense
  geom_point(aes(y = chilled_propn,fill = "Normal"),pch = 21,size = 4) +       # Add normal tension points
  geom_point(aes(y = tense_propn,fill = "Tense"),pch = 21,size = 4) +          # Add high tension points
  coord_flip() +                                                               # Flip the axes
  scale_fill_manual(name = "Game tension",values = dot_cols) +                 # Set up the colours for the points
  geom_vline(aes(xintercept = first_negative_player-0.5),linetype=2) +         # Add a horizontal line
  annotate(geom = "text",                                                      # Add a note to the graph about high performance 
           x = first_negative_player+0.8,
           y = performance_label_y,
           label = "Worse performance in \ntense moments",size = 3,fontface = "italic") +
  annotate(geom = "text",                                                      # Add a note to the graph about low performance
           x = first_negative_player-1.8,
           y = performance_label_y,
           label = "Better performance in \ntense moments",size = 3,fontface = "italic") +
  ylab("Proportion of free throws made in \n normal (orange) or tense (blue) game moments") + 
  xlab("Players") +                                                            # Add axis labels          
  theme_bw() +                                                                 # Set a nice, clean theme
  theme(legend.position = c(0.92,0.93),legend.background = element_blank()) +  # Set legend position
  ggtitle(label = "Player performance (free-throw conversion) in tense game moments", # Add titles
          subtitle = paste0("Tense moments: when there is <",final_x_minutes,
                            " minutes left and the score differential is <",min_score_difference," points"))

# Create a bar plot showing the differential between tense and chill moments
g_diffplot <- 
  ggplot(all_shots_differential,aes(x = player,y = diff,fill = as.character(sign))) +
  scale_fill_manual(values = bar_cols) +
  geom_bar(stat = "identity") + 
  coord_flip() + 
  geom_vline(aes(xintercept = first_negative_player-0.5),linetype=2) +
  ylab("Normal - \n tense") + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  ggtitle(label = " ",subtitle = " ")

# Combine the two plots, write to file
g_out <- cowplot::plot_grid(g_dotplot,g_diffplot,rel_widths = c(6,1))
ggsave("free_throws_analysis_AndyWalker.pdf",plot = g_out,height = 7,width = 10)
