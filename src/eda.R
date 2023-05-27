library(ggforce)
library(ggplot2)

free_throws <- read.csv("data/free_throws.csv",stringsAsFactors = FALSE)
unique_ids <- unique(free_throws$game_id)
free_throws_sampled_games <- sample(x = unique_ids,
                                    size = length(unique_ids)*0.1,replace = FALSE)
free_throws_sample_indx <- which(free_throws$game_id %in% free_throws_sampled_games)
free_throws <- free_throws[free_throws_sample_indx,]

teams_split <- do.call(rbind,strsplit(free_throws$game,split = " - "))
free_throws$teamA <- teams_split[,1]
free_throws$teamB <- teams_split[,2]

# free_throws$seasonStart <- do.call(rbind,strsplit(free_throws$season,split = " - "))[,2]


# player_data <- read.csv("data/Seasons_Stats.csv",stringsAsFactors = FALSE)
# 
# get_player_team <- function(year,player){
#   free_throws_yearPlayer <- paste0(free_throws$seasonStart,"_",free_throws$player)
#   player_data_yearPlayer <- paste0(player_data$Year,"_",player_data$Player)
#   free_throws_yearPlayer[18]
#   player_data[which(player_data$Player=="Jord"),]
#   match(free_throws_yearPlayer,player_data_yearPlayer)
#   # year_indx <- which(player_data$Year==year)
#   # player_indx <- which(player_data$Player==player)
#   # team_indx <- intersect(year_indx,player_indx)
#   return(player_data$Tm[team_indx])
# }
# free_throws$player_team <- sapply(1:nrow(free_throws),
#                                   function(i) get_player_team(year = free_throws$seasonStart[i],
#                                                               player = free_throws$player[i]))


# Convert time to seconds
time_split_raw <- unlist(strsplit(free_throws$time,split = ":"))
time_split <- matrix(as.numeric(time_split_raw),
                     ncol = 2,byrow = TRUE)
free_throws$time_seconds <- (time_split[,1] * 60) + time_split[,2]
free_throws$time_seconds_overall <- 
  (free_throws$period - 1) * (12 * 60) + # Time playing in previous quarters
  free_throws$time_seconds # Time played in this quarter
  
get_score_differential <- function(scores_in){
  split_scores <- strsplit(scores_in,split = " - ")
  split_scores_matrix <- matrix(as.numeric(unlist(split_scores)),ncol = 2,byrow = TRUE)
  split_scores_diff <- apply(split_scores_matrix,1,function(x) abs(x[1] - x[2]))
  return(split_scores_diff)
}
free_throws$score_differential <- get_score_differential(free_throws$score)

free_throws[1,]

free_throws$time_interval_indx  <- as.numeric(cut(free_throws$time_seconds_overall,breaks = 41))
free_throws$score_interval_indx <- as.numeric(cut(free_throws$score_differential,breaks = 41))



score_time_interval_matrix <-
  sapply(unique(free_throws$time_interval_indx),function(time_interval)
    sapply(unique(free_throws$score_interval_indx),function(score_interval){
      t_indx <- which(time_interval_indx==time_interval)
      s_indx <- which(score_interval_indx==score_interval)
      joint_indx <- intersect(s_indx,t_indx)
      if(length(joint_indx)>20){
        sum(free_throws$shot_made[joint_indx]) / length(joint_indx)  
      }else{
        NaN
      }
    }))
# colnames(score_time_interval_matrix) <- unique(time_interval_indx)
# rownames(score_time_interval_matrix) <- unique(score_interval_indx)
score_time_interval_long <- reshape2::melt(score_time_interval_matrix,
                                           varnames = c("Time interval","Score interval"),
                                           value.name = "Proportion successful")
score_time_interval_long$`Time interval` <- factor(score_time_interval_long$`Time interval`)
score_time_interval_long$`Score interval` <- factor(score_time_interval_long$`Score interval`)
ggplot(score_time_interval_long,aes(x = `Time interval`,
                                    y = `Score interval`,
                                    fill = `Proportion successful`)) +
  geom_tile()

score_time_interval_long

library(ggplot2)
ggplot(free_throws,aes(x = time_seconds_overall,y = score_differential,fill = factor(shot_made))) + 
geom_point(shape = 21)

ggplot(score_time_interval_long,aes(x = `Score interval`,y = Count)) + 
  geom_violin()

#### First vs second throws
makes_indx <- grep("makes free throw",free_throws$play)
misses_indx <- grep("misses free throw",free_throws$play)
of_indx <- grep("of",free_throws$play)
makes_indx <- intersect(makes_indx,of_indx)
misses_indx <- intersect(misses_indx,of_indx)

free_throws_mm <- free_throws[unique(c(makes_indx,misses_indx)),]
free_throws_mm$play <- gsub(pattern = "clear path ",
                            replacement = "",
                            x = free_throws_mm$play)
free_throws_mm$number_of_shots <- as.numeric(sapply(strsplit(free_throws_mm$play,split = " of "),"[[",2))

get_throw_num <- function(strng){
  shot_info <- strsplit(strng,split = " of ")[[1]][1]
  as.numeric(tail(strsplit(shot_info,split = "")[[1]],1))
}
free_throws_mm$play[1]
free_throws_mm$throw_number <- sapply(free_throws_mm$play,get_throw_num)

free_throws_mm$shot_id <- paste0(free_throws_mm$game_id,"_",free_throws_mm$time_seconds_overall)
unique_sids <- unique(free_throws_mm$shot_id)
unique_sids_indx <- sapply(unique_sids,function(this_sid) which(free_throws_mm$shot_id==this_sid))
shots_code <- sapply(unique_sids_indx,function(i) {
  indx_order <- sort.int(free_throws_mm$throw_number[i],index.return = TRUE)$ix
  i_ordered <- i[indx_order]
  paste0(free_throws_mm$shot_made[i_ordered],collapse = "")
})
shots_code_counts <- table(shots_code)
shots_code_counts_df <- 
  data.frame(shot_code = names(shots_code_counts),
             n_shots = nchar(names(shots_code_counts)),
             shot_counts = as.vector(shots_code_counts),stringsAsFactors = FALSE)
shot_code_matrix <- 
  t(sapply(shots_code_counts_df$shot_code,function(this_code) {
    split_code <- strsplit(this_code,split = "")[[1]]
    as.vector(sapply(1:max(nchar(shots_code_counts_df$shot_code)),function(i) split_code[i]))
  }))
colnames(shot_code_matrix) <- paste0("Shot_",1:max(nchar(shots_code_counts_df$shot_code)))
shots_code_counts_df_mat <- cbind(shots_code_counts_df,shot_code_matrix)
shots_code_counts_df_mat <- shots_code_counts_df_mat[sort.int(shots_code_counts_df_mat$n_shots,
                                                              index.return = TRUE)$ix,]
shots_code_counts_df_mat_by_Nshots <- split(shots_code_counts_df_mat,
                                            f = shots_code_counts_df_mat$n_shots)
two_shot_cm <- matrix(shots_code_counts_df_mat_by_Nshots$`2`$shot_counts,ncol = 2)
colnames(two_shot_cm) <- c("First shot missed","First shot hit")
rownames(two_shot_cm) <- c("Second shot missed","Second shot hit")
apply(two_shot_cm,2,function(x) x/sum(x))


shots_code_counts_df2 <- data.frame(cbind(shot_code_matrix,
                                          count = shots_code_counts_df$shot_counts),stringsAsFactors = FALSE)
rownames(shots_code_counts_df2) <- NULL
shots_code_counts_df2$count <- as.numeric(shots_code_counts_df2$count)
shots_code_counts_df2$Shot_1 <- as.character(as.logical(as.numeric(shots_code_counts_df2$Shot_1)))
shots_code_counts_df2$Shot_2 <- as.character(as.logical(as.numeric(shots_code_counts_df2$Shot_2)))
shots_code_counts_df2$Shot_3 <- as.character(as.logical(as.numeric(shots_code_counts_df2$Shot_3)))
shots_code_counts_df2$Shot_4 <- as.character(as.logical(as.numeric(shots_code_counts_df2$Shot_4)))
shots_code_counts_df2$Shot_5 <- as.character(as.logical(as.numeric(shots_code_counts_df2$Shot_5)))
shots_code_counts_df2$Shot_6 <- as.character(as.logical(as.numeric(shots_code_counts_df2$Shot_6)))
# shots_code_counts_df_long <- melt(data = shots_code_counts_df2,
#                                   measure.vars = paste0("Shot_",1:6),value.name = "shot_made")
# shots_code_counts_df_long[1,]


set_data <- gather_set_data(data = shots_code_counts_df2,x = 1:6)
set_data$count
View(set_data)

set_data$Shot_2[is.na(set_data$Shot_2)] <- "No data"
set_data$Shot_3[is.na(set_data$Shot_3)] <- "No data"
set_data$Shot_4[is.na(set_data$Shot_4)] <- "No data"
set_data$Shot_5[is.na(set_data$Shot_5)] <- "No data"
set_data$Shot_6[is.na(set_data$Shot_6)] <- "No data"
set_data$y[is.na(set_data$y)] <- "No data"
table(set_data$x)
ggplot(set_data, aes(x, id = id, split = y, fill = Shot_3,value = count)) +
geom_parallel_sets(alpha = 0.3, axis.width = 0.01) +
geom_parallel_sets_axes(axis.width = 0.1) +
geom_parallel_sets_labels(colour = 'black')


# Look at most prolific scorers




players <- unique(free_throws_mm$player)
player_indices <- sapply(players,function(x) which(free_throws_mm$player==x))
min_n_shots <- 100
player_n_shots <- sapply(player_indices,length)
top_players <- players[which(player_n_shots>min_n_shots)]

final_x_minutes <- 5
final_seconds <- 60 * ((12*4)-final_x_minutes)
min_score_difference <- 10

tense_indices <- intersect(which(free_throws_mm$time_seconds_overall>final_seconds),
                           which(free_throws_mm$score_differential<min_score_difference))
tense_data <- free_throws_mm[tense_indices,]
chill_data <- free_throws_mm[-tense_indices,]

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
tense_shots <- tense_shots[which(tense_shots$taken>20),]
chilled_shots <- chilled_shots[which(chilled_shots$taken>20),]
tense_players <- unique(tense_shots$player)
chilled_shots <- chilled_shots[match(tense_players,chilled_shots$player),]
all_shots <- data.frame(player = chilled_shots$player,
                        chilled_propn = chilled_shots$propn,
                        tense_propn = tense_shots$propn)
all_shots$diff <- (all_shots$tense_propn - all_shots$chilled_propn)#/((all_shots$tense_propn + all_shots$chilled_propn)/2)

all_shots_differential <- all_shots[which(abs(all_shots$diff)>0.12),]
all_shots_differential <- all_shots_differential[order(all_shots_differential$diff,decreasing = TRUE),]
all_shots_differential$player <- factor(all_shots_differential$player,
                                        levels = as.character(all_shots_differential$player))
all_shots_differential$sign <- sign(all_shots_differential$diff)
max_diff <- max(abs(all_shots_differential$diff))

all_shots_differential_long <- melt(all_shots_differential,
                                    id.vars = c("player","sign"),
                                    measure.vars = c("tense_propn","chilled_propn"))

ggplot(all_shots_differential,aes(x = player,y = diff)) + 
  geom_point(aes(size = tense_propn)) + 
  geom_segment(aes(x = player,xend = player,y = 0,yend = diff)) + 
  coord_flip() +
  ylab("Difference in proportion of shots hit in tense and normal settings") + xlab("")

first_negative_player <- which(all_shots_differential$diff<0)[1]

g_dotplot <- 
  ggplot(all_shots_differential,aes(x = player)) + 
  geom_point(aes(y = chilled_propn),pch = 21,fill = "blue",size = 4) + 
  geom_point(aes(y = tense_propn),pch = 21,fill = "red",size = 4) + 
  geom_segment(aes(y = chilled_propn,yend = tense_propn,xend = player)) +
  coord_flip() +
  geom_vline(aes(xintercept = first_negative_player+0.5),linetype=2) +
  ylab("Proportion of free throws made in \n normal (blue) or tense (red) throws") + 
  xlab("Players") +
  theme_bw()

g_diffplot <- 
  ggplot(all_shots_differential,aes(x = player,y = diff)) +
  geom_bar(stat = "identity") + 
  coord_flip() + 
  geom_vline(aes(xintercept = first_negative_player+0.5),linetype=2) +
  ylab("P(normal) - \n P(tense)") + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

cowplot::plot_grid(g_dotplot,g_diffplot,rel_widths = c(6,1))
  
all_shots_differential_long[1,]
facet_grid()


g_chilled <-
  ggplot(all_shots_differential,aes(x = player,y = chilled_propn,fill = diff)) +
  geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(limits = c(0,1)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_continuous(limits = c(-max_diff,max_diff)) +
  facet_grid(sign~.,scales = "free",space = "free")
g_tense <-
  ggplot(all_shots_differential,aes(x = player,y = tense_propn,fill = diff)) +
  geom_bar(stat = "identity") + coord_flip() + scale_y_reverse(limits = c(1,0)) +
  scale_fill_continuous(limits = c(-max_diff,max_diff)) +
  theme(legend.position = "none") +
  facet_grid(sign~.,scales = "free",space = "free",)
library(cowplot)
cowplot::plot_grid(g_tense,g_chilled)
# free_throws_top_players <- free_throws_mm[free_throws_mm$player %in% top_players,]

hist(free_throws_top_players$time_seconds_overall)


free_throws_top_players

n_shots_made_per_year  <- aggregate(free_throws_top_players$shot_made,
                                   by = list(player = free_throws_top_players$player,
                                             season = free_throws_top_players$season),
                                   FUN = sum)
n_shots_taken_per_year <- aggregate(free_throws_top_players$shot_made,
                                   by = list(player = free_throws_top_players$player,
                                             season = free_throws_top_players$season),
                                   FUN = length)

shots_per_player <- data.frame(Player = n_shots_made_per_year$player,
                               Season = n_shots_made_per_year$season,
                               Taken = n_shots_taken_per_year$x,
                               Made = n_shots_made_per_year$x,
                               Propn = n_shots_made_per_year$x / n_shots_taken_per_year$x)
shots_per_player <- shots_per_player[which(shots_per_player$Taken>min_n_shots),]
shots_per_player <- shots_per_player[order(shots_per_player$Propn,decreasing = TRUE),]
shots_per_player

ggplot(shots_per_player,aes(x = Player,y = Propn)) + 
  geom_bar(stat = "identity") +
  facet_wrap("Season",drop = TRUE,scales = "free_x")
  
head(shots_per_player)


