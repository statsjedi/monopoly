library(tidyverse)
library(readxl)
library(ggraph)

#dice roller function
#includes doubles rule and 3 doubles means go to jail
monte_carlo_monopoly <- function(){
  roll1d6 <- seq(1,6,1)
  temp <- tibble(rolla1=0, rolla2=0, rollb1=0, rollb2=0, rollc1=0, rollc2=0, sum=0)
  temp$rolla1 <- sample(roll1d6, 1)
  temp$rolla2 <- sample(roll1d6, 1)
  if(temp$rolla1 > 0 & temp$rolla1==temp$rolla2){
    temp$rollb1 <- sample(roll1d6, 1)
    temp$rollb2 <- sample(roll1d6, 1)
  }
  if(temp$rollb1 > 0 & temp$rollb1==temp$rollb2){
    temp$rollc1 <- sample(roll1d6, 1)
    temp$rollc2 <- sample(roll1d6, 1)
  }
  if(temp$rolla1==temp$rolla2 & temp$rollb1==temp$rollb2 & temp$rollc1==temp$rollc2){
    temp$sum <- -1 #go to jail
  }
  
  temp$sum <- rowSums(temp)
  return(temp)
}

#initialize the game board
game <- tibble(properties=seq(1, 40, 1), 
               initial=c(1, rep(0, 39)))
n_rounds <- 100000

for(i in 1:n_rounds){
  game_cols <- ncol(game)
  temp_initial <- game[,game_cols]
  initial_space <- which(temp_initial==1)
  if(initial_space==31){
    initial_space <- 11 #go from go to jail/in jail to just visiting
  }
  temp_roll <- monte_carlo_monopoly()$sum
  if(temp_roll==-1){
    final_space <- 31 #define go to jail and being in jail as the same space, to better mesh with
    #network and markov analyses
  } else{
    final_space <- temp_roll + initial_space
  }
  
  if(final_space > 40){
    final_space <- final_space - 40
  }
  
  temp_final <- tibble(temp_name=rep(0, 40))
  temp_final$temp_name[final_space] <- 1
  names(temp_final) <- paste("Round_", i, sep="")
  
  game <- bind_cols(game, temp_final)
}

game$sums <- rowSums(game[,3:ncol(game)])

sum_total <- sum(game$sums)
game$mc_probabilities <- game$sums/sum_total

game_plot <- 
  game %>% 
  select(properties, mc_probabilities)

board <- read_excel("monopoly_data_final.xlsx", sheet = "board")
head(board)

board$mc_probabilities <- game_plot$mc_probabilities

p.mcboard <- ggplot(board, aes(x=x, y=y, color=cut_number(mc_probabilities,4)))+
  geom_point(shape=21, fill="white", size=8, stroke=4)+
  geom_text(aes(label=id), color="black", fontface = "bold")+
  scale_color_viridis_d()+
  labs(color="Monte Carlo\nProbability\nof Landing\n100,000 turns",
       title="Monte Carlo Simulation of \"Monopoly\"")+
  theme_graph()+
  coord_equal()

p.mcboard

ggsave("monte_carlo_board.png", p.mcboard, width=8, height=6, units="in")

head(board)

write.csv(board, "monte_carlo probababilities.csv")

#save.image("~/R/monopoly/monopoly monte carlo2.RData")
#load("~/R/monopoly/monopoly monte carlo2.RData")
