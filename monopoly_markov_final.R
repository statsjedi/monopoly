library(readxl)
library(tidyverse)
library(matrixcalc) #for matrix.power()
library(ggraph) #for theme_graph()

#read transition matrix data
monopoly_df <- read_excel("monopoly_data_final.xlsx", sheet="markov matrix")
monopoly_matrix <- as.matrix(monopoly_df[,2:ncol(monopoly_df)])

#initial state is being on space 1 "Go"
initial_state <- c(1, rep(0,nrow(monopoly_matrix)-1))

#permute 100 times
rounds <- 100 

#initialize matrix of results
results_matrix <- matrix(nrow = nrow(monopoly_matrix), ncol = rounds) 

for(i in 1:rounds){ #calculate result matrix
  results_matrix[,i] <- matrix.power(monopoly_matrix, i) %*% initial_state
}

#read in data to plot spaces on a board, append markov probabilities
markov_board <- read_excel("monopoly_data_final.xlsx", sheet="board")
markov_board$markov_probability <- results_matrix[,rounds]

p.markov <- ggplot(markov_board, aes(x=x, y=y, color=cut_number(markov_probability,4)))+
  geom_point(shape=21, fill="white", size=8, stroke=4)+
  geom_text(aes(label=id), color="black", fontface = "bold")+
  scale_color_viridis_d()+
  labs(color="Probability", title="Markov Simulation of \"Monopoly\"")+
  theme_graph()+
  coord_equal()+
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))

p.markov

ggsave("markov_plot.png", p.markov, width=8, height=6, units="in")

ggplot(markov_board, aes(x=markov_probability))+
  geom_histogram(color="white", binwidth=0.0005)
