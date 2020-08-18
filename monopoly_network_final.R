library(readxl)
library(tidyverse)
library(matrixcalc) #for matrix.power()
library(tidygraph)
library(ggraph)
library(igraph) #for vertex.attributes()

#read transition matrix data
monopoly_df <- read_excel("monopoly_data_final.xlsx", sheet="markov matrix")

#make tibble of nodes; need numeric id column and character labels column
nodes <- tibble(id=seq(1, nrow(monopoly_df), 1), labels=monopoly_df$property)

#make tibble of edges; need numeric from and to columns, and weights
edges <- 
  monopoly_df %>% 
  pivot_longer(cols = -property) %>% 
  rename(to_char=property, from_char=name, weights=value)

#get numeric from and to columns from names
edges$from <- 0
edges$to <- 0
for(i in 1:nrow(nodes)){
  edges$from[edges$from_char==nodes$labels[i]] <- nodes$id[i]
  edges$to[edges$to_char==nodes$labels[i]] <- nodes$id[i]
}

#not necessary, but I like to see them in this order
edges <- 
  edges %>% 
  arrange(from, to)

#make the network
routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

#calculate centrality
routes_tidy <- 
  routes_tidy %>% 
  mutate(authority = centrality_authority(weights=weights))

#load coordinates of spaces
board <- read_excel("monopoly_data_final.xlsx", sheet = "board")

p.network.square <- ggraph(routes_tidy, layout = "manual", x=board$x, y=board$y) + 
  geom_edge_parallel(aes(alpha=weights, width=weights), color="#737373",
                     arrow = arrow(length = unit(4, 'mm')), 
                     end_cap = circle(5, 'mm')) + 
  geom_node_point(shape=21, size=8, fill="white", stroke=4, aes(color=cut_number(authority,4))) +
  geom_node_text(aes(label = id), color="black", fontface = "bold") + 
  scale_color_viridis_d()+
  scale_edge_width(range=c(.01,0.75), guide=FALSE)+
  scale_edge_alpha(range=c(.01,.75), guide=FALSE)+
  labs(color="Network Centrality",
       title="Network Analysis of \"Monopoly\"")+
  theme_graph()+
  coord_equal()+
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))

p.network.square

ggsave("network_square.png", p.network.square, width=8, height=6, units="in")

#just in case -- export centrality results
centrality_results <- data.frame(nodes=nodes$id,
                                 labels=nodes$labels,
                                 authority=vertex.attributes(routes_tidy)$authority)
#for some reason this throws an error as a tibble

ggplot(centrality_results, aes(x=authority))+
  geom_histogram(color="white", binwidth = 0.01)

#write_csv(centrality_results, "network_centrality.csv")
#write_csv(nodes, "nodes.csv")
#write_csv(edges, "edges.csv")