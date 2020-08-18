library(tidyverse)
#this script calculates the probability of rolling different numbers in monopoly
#it takes into account rolling doubles, and 3 doubles sends you to jail
#this does not take into effect community chest and chance cards

roll1d6 <- seq(1,6,1)

#make the matrix
monopoly <- expand.grid(a1=roll1d6, a2=roll1d6, b1=roll1d6, b2=roll1d6, c1=roll1d6, c2=roll1d6)

#zero out the extra rolls for when doubles aren't rolled the first or second time
monopoly[monopoly[,1] != monopoly[,2],3:6] <- 0 
monopoly[monopoly[,3] != monopoly[,4],5:6] <- 0

#sum everything
monopoly$sum <- rowSums(monopoly)

triples <- 
  monopoly %>% 
  filter(a1==a2) %>% 
  filter(b1==b2) %>% 
  filter(c1==c2) %>% 
  mutate(sum=-1)

non_triples_0 <- 
  monopoly %>% 
  filter(c1==0)

non_triples <- 
  monopoly %>% 
  filter(c1 != c2)

monopoly2 <- bind_rows(non_triples, non_triples_0)
monopoly2 <- bind_rows(monopoly2, triples) 

monopoly3 <- 
  monopoly2 %>% 
  group_by(sum) %>% 
  summarize(count=n())

monopoly3$prob <- monopoly3$count/sum(monopoly3$count)
monopoly3$pct <- monopoly3$prob*100
temp_label <- "\U1F817"

monopoly3$labels <- c("Roll\n3 Doubles,\nGo to Jail\n↓", rep("", nrow(monopoly3)-1))

write_csv(monopoly3, "monopoly_roll_prob.csv")

p.rolls <- ggplot(monopoly3, aes(x=sum, y=prob, fill=labels))+
  geom_col(color="black")+
  xlab("Roll")+
  ylab("Probability")+
  scale_fill_manual(guide=FALSE, values=c("forestgreen", "#e41a1c"))+
  geom_text(aes(label=labels), nudge_y = 0.02)+
  scale_x_continuous(limits=c(-2, 36), breaks=seq(0, 35, 5))+
  theme_classic()+
  labs(title="Roll Probabilities in \"Monopoly\"")+
  theme(plot.title = element_text(size = 22, family="Arial Narrow", face="bold"),
        axis.title = element_text(size = 16), axis.text = element_text(size = 16))
  
p.rolls

p.rolls_high <- 
  ggplot(monopoly3, aes(x=sum, y=prob, fill=labels))+
  geom_col(color="black")+
  xlab("Roll")+
  ylab("Probability")+
  scale_fill_manual(guide=FALSE, values=c("forestgreen", "#e41a1c"))+
  geom_text(aes(label=labels), nudge_y = 0.02)+
  scale_x_continuous(limits=c(11, 36), breaks=seq(15, 35, 5))+
  scale_y_continuous(limits=c(NA, 0.02))+
  theme_classic()+
  theme(plot.title = element_text(size = 22, family="Arial Narrow", face="bold"),
        axis.title = element_text(size = 16), axis.text = element_text(size = 16))

p.rolls_high


ggsave("rolls.png", p.rolls, width=8, height=6, units="in")
ggsave("rolls_high.png", p.rolls_high, height=3, width=4, units="in")
