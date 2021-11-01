#Simple plot of communities

#create some data

Site<-c("A", "B", "C", "D", "E", "F", "G", "H")
Species1<-c(2, 3, 2, 1, 10, 11, 9, 10)
Species2<-c(10, 8, 9, 9, 2, 2, 3, 4)

Communities<-data.frame(cbind(Site, Species1, Species2))

library(ggplot2)
ggplot(Communities, aes(Species1, Species2))+
  geom_point()

#strange portrayal of x and y axis! Try making Species 1 and species 2 integers

Communities$Species1<-as.integer(Communities$Species1)
Communities$Species2<-as.integer(Communities$Species2)

Communities$Group<-c(1, 1, 1, 1, 2, 2, 2, 2)
Communities$Group<-as.factor(Communities$Group)
ggplot(Communities, aes(Species1, Species2, color = Group))+
  geom_point(size = 4, show.legend = F)+
  theme_bw()+
  labs(x = "Number of species 1", y = "Number of species 2")+
  theme(text = element_text(size = 20))

ggsave("Ordination.png", device = "png")
