###ggplot
library(ggplot2)

ggplot(OrderedRAI, aes(x = OrderedRAI$Site, y = OrderedRAI$RAI, fill = OrderedRAI$Species, group = factor(OrderedRAI$Species))) +
  labs(x = "Site", y="RAI", fill = "Species", title = "Tapir Relative Abundance Index (RAI) by Site") +
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_manual(values = c("#B7E6A5", "#46AEA0", "#00718B", "#003147")) +  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, size = 10, vjust = 0.7), axis.text.y = element_text(size = 11), 
        axis.title.x = element_text(size = 15, vjust = 0.1), axis.title.y = element_text(size = 15, vjust = 0.1), 
        plot.title = element_text(hjust = 0.5, size = 20), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_line(lineend = 2),
        axis.ticks.length.x = unit(0.1, "inch"))+
  scale_x_discrete(limits = 
                     c("CBQTC","Kamuk","Tapanti","Villa Mills","PILA","Campanario","Copal","Chirripo","Savegre Valley",
                       "PN Carara","ASBC","Corumbiara","Guapore","Royal Belum","Temengor","Tabaconas"))
