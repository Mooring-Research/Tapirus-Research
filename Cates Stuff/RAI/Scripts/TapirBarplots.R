###Making bar plots for tapir abundance by species and site###
library(ggplot2)

'#Making bar plot for single species
b_table <- table(bairdssubm$Site)


barplot(b_table,
        main = "Baird's barchart",
        xlab = "Site",
        ylab= "Abundance",
        col = c("pink", "turquoise", "green", "blue", "purple", "orange", "yellow", "grey", "brown", "gold", "navy"))


ggplot(data = bairdssubm, aes(x= Site, fill = Site))+
  geom_bar(aes(col = c("#000000", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")))'

###
'RAItable <- table(AllRAI$RAI)
barplot(AllRAI$RAI, names = AllRAI$Site, xlab = 'Site', ylab = 'RAI'
        main = "RAI barchart",
        xlab = "Site",
        ylab= "RAI")

ggplot(data = AllRAI, aes(x= AllRAI$Site, fill = AllRAI$RAI))

ggplot(data = AllRAI, aes(x=Site, y=RAI, fill = Site))+
  geom_boxplot()+
  scale_fill_brewer(palette = "RdBu")

ggplot(data = AllRAI, aes(x=RAI, y=Site, fill = RAI))+
  geom_boxplot()+
  scale_fill_brewer(palette = "RdBu")

###Barplot for all RAI values and sites###
barplot(AllRAI$RAI, names = AllRAI$Site, 
        main = "Tapir RAI by Site",
        xlab = 'Site', 
        ylab = 'RAI',
        ylim = c(0, 200),
        cex.names = 0.7,
        col = c("blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","lightblue","lightblue","darkorchid","darkorchid","deeppink4"))
#Add a legend
legend(x = "top", 
       legend = c("Baird's", "Lowland", "Malayan", "Mountain"),
       box.lty = c(1,1),
       col = c("blue", "lightblue", "darkorchid", "deeppink4"))

legend("topright", title="Species",
       c("Baird's", "Lowland", "Malayan", "Mountain"), 
       fill= c("blue", "lightblue", "darkorchid", "deeppink4"), 
       horiz=FALSE, cex=0.8)'

###IN BASE R
barplot(OrderedRAI$RAI, names = OrderedRAI$Site, 
        main = "Tapir RAI by Site",
        xlab = 'Site', 
        ylab = 'RAI',
        ylim = c(0, 200),
        cex.names = 0.7,
        col = c("#B7E6A5","#B7E6A5","#B7E6A5","#B7E6A5","#B7E6A5","#B7E6A5","#B7E6A5","#B7E6A5","#B7E6A5","#B7E6A5","#B7E6A5", "#46AEA0","#46AEA0", "#00718B", "#00718B", "#003147"))

legend("topright", title="Species",
       c("Baird's", "Lowland", "Malayan", "Mountain"), 
       fill= c("#B7E6A5", "#46AEA0", "#00718B", "#003147"), 
       horiz=FALSE, cex=1.5)


###Add a new column "species" to dataframe
AllRAI["species"] <- rep(NA, length(16))
AllRAI$species[1:11] <- "Baird's"
AllRAI$species[12:13] <- "Lowland"
AllRAI$species[14:15] <- "Malayan"
AllRAI$species[16] <- "Mountain"

###ggplot
library(ggplot2)

ggplot(OrderedRAI, aes(x = OrderedRAI$Site, y = OrderedRAI$RAI, fill = OrderedRAI$Species, group = factor(OrderedRAI$Species))) +
  labs(x = "Site", y="RAI", fill = "Species", title = "Tapir RAI by Site") +
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_manual(values = c("#B7E6A5", "#46AEA0", "#00718B", "#003147")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, size = 15), axis.text.y = element_text(size = 11), 
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, size = 20), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15)) + 
  scale_x_discrete(limits = 
                     c("CBQTC","Kamuk","Tapanti","Villa Mills","PILA","Campanario","Copal","Chirripo","Savegre Valley",
                       "PN Carara","ASBC","Corumbiara","Guapore","Royal Belum","Temengor","Tabaconas"))+
  opts(panel.background = theme_rect(fill='white', colour='black')) + opts(panel.grid.major = none, panel.grid.minor = none) + 
  opts(panel.background=theme_rect(colour=NA))


panel.background = element_rect(fill = 'white', color = 'white')

AllRAI$species <- factor(RAI$species, levels = c("Baird's","Lowland","Malayan","Mountain"))

barplot(AllRAI$RAI ~ AllRAI$species)

AllRAI$Site[16] <- "Tabaconas"
AllRAI$Site[15] <- "Temengor"

scale_x_discrete()