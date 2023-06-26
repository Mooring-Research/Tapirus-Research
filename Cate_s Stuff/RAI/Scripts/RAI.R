###For relative abundance index (RAI)###

#Create subset for each species and site and determine number of independent records

#Baird's (Costa Rica)
#Get only independent records for Baird's by creating a new subset
indsub.bairds <- subset(bairdssubm, bairdssubm$Independent == "Yes")
View(indsub.bairds)

ASBCsub<-subset(indsub.bairds, indsub.bairds$Site == "ASBC")
View(ASBCsub)
nrow(ASBCsub) #1

CBQTCsub<-subset(indsub.bairds, indsub.bairds$Site == "CBQTC")
View(CBQTCsub)
nrow(CBQTCsub) #88

Chirriposub<- subset(indsub.bairds, indsub.bairds$Site == "Chirripo")
nrow(Chirriposub) #132

Copalsub <- subset(indsub.bairds, indsub.bairds$Site == "Copal")
nrow(Copalsub) #18

Kamuksub <- subset(indsub.bairds, indsub.bairds$Site == "Kamuk")
nrow(Kamuksub) #840

Osasub <- subset(indsub.bairds, indsub.bairds$Site == "Osa Campanario")
nrow(Osasub) #49

PILAsub <- subset(indsub.bairds, indsub.bairds$Site == "PILA")
nrow(PILAsub) #154

Cararasub <- subset(indsub.bairds, indsub.bairds$Site == "PN Carara")
nrow(Cararasub) #5

Savegresub <- subset(indsub.bairds, indsub.bairds$Site == "Savegre Valley")
nrow(Savegresub) #393

Tapantisub <- subset(indsub.bairds, indsub.bairds$Site == "Tapanti")
nrow(Tapantisub) #851

Villasub <- subset(indsub.bairds, indsub.bairds$Site == "Villa Mills")
nrow(Villasub) #48


#Lowland (Brazil)
Corumbiarasub <- subset(lowsub, lowsub$survey == "Corumbiara")
nrow(Corumbiarasub) #37

Guaporesub <- subset(lowsub, lowsub$survey == "Guapore")
nrow(Guaporesub) #34


#Malayan (Malaysia)
Belumsub <- subset(MalayanCirc, MalayanCirc$Site == "Royal Belum State Park")
nrow(Belumsub) #464

Temengorsub <- subset(MalayanCirc, MalayanCirc$Site == "Temengor Forest Reserve")
nrow(Temengorsub) #345


#Mountain (Peru) -- there's only one site for these guys
nrow(recs_all_MountainTapir$station) #95
