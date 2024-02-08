library(aplot)
library(ggplot2)
library(cowplot)
library(ape)
library(ggtree)
#library(ggtreeDendro)
library(treeio)
library(dplyr)
library(tidytree)
library(data.table)
library(gridExtra)

####################################
#    DEFINE FUNCTIONS 
####################################

# groupLineages <- function(tree, metadata) {
#   LINEAGES = unique(metadata$lineage)
#   LISTS = list()
#   for (L in LINEAGES){
#     print(L)
#     MDATA = metadata[metadata$lineage == L]
#     
#     #LISTS = append(LISTS, list(L = c(MDATA$sample)))
#     LISTS = append(LISTS, list(assign(L, c(MDATA$sample))))
#     
#   }
#   
#   print("FINISHED")
#   return(LISTS)
# }
groupLineages <- function(tree, metadata) {
  LINEAGES = unique(metadata$lineage)
  LISTS = list()
  for (L in LINEAGES){
    print(L)
    MDATA = metadata[metadata$lineage == L, ]
    LISTS[[L]] <- MDATA$sample
  }
  
  print("FINISHED")
  return(LISTS)
}
# addGeomCountry <- function(tree, lineage_COLOUR, country_name, metadata) {
#   SUBDATA = metadata[metadata$country == country_name]
#   LINEAGES = unique(SUBDATA$lineage)
#   for (L in names(lineage_COLOUR)){
#     if (L %in% LINEAGES){
#       print(L)
#       print(lineage_COLOUR[L])
#       SUBDATA2 = SUBDATA[SUBDATA$lineage == L]
#       IDS = SUBDATA2$ID
#       #print(IDS)
#       tree <- tree + geom_point2(aes(subset = ID %in% IDS), shape=21, fill=lineage_COLOUR[L], colour="black", stroke=0.1, size=3.75)
#     }
#     
#   }
#   
#   print("FINISHED")
#   return(tree)
# }

addGeomCountry <- function(tree, lineage_COLOUR, country_name, metadata) {
  SUBDATA = metadata[metadata$country == country_name, ]
  LINEAGES = unique(SUBDATA$lineage)
  for (L in names(lineage_COLOUR)){
    if (L %in% LINEAGES){
      print(L)
      print(lineage_COLOUR[L])
      SUBDATA2 = SUBDATA[SUBDATA$lineage == L, ]
      IDS = SUBDATA2$ID
      tree = tree + geom_point2(aes(subset = ID %in% IDS), shape = 21, fill = lineage_COLOUR[L], colour = "black", stroke = 0.1, size = 3.75)
      #tree = tree +aux
      plot(tree)
    }
  }
  
  print("FINISHED")
  return(tree)
}

####################################
#    DEFINE GLOBAL VARIABLES
####################################

setwd("~/Documents/prj_influenza/PAPER/TREE/influenzaB/")

Sys.setlocale("LC_TIME", "C")

# Configuração para ignorar comprimentos de ramo negativos
options(ignore.negative.edge = TRUE)

COLOURS = c("V1A" = "#8dd3c7", "V1A.3" = "#e78ac3", "V1A.3a.1" = "#bebada", "V1A.3a" = "#fb8072", "V1A.3a.2" = "#80b1d3")

####################################
#    READ TREE and DATA 
####################################

MCCtree <- read.nexus(".")
MDATA <- read.delim2("./metadata3.txt")
TIPLABELS = MCCtree$tip.label
MDATA2 = setDT(MDATA)[J(TIPLABELS), on=.(sample), nomatch = 0]
MDATA2$collection.date <- as.Date(as.character(MDATA2$collection.date), format = "%Y-%m-%d")

DecDATE = Date2decimal(MDATA2$collection.date)

COUNTRY = MDATA2$country
OTHERS = COUNTRY[COUNTRY != "Brazil"]
BRAZIL = COUNTRY[COUNTRY == "Brazil"]
STUDY = MDATA2$study[MDATA2$study != ""]
VACCINE = MDATA2$ID[MDATA2$vaccine == "true"]


timeline_data <- data.frame(
  date = MDATA2$collection.date
)


GROUPS <- groupLineages(MCCtree, MDATA2)
names(GROUPS) <- unique(MDATA2$lineage)

MCCtree = groupOTU(MCCtree, GROUPS, 'subclades')

lineage_COLOUR = c("V1A" = "#8dd3c7", "V1A.3" = "#e78ac3", "V1A.3a.1" = "#bebada", "V1A.3a" = "#fb8072", "V1A.3a.2" = "#80b1d3")
lineage_NAMES = names(lineage_COLOUR)


SUBDATA = MDATA2[MDATA2$country == "Brazil", ]


SUBDATA2 = SUBDATA[SUBDATA$lineage == lineage_NAMES[1], ]
IDS2 = SUBDATA2$ID
SUBDATA3 = SUBDATA[SUBDATA$lineage == lineage_NAMES[2], ]
IDS3 = SUBDATA3$ID
SUBDATA4 = SUBDATA[SUBDATA$lineage == lineage_NAMES[3], ]
IDS4 = SUBDATA4$ID
SUBDATA5 = SUBDATA[SUBDATA$lineage == lineage_NAMES[4], ]
IDS5 = SUBDATA5$ID
SUBDATA6 = SUBDATA[SUBDATA$lineage == lineage_NAMES[5], ]
IDS6 = SUBDATA6$ID

p1 <- ggtree(MCCtree,  mrsd = decimal2Date(max(DecDATE)), aes(color=subclades), as.Date=TRUE, size=0.25, alpha=0.75)%<+% MDATA2 +
  geom_point2(aes(subset = ID %in% IDS2, label="label1"), shape = 21, fill = lineage_COLOUR[lineage_NAMES[1]], colour = "black", stroke = 0.1, size = 2.75)+
  geom_point2(aes(subset = label %in% SUBDATA2$study), fill = lineage_COLOUR[lineage_NAMES[1]], color="red", shape=21, size=3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS3), shape = 21, fill = lineage_COLOUR[lineage_NAMES[2]], colour = "black", stroke = 0.1, size = 2.75)+
  geom_point2(aes(subset = label %in% SUBDATA3$study), fill = lineage_COLOUR[lineage_NAMES[2]], color="red", shape=21, size=2.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS4), shape = 21, fill = lineage_COLOUR[lineage_NAMES[3]], colour = "black", stroke = 0.1, size = 2.75)+
  geom_point2(aes(subset = label %in% SUBDATA4$study), fill = lineage_COLOUR[lineage_NAMES[3]], color="red", shape=21, size=3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS5), shape = 21, fill = lineage_COLOUR[lineage_NAMES[4]], colour = "black", stroke = 0.1, size = 2.75)+
  geom_point2(aes(subset = label %in% SUBDATA5$study), fill = lineage_COLOUR[lineage_NAMES[4]], color="red", shape=21, size=2.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS6), shape = 21, fill = lineage_COLOUR[lineage_NAMES[5]], colour = "black", stroke = 0.1, size = 2.75)+
  geom_point2(aes(subset = label %in% SUBDATA6$study), fill = lineage_COLOUR[lineage_NAMES[5]], color="red", shape=21, size=2.75, stroke=0.25, alpha=1)+
  theme_dendrogram()+
  #theme_tree2()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  geom_hline(yintercept = -40)+
  geom_point2(aes(subset = ID %in% VACCINE, color=subclades), colour="black", shape="*", size=4, stroke=1, alpha=1)+
  geom_tiplab(aes(subset = ID %in% VACCINE, color=subclades), colour="black", size=3, stroke=2, alpha=1, angle=120)+
  scale_color_manual(values= lineage_COLOUR)+
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 0.5, vjust = 0.5), legend.text = element_text(size = 8), plot.margin = margin(t = 0.5, r = 1, b = 0.5, l = 1, unit = "cm"))+
  xlab("Date")+
  #labs(shape="TEST")+
  expand_limits(y = length(TIPLABELS)+100)

pdf("./influenzaB/OUTPUT2.pdf")
#ggplotify::as.ggplot(p1, angle=0)
plot(p1)
dev.off()