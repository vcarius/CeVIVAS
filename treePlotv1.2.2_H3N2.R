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

setwd("~/Documents/Artigo_1_Influenza/H3N2/R_tree")

Sys.setlocale("LC_TIME", "C")

# Configuração para ignorar comprimentos de ramo negativos
options(ignore.negative.edge = TRUE)

COLOURS = c("3C.2a1b.2a.1" = "#E31A1C", "3C.2a1b.2a.1a.1" = "#E6AB02", "3C.2a1b.2a.2" = "#bebada", 
            "3C.2a1b.2a.2c" = "#e78ac3", "3C.2a1b.2a.2b" = "#FF7F00", "3C.2a1b.2a.2a" = "#33A02C", 
            "3C.2a1b.2a.2a.2" = "#6A3D9A", "3C.2a1b.2a.2a.1" = "#8dd3c7", "3C.2a1b.2a.2a.1a" = "#FB9A99", 
            "3C.2a1b.2a.2a.1b" = "#B2DF8A", "3C.2a1b.2a.2a.3" = "#80b1d3", "3C.2a1b.2a.2a.3b" = "#B15928", "3C.2a1b.2a.2a.3a" = "#1F78B4", "3C.2a1b.2a.2a.3a.1" = "#E7298A")

####################################
#    READ TREE and DATA 
####################################

MCCtree <- read.nexus("./nextstrain__timetree_h3n2.nexus")
MDATA <- read.delim2("./metadata_h3n2.txt")
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

lineage_COLOUR = c("3C.2a1b.2a.1" = "#E31A1C", "3C.2a1b.2a.1a.1" = "#bebada", "3C.2a1b.2a.2" = "navy", 
                   "3C.2a1b.2a.2c" = "#e78ac3", "3C.2a1b.2a.2b" = "grey", "3C.2a1b.2a.2a" = "#33A02C", 
                   "3C.2a1b.2a.2a.2" = "#6A3D9A", "3C.2a1b.2a.2a.1" = "#8dd3c7", "3C.2a1b.2a.2a.1a" = "#FB9A99", 
                   "3C.2a1b.2a.2a.1b" = "#1F78B4", "3C.2a1b.2a.2a.3" = "#80b1d3", "3C.2a1b.2a.2a.3b" = "#B15928", "3C.2a1b.2a.2a.3a" = "#B2DF8A", "3C.2a1b.2a.2a.3a.1" = "pink3",
                   "3C.2a1b.1b"="#E6AB02", "3C.2a1b.1a"="turquoise4")
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
SUBDATA7 = SUBDATA[SUBDATA$lineage == lineage_NAMES[6], ]
IDS7 = SUBDATA7$ID
SUBDATA8 = SUBDATA[SUBDATA$lineage == lineage_NAMES[7], ]
IDS8 = SUBDATA8$ID
SUBDATA9 = SUBDATA[SUBDATA$lineage == lineage_NAMES[8], ]
IDS9 = SUBDATA9$ID
SUBDATA10 = SUBDATA[SUBDATA$lineage == lineage_NAMES[9], ]
IDS10 = SUBDATA10$ID
SUBDATA11 = SUBDATA[SUBDATA$lineage == lineage_NAMES[10], ]
IDS11 = SUBDATA11$ID
SUBDATA12 = SUBDATA[SUBDATA$lineage == lineage_NAMES[11], ]
IDS12 = SUBDATA12$ID
SUBDATA13 = SUBDATA[SUBDATA$lineage == lineage_NAMES[12], ]
IDS13 = SUBDATA13$ID
SUBDATA14 = SUBDATA[SUBDATA$lineage == lineage_NAMES[13], ]
IDS14 = SUBDATA14$ID
SUBDATA15 = SUBDATA[SUBDATA$lineage == lineage_NAMES[14], ]
IDS15 = SUBDATA15$ID


p1 <- ggtree(MCCtree,  mrsd = decimal2Date(max(DecDATE)), aes(color=subclades), as.Date=TRUE, size=0.75, alpha=0.75)%<+% MDATA2 +
  geom_point2(aes(subset = ID %in% IDS2, label="label1"), shape = 21, fill = lineage_COLOUR[lineage_NAMES[1]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA2$study), fill = lineage_COLOUR[lineage_NAMES[1]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS3), shape = 21, fill = lineage_COLOUR[lineage_NAMES[2]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA3$study), fill = lineage_COLOUR[lineage_NAMES[2]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS4), shape = 21, fill = lineage_COLOUR[lineage_NAMES[3]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA4$study), fill = lineage_COLOUR[lineage_NAMES[3]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS5), shape = 21, fill = lineage_COLOUR[lineage_NAMES[4]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA5$study), fill = lineage_COLOUR[lineage_NAMES[4]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS6), shape = 21, fill = lineage_COLOUR[lineage_NAMES[5]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA6$study), fill = lineage_COLOUR[lineage_NAMES[5]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS7), shape = 21, fill = lineage_COLOUR[lineage_NAMES[6]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA7$study), fill = lineage_COLOUR[lineage_NAMES[6]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS8), shape = 21, fill = lineage_COLOUR[lineage_NAMES[7]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA8$study), fill = lineage_COLOUR[lineage_NAMES[7]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS9), shape = 21, fill = lineage_COLOUR[lineage_NAMES[8]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA9$study), fill = lineage_COLOUR[lineage_NAMES[8]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS10), shape = 21, fill = lineage_COLOUR[lineage_NAMES[9]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA10$study), fill = lineage_COLOUR[lineage_NAMES[9]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS11), shape = 21, fill = lineage_COLOUR[lineage_NAMES[10]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA11$study), fill = lineage_COLOUR[lineage_NAMES[10]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS12), shape = 21, fill = lineage_COLOUR[lineage_NAMES[11]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA12$study), fill = lineage_COLOUR[lineage_NAMES[11]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS13), shape = 21, fill = lineage_COLOUR[lineage_NAMES[12]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA13$study), fill = lineage_COLOUR[lineage_NAMES[12]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS14), shape = 21, fill = lineage_COLOUR[lineage_NAMES[13]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA14$study), fill = lineage_COLOUR[lineage_NAMES[13]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  geom_point2(aes(subset = ID %in% IDS15), shape = 21, fill = lineage_COLOUR[lineage_NAMES[14]], colour = "black", stroke = 0.1, size = 3.75)+
  geom_point2(aes(subset = label %in% SUBDATA15$study), fill = lineage_COLOUR[lineage_NAMES[14]], color="red", shape=21, size = 3.75, stroke=0.25, alpha=1)+
  
  theme_dendrogram()+
  #theme_tree2()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  geom_hline(yintercept = -100)+
  geom_point2(aes(subset = ID %in% VACCINE, color=subclades), colour="black", shape="*", size=4, stroke=1, alpha=1)+
  geom_tiplab(aes(subset = ID %in% VACCINE, color=subclades), colour="black", size=3, stroke=2, alpha=1, angle=0)+
  scale_color_manual(values= lineage_COLOUR)+
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 0.5, vjust = 0.5), legend.text = element_text(size = 8), plot.margin = margin(t = 0.5, r = 1, b = 0.5, l = 1, unit = "cm"))+
  xlab("Date")+
  #labs(shape="TEST")+
  expand_limits(y = length(TIPLABELS)+100)

#pdf("./OUTPUT2.pdf")
#ggplotify::as.ggplot(p1, angle=0)
plot(p1)
#dev.off()
ggsave("h3n2_tree.pdf", plot= p1, dpi=300 , height = 10, width = 20, limitsize = FALSE)