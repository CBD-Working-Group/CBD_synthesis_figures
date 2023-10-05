

mydata<-read.csv("~/Documents/UBC/CBD/Synthesis_MS/figures/wos_sankey2.csv")

test<-mydata[,c(7,8,9,5,12)]

#library(remotes)
#remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggplot2)
library(dplyr)
df <- test %>%
  make_long("Data.source", "Ecosystem","Taxon", "Mechanisms.Identified", value = N) %>%
  filter(!is.na(node))
df

pl <- ggplot(df, aes(x = x
                     , next_x = next_x
                     , node = node
                     , next_node = next_node
                     , fill = factor(node)
                     , label = node)
)
pl <- pl +geom_sankey(flow.alpha = 0.7
                      , node.color = 1
                      , node.size = .2
                      ,show.legend = FALSE)
pl <- pl +geom_sankey_label(size = 3, color = "black", fill = "white", hjust = -0.1)
#pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank()
                  , panel.background = element_blank())

pl <- pl + scale_fill_viridis_d(option = "viridis")
#pl <- pl + labs(title = "Sankey diagram using ggplot")
#pl <- pl + labs(subtitle = "using ggsankey package")
#pl <- pl + labs(fill = 'Nodes')
pl


#Change all the forest types just to forest 
test2<-test
test2$Ecosystem<-gsub("tropical forest", "forest", test2$Ecosystem)
test2$Ecosystem<-gsub("mediterranean forest", "forest", test2$Ecosystem)
test2$Ecosystem<-gsub("temperate forest", "forest", test2$Ecosystem)
test2$Taxon<-gsub("parasitoid", "parasites", test2$Taxon)

df2 <- test2 %>%
  make_long("Data.source", "Ecosystem","Taxon", "Mechanisms.Identified", value = N) %>%
  filter(!is.na(node))
df2

pl <- ggplot(df2, aes(x = x
                     , next_x = next_x
                     , node = node
                     , next_node = next_node
                     , fill = factor(node)
                     , label = node)
)
pl <- pl +geom_sankey(flow.alpha = 0.7
                      , node.color = 1
                      , node.size = .2
                      ,show.legend = FALSE)
pl <- pl +geom_sankey_label(size = 3, color = "black", fill = "white", hjust = -0.1)
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank()
                  , panel.background = element_blank())

pl <- pl + scale_fill_viridis_d(option = "viridis")

pl



#Combine the mechanisms
test3<-test2

test3$Mechanisms.Identified<-gsub("temporal mismatch", "thermal mismatch", test3$Mechanisms.Identified)
test3$Mechanisms.Identified<-gsub("temperature", "gradual climate change", test3$Mechanisms.Identified)
test3$Mechanisms.Identified<-gsub("UV", "gradual climate change", test3$Mechanisms.Identified)
test3$Mechanisms.Identified<-gsub("precipitation", "gradual climate change", test3$Mechanisms.Identified)
test3$Mechanisms.Identified<-gsub("genetic diversity", "ecosystem function", test3$Mechanisms.Identified)

df3 <- test3 %>%
  make_long("Data.source", "Ecosystem","Taxon", "Mechanisms.Identified", value = N) %>%
  filter(!is.na(node))
df3

pl <- ggplot(df3, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      , label = node)
)
pl <- pl +geom_sankey(flow.alpha = 0.7
                      , node.color = 1
                      , node.size = .2
                      ,show.legend = FALSE)
pl <- pl +geom_sankey_label(size = 3, color = "black", fill = "white", hjust = -0.1)
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank()
                  , panel.background = element_blank())

pl <- pl + scale_fill_viridis_d(option = "virdis")

pl


##now only have 6 ecosystems
test4<-test3

test4$Ecosystem<-gsub("wetlands", "freshwater", test4$Ecosystem)
test4$Ecosystem<-gsub("forest", "terrestrial", test4$Ecosystem)
test4$Ecosystem<-gsub("grasslands", "terrestrial", test4$Ecosystem)
test4$Ecosystem<-gsub("taiga", "terrestrial", test4$Ecosystem)
test4$Ecosystem<-gsub("tundra", "terrestrial", test4$Ecosystem)
test4$Ecosystem<-gsub("mountain", "terrestrial", test4$Ecosystem)

df4 <- test4 %>%
  make_long("Data.source", "Ecosystem","Taxon", "Mechanisms.Identified", value = N) %>%
  filter(!is.na(node))
df4

pl <- ggplot(df4, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      , label = node)
)
pl <- pl +geom_sankey(node.color = 1
                      , node.size = .3
                      , space = 15
                      , alpha = 0.8
                      ,show.legend = FALSE)
#pl <- pl +geom_sankey_label(size = 3, color = "black", fill = "white")
pl <- pl + theme(legend.position = "none"
                 ,axis.title = element_blank()
                 , axis.text.y = element_blank()
                 , axis.ticks = element_blank()  
                 , panel.grid = element_blank()
                 , panel.background = element_blank())
#pl <- pl + scale_fill_viridis_d(option = "viridis")
#pl <- pl + scale_fill_manual(values=colors_to_use)
pl<-pl + scale_fill_manual(values = c('theory'= '#CCAA7A'
                                        , 'literature' = '#084C92'
                                        , 'empirical' = '#990F26'
                                        , 'terrestrial' = '#54990F'
                                        , 'marine' = '#90298F'
                                        , 'in silico' = '#D87B37'
                                        , 'freshwater' = '#A3C6E5'
                                        , 'anthropogenic' = '#99600F'
                                        , 'agriculture' = '#333333'
                                        , 'viruses' = '#99C9B8'
                                        , 'vectors' = '#CC7A88'
                                        , 'pollinators' = '#7ABECC'
                                        , 'plants' = '#CA8DC6'
                                        , 'parasites' = '#E89D5E'
                                        , 'microbiome' = '#6C97CB'
                                        , 'invertebrates' = '#A3CC7A'
                                        , 'insects' = '#E6B8BF'
                                        , 'fungi' = '#967ACC'
                                        , 'fish' = '#79CBB0'
                                        , 'birds' = '#999999'
                                        , 'animals' =  '#B33E52'
                                        , 'amphibians' = '#AE55B1'
                                        , 'vector abundance' = '#3E9FB3'
                                        , 'urbanization / deforestation' = '#E57D32'
                                        , 'thermal mismatch' = '#78B33E'
                                        , 'species introductions' = '#496BAD'
                                        , 'species decline' = '#B3823E'
                                        , 'reservoir population' = '#653EB3'
                                        , 'range shifts' = '#3D0F99'
                                        , 'physiology' = '#349B89'
                                        , 'parasite-mediated competition' = '#B8DEE6'
                                        , 'migration' = '#0F8299'
                                        , 'invasive species' = '#E5BDDF'
                                        , 'gradual climate change' = '#CCCCCC'
                                        , 'food web dynamics' = '#C7B8E6'
                                        , 'ecosystem function' = '#CFE6B8'
                                        , 'dilution / amplification' = '#E6D2B8'
                                        , 'climatic pulse events' = '#50BFA9'
                                        , 'behavior' = '#666666'
                                        , 'anthropogenic factors' = '#EEB976'))

pl

ggsave("sankey_WOS_final_oct2023.pdf", width = 16, height = 8, dpi = 300)

#### to color pathways
set.seed(856)

library(pals) #has palette36, stepped

as.vector(stepped(24))->ll
ll
colors_to_use<- c("#990F26", "#B33E52", "#CC7A88", "#E6B8BF", 
                  "#0F8299", "#3E9FB3", "#7ABECC", "#B8DEE6",
                  "#90298F", "#AE55B1", "#CA8DC6","#E5BDDF",
                  "#D87B37","#E57D32", "#E89D5E", "#EEB976",
                  "#54990F", "#78B33E", "#A3CC7A", "#CFE6B8",
                  "#084C92","#496BAD", "#6C97CB", "#A3C6E5",
                  "#99600F", "#B3823E", "#CCAA7A", "#E6D2B8",
                  "#3D0F99", "#653EB3", "#967ACC", "#C7B8E6",
                  "#349B89", "#50BFA9", "#79CBB0", "#99C9B8",
                  "#333333", "#666666", "#999999", "#CCCCCC")

pie(rep(1, 40), col=colors_to_use)

