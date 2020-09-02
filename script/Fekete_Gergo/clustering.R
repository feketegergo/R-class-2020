
library(tidyverse)

file1<-"data/clustering/calmorph_data-step1-outlier_images_filtered-data_A.rds"


tbl1<-read_rds(file1)
tbl1<-tbl1 %>%  filter(is_ok)

# kivalasztok 5 random genotipust, es csak azokat hasznalom
random_genotype_id_list<-sample(unique(tbl1$genotypeID), 5)   # igy kell listából mintát venni
tbl1<-tbl1 %>%  filter(genotypeID %in% random_genotype_id_list)


tbl1<-tbl1 %>%  sample_n(size=150,replace = FALSE)  # igy kell tibble-ből mintát venni


tbl1<-tbl1 %>% select(genotypeID,grep("^C\\d.*",names(tbl1),value = TRUE)) # azon oszlopnevek, amiket használni akarok

variable_column_names<-grep("^C\\d.*",names(tbl1),value = TRUE)

tbl1<- tbl1 %>% mutate_at(.vars =variable_column_names, .funs = function(x) { scale(x, center = TRUE, scale = TRUE)} )

# ellenorzom, h a szoras=0 és atlag=0 minden oszlopban
tbl1 %>% summarise_at(.vars =variable_column_names, .funs = mean)
tbl1 %>% summarise_at(.vars =variable_column_names, .funs = sd)


mx1<-as.matrix(tbl1 %>% select(-genotypeID)) # ha a genotypeID oszlopot nem venném ki, akkor nem csak szamok lennének benne, nem tudna matrix lenni
rownames(mx1) <- sprintf("cell_%05i", 1:nrow(mx1))

# ha ez működik, akkor megold mindent. Sajnos nem biztos, h telepthető a gplots package
gplots::heatmap.2(mx1, trace="none")
# a trace="none" arra való, hogy egy teljesen felesleges kék vonalakkal ne rakja tele az ábrát


my_dist_1 <- dist(mx1, method = "euclidean") 

#A my_dist_1 egy distance objektum, ami használható, de ahhoz, h ránézzünk matrixá kell alakítani:
as.matrix(my_dist_1) %>% View()

# Nézzünk rá a távolságmátrixra, már ha megy a gplots package
gplots::heatmap.2(as.matrix(my_dist_1), trace="none",Rowv=FALSE, Colv=FALSE) # megtiltom a clusterezést
gplots::heatmap.2(as.matrix(my_dist_1), trace="none")

# tipikus hiba  a  távoilságmátrix klaszterezését összekeverni az eredeti adatok klaszterezésével.



# ha nem menne a heatmap.2() akor nézzük hogy lehet a heatmap-et kirajzolni:
dist_matrix<-as.matrix(my_dist_1) 

dist_as_long_format_df<-reshape2::melt(dist_matrix)
dist_as_long_format_df<- dist_as_long_format_df %>%
  as.tibble() %>% 
  mutate(Var1=as.character(Var1), Var2=as.character(Var2)) %>% 
  rename(cell_X=Var1, cell_Y=Var2, dist=value)

dist_as_long_format_df %>%
  ggplot(mapping = aes(x=cell_X, y=cell_Y, fill=dist))+  geom_tile()
# ez zavaros, mert nincsenek rendszve a sorok és oszlopok
# rendezzük!


clustering_object1<-hclust(my_dist_1)
orderer_labels1<-clustering_object1$labels[clustering_object1$order] # igy kell kiszedni a klaszterezés objektumból a sorrendet

dist_as_long_format_df %>%
  ggplot(mapping = aes(x=cell_X, y=cell_Y, fill=dist))+
  geom_tile()+
  scale_x_discrete(limits=orderer_labels1)+
  scale_y_discrete(limits=orderer_labels1)


# ok még állítgassunk a kinézetén
dist_as_long_format_df %>%
  ggplot(mapping = aes(x=cell_X, y=cell_Y, fill=dist))+
  geom_tile()+
  scale_x_discrete(limits=orderer_labels1)+
  scale_y_discrete(limits=orderer_labels1)+
  #scale_fill_continuous(trans="log")+
  scale_fill_viridis_c(trans="log10")+
  coord_equal()+
  theme(axis.text.x = element_text(angle = 90), axis.title = element_blank(), legend.position = "top")


# nézzünk rá a klaszter dendogrammra:
plot(clustering_object1)
rect.hclust(clustering_object1, k=5, border="red") 

# ezt ggplot-tal macerás kirajzolni sajnos, de majd tanuljuk

#############################################################################
######### t-SNE

library(Rtsne)

res1<-Rtsne(dist_matrix, is_distance = TRUE)
tbl6<-tibble(name=rownames(dist_matrix), tnse_coord1=res1$Y[,1], tnse_coord2=res1$Y[,2])
# tbl6<-tbl6 %>% left_join(tbl3_ext, by = "name")
# tbl6<-tbl6 %>%  mutate(cluster_label=paste0("c",clusterID));
# tbl6<-tbl6 %>%  mutate(supertype=case_when(genotype=="YOR202W_ancestor"~"WT-anc" ,
#                                            genotype=="YOR202W_evolved_1"~"WT-evo1",
#                                            genotype=="YOR202W_evolved_2"~"WT-evo2",
#                                            orf=="YOR202W"~"WT-evo", TRUE~"else"))


  tbl6 %>%  ggplot(aes(x=tnse_coord1, y=tnse_coord2))+
  geom_text(mapping = aes(label=name), size=3, color="gray")+
  geom_point()+
  labs(title="t-SNE (Stochastic Neighbor Embedding)")
#theme(legend.position = "none")



