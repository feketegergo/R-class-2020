
library(tidyverse)

file1<-"/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/mater-BRANCH/Morphology-2018-Karcsi/data/GLMM-normalisation/cell_level_normalisation/calmorph_data-normalised-formula3-cell_level-use_for_paper-A.rds"


tbl1<-read_rds(file1)
tbl1<-tbl1 %>%  filter(is_ok)
tbl1<-tbl1 %>%  sample_n(size=150,replace = FALSE)

tbl1<-tbl1 %>% select(genotypeID,grep("^C\\d.*",names(tbl1),value = TRUE))

variable_column_names<-grep("^C\\d.*",names(tbl1),value = TRUE)

tbl1<- tbl1 %>% mutate_at(.vars =variable_column_names, .funs = function(x) { scale(x, center = TRUE, scale = TRUE)} )

tbl1 %>% summarise_at(.vars =variable_column_names, .funs = mean)
tbl1 %>% summarise_at(.vars =variable_column_names, .funs = sd)


mx1<-as.matrix(tbl1 %>% select(-1))
rownames(mx1) <- sprintf("cell_%05i", 1:nrow(mx1))

gplots::heatmap.2(mx1, trace="none")


#my_dist_1 <- dist(tbl1, method = "euclidean")
my_dist_1 <- dist(mx1, method = "euclidean")

as.matrix(my_dist_1) %>% View()

gplots::heatmap.2(as.matrix(my_dist_1), trace="none",Rowv=FALSE, Colv=FALSE) # megtiltom a clusterezést

gplots::heatmap.2(as.matrix(my_dist_1), trace="none")

# tipikus hiba  a  távoilságmátrix klaszterezését összekeverni az eredeti adatok klaszterezésével.
