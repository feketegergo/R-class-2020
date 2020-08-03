
library("tidyverse")

rm(list=ls())

mean_NT<-1.2
sigma_NT<-0.15

mean_AMP<-0.25
sigma_AMP<-0.8

N_noEffect<-4000
N_effect<-400

od_NT<-rnorm(10,mean = mean_NT, sd = sigma_NT)

tbl1<-tibble( experiment_id=character(),p=numeric(), type=character())
for( i in 1:N_noEffect)
{
#  od_NT<-rnorm(4,mean = mean_NT, sd = sigma_NT)
  od_AMP<-rnorm(5,mean = mean_NT, sd = sigma_NT)
  
  result_of_t_test<-t.test(od_AMP,od_NT,alternative =  "less")
  tbl1<-tbl1 %>%  add_row(experiment_id=sprintf("exp_%03i",i),p=result_of_t_test$p.value, type="no-effect" )
}

for( i in N_noEffect+(1:N_effect))
{
 # od_NT<-rnorm(4,mean = mean_NT, sd = sigma_NT)
  od_AMP<-rnorm(5,mean = mean_AMP, sd = sigma_AMP)
  
  result_of_t_test<-t.test(od_AMP,od_NT,alternative =  "less")
  tbl1<-tbl1 %>%  add_row(experiment_id=sprintf("exp_%03i",i),p=result_of_t_test$p.value, type="operative" )
}


tbl1 %>% ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.05)) +
  geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
  labs(title = "Histogram finom nagy bin-ekkel")

tbl1 %>%  ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.01)) +
  geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
  labs(title = "Histogram finom bin felosztassal")

tbl1 %>%  ggplot(aes(x=p)) +
  geom_abline(color="blue", linetype="dotted")+stat_ecdf() +
  labs(title = "CDF of p-values (Cumulative distribution function)")


tbl1<-tbl1 %>% 
  dplyr::mutate(p_fdr=p.adjust(p, method = "fdr")) %>%
  mutate(significant=ifelse(p<0.05,"significant","non-significant")) %>%
  mutate(significant_fdr=ifelse(p_fdr<0.05,"significant_after_fdr","non-significant_after_fdr"))

tbl1<-tbl1 %>% 
  mutate(ok=case_when(
    (significant=="significant" &  type=="operative") ~ "True-Positive",
    (significant=="significant" &  type=="no-effect") ~ "False-Positive",
    (significant=="non-significant" &  type=="operative") ~ "False-Negative",
    (significant=="non-significant" &  type=="no-effect") ~ "True-Negative"))


tbl1<-tbl1 %>% 
  mutate(ok_fdr=case_when(
    (significant_fdr=="significant_after_fdr" &  type=="operative") ~ "True-Positive",
    (significant_fdr=="significant_after_fdr" &  type=="no-effect") ~ "False-Positive",
    (significant_fdr=="non-significant_after_fdr" &  type=="operative") ~ "False-Negative",
    (significant_fdr=="non-significant_after_fdr" &  type=="no-effect") ~ "True-Negative"))

my_colors_1<-c(
  "True-Negative"="blue",
  "False-Positive"="red",
  "True-Positive"="darkblue",
  "False-Negative"="purple"
)
# 
# cowplot::plot_grid(
#   
#   tbl1 %>% ggplot()+
#     geom_mosaic(aes(x = product(significant, type ), fill=ok), na.rm=TRUE)+
#     coord_equal()+
#     scale_fill_manual(values = my_colors_1),
#   
#   tbl1 %>% ggplot()+
#     geom_mosaic(aes(x = product(type,significant_fdr ), fill=ok_fdr), na.rm=TRUE)+
#     coord_equal()+
#     scale_fill_manual(values = my_colors_1)+
#     labs(x="dd"),
#   
#   
#   ncol=2)


cowplot::plot_grid(
  
  
  tbl1 %>% ggplot(aes(x=p, fill=factor(type,levels =c("operative","no-effect") ))) +  
    geom_histogram(breaks=seq(from=0, to=1, by = 0.01)) +
    geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
    geom_text(x = 0.05, y=0,label= "p=0.05",  color="red", angle=90, hjust=-4, vjust=-0.3)+
    labs(title="raw p-values") ,
  
  
  tbl1 %>% ggplot(aes(x=p_fdr, fill=factor(type,levels =c("operative","no-effect") ))) +  
    geom_histogram(breaks=seq(from=0, to=1, by = 0.01)) +
    geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
    geom_text(x = 0.05, y=0,label= "p=0.05",  color="red", angle=90, hjust=-4, vjust=-0.3)+
    labs(title="FDR") ,
  ncol=1
)
