
library("tidyverse")

mean_NT<-1.2
sigma_NT<-0.15

mean_AMP<-1.05
sigma_AMP<-0.3

N<-2500


tbl1<-tibble( experiment_id=character(),p=numeric(), type=character())
for( i in 1:N)
{
  od_NT<-rnorm(4,mean = mean_NT, sd = sigma_NT)
  od_AMP<-rnorm(5,mean = mean_AMP, sd = sigma_AMP)
  
  result_of_t_test<-t.test(od_AMP,od_NT,alternative =  "less")
  tbl1<-tbl1 %>%  add_row(experiment_id=sprintf("exp_%03i",i),p=result_of_t_test$p.value, type="NT-AMP" )
}

tbl1 %>% ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.05)) +
  geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
  labs(title = "Histogram nagy bin-ekkel")

dir.create("out/undersatndig-of-FDR")
ggsave(filename = "out/undersatndig-of-FDR/hitogram1.jpg")

tbl1 %>%  ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.01)) +
  geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
  labs(title = "Histogram finom bin felosztassal")
ggsave(filename = "out/undersatndig-of-FDR/hitogram2.jpg")

tbl1 %>%  ggplot(aes(x=p)) +
  geom_abline(color="blue", linetype="dotted")+stat_ecdf() +
  labs(title = "CDF of p-values (Cumulative distribution function)")

#################################################################

tbl1<-tibble( experiment_id=character(),p=numeric(), type=character())
for( i in 1:N)
{
  od_NT1<-rnorm(4,mean = mean_NT, sd = sigma_NT)
  od_NT2<-rnorm(5,mean = mean_NT, sd = sigma_NT)
  
  result_of_t_test<-t.test(od_NT1,od_NT2,alternative =  "less")
  tbl1<-tbl1 %>%  add_row(experiment_id=sprintf("exp_%03i",i),p=result_of_t_test$p.value, type="NT-NT" )
}

tbl1 %>% ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.05)) +
  geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
  labs(title = "Histogram finom nagy bin-ekkel")

tbl1 %>%  ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.01)) +
  geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
  labs(title = "Histogram finom bin felosztassal")

# CDF plot - ez az integrálja a hisztogramnak. Szokatlanabb, de a zaj kevésbé zavaró rajta
tbl1 %>%  ggplot(aes(x=p)) +
  geom_abline(color="blue", linetype="dotted", size=1.5)+
  stat_ecdf() +
  labs(title = "CDF of p-values (Cumulative distribution function)")

mean(tbl1$p<0.05)

########################################################################x


od_NT1<-rnorm(4,mean = mean_NT, sd = sigma_NT)

tbl1<-tibble( experiment_id=character(),p=numeric(), type=character())
for( i in 1:N)
{
  od_AMP<-rnorm(5,mean = mean_AMP, sd = sigma_AMP)
  
  result_of_t_test<-t.test(od_NT1,od_AMP,alternative =  "two.sided")
  tbl1<-tbl1 %>%  add_row(experiment_id=sprintf("exp_%03i",i),p=result_of_t_test$p.value, type="NT-NT" )
}


tbl1 %>% ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.05)) +
  labs(title = "Histogram finom nagy bin-ekkel")

tbl1 %>%  ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.01)) +
  labs(title = "Histogram finom bin felosztassal")

tbl1 %>%  ggplot(aes(x=p)) +
  geom_abline(color="blue", linetype="dotted", size=1.5)+
  stat_ecdf() +
  labs(title = "CDF of p-values (Cumulative distribution function)")

#power
mean(tbl1$p<0.05)


################################################################

########################################################################x


od_NT1<-rnorm(4,mean = mean_NT, sd = sigma_NT)

tbl1<-tibble( experiment_id=character(),p=numeric(), type=character())
for( i in 1:N)
{
  od_NT2<-rnorm(5,mean = mean_NT, sd = sigma_NT)
  
  result_of_t_test<-t.test(od_NT1,od_NT2,alternative =  "two.sided")
  tbl1<-tbl1 %>%  add_row(experiment_id=sprintf("exp_%03i",i),p=result_of_t_test$p.value, type="NT-NT" )
}


tbl1 %>% ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.05)) +
  geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
  labs(title = "Histogram finom nagy bin-ekkel")

tbl1 %>%  ggplot(aes(x=p))+
  geom_histogram(breaks=seq(from=0, to=1, by = 0.01)) +
  geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
  geom_text(x = 0.05, y=0,label= "p=0.05",  color="red", angle=90, hjust=-4, vjust=-0.3)+
  labs(title = "Histogram finom bin felosztassal")

tbl1 %>%  ggplot(aes(x=p)) +
  geom_abline(color="blue", linetype="dotted", size=1.5)+
  geom_vline(xintercept = 0.05, color="red" , linetype="dashed")+
  geom_text(x = 0.05, y=0,label= "p=0.05",  color="red", angle=90, hjust=-4, vjust=-0.3)+
  stat_ecdf() +
  labs(title = "CDF of p-values (Cumulative distribution function)")

mean(tbl1$p<0.05)


################################################################


