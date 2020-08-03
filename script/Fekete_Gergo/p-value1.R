library("tidyverse")

mean_NT<-1.2
sigma_NT<-0.25
N_b<-5000


tbl2<-tibble( experiment_id=character(),p=numeric(), type=character())
for( i in 1:N_b)
{
  od_NT1<-rnorm(6,mean = mean_NT, sd = sigma_NT)
  od_NT2<-rnorm(6,mean = mean_NT, sd = sigma_NT)
  
  result_of_t_test<-t.test(od_NT1,od_NT2,alternative = "less")
  tbl2<-tbl2 %>% add_row(experiment_id=sprintf("exp_%03i",i),p=result_of_t_test$p.value, type="NT-NT" )
}

tbl2 %>% ggplot(aes(x=p))+geom_histogram(breaks=seq(from=0, to=1, by = 0.01))
tbl2 %>% ggplot(aes(x=p))+geom_histogram(breaks=seq(from=0, to=1, by = 0.1))


tbl2 %>%  ggplot(aes(x=p)) +geom_abline(color="blue", linetype="dotted")+stat_ecdf()

mean(tbl2$p<0.05) # ennek elég pontosan 0.05 nek kéne lenni. Miért lesz kb 0.045 ?
