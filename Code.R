####All again####
library(readxl)
all2 <- read_excel('~/Downloads/all2.xlsx')
all2 <- read_excel('~/Desktop/final.xlsx')
dat_h <- dplyr::filter(all2, res==2)
dat_l <- dplyr::filter(all2, res==1)
dat_c <- dplyr::filter(all2, res==0)


cor(all2$payoff, all2$payoff2)
View(all2)
t.test(dat_l$payoff2, dat_c$payoff2, alternative = 'greater')
t.test(dat_l$desp, dat_c$desp, alternative = 'less')
t.test(dat_l$conf, dat_c$conf2)


data_all2 <- data_all[-c(17, 23, 27, 45, 47:51),] 


####Визуализация усилий####
data_box3 <- dplyr::select(all2, res, desp2)%>% 
  mutate(res=case_when(res==0~"Control", TRUE~case_when(res==1~"Negative feedback", TRUE~"Positive feedback")))
bargraph.CI(x.factor = res, response = desp2, data = data_box3,  err.col="darkred", 
            xlab="Group", ylab="Number of bonus tasks solved", col=c("#6E8B3D", "#838B8B"), 
            ci.fun=function(x) c(mean(x)-0.645*sd(x)/sqrt(length(x)), mean(x)+0.645*sd(x)/sqrt(length(x))), err.lty=1)

####Визуализация вероятности####
data_all3 <- data_all[-c(17, 23, 27, 47:51),] %>% mutate(diff=conf2-conf)
data_box <- dplyr::select(all2, res, conf, conf2) %>% na.omit() %>% 
  mutate(res=as.factor(case_when(res==0~"Control", TRUE~case_when(res==1~"Negative feedback", TRUE~"Positive feedback"))))
box1 <-  dplyr::select(data_box, res, conf) %>% mutate(stage="day I")
box2 <-  dplyr::select(data_box, res, conf2) %>% mutate(stage="day II", conf=conf2) %>% dplyr::select(-conf2)
data_box <- rbind(box1, box2) %>% mutate(stage=as.factor(stage))
bargraph.CI(x.factor = res, response = conf, group=stage, data = data_box,
            xlab="Group", ylab="Confidence",  col=c("#6E8B3D", "#838B8B"),
            ci.fun=function(x) c(mean(x)-0.645*sd(x)/sqrt(length(x)), mean(x)+0.645*sd(x)/sqrt(length(x))), err.lty=1) 
dat_h <- filter(data_all3, res==2)
dat_l <- filter(data_all3, res==1)
dat_c <- filter(data_all3, res==0)
t.test(dat_h$conf, dat_c$conf, alternative='greater')
t.test(dat_l$conf, dat_c$conf)
t.test(dat_h$conf2, dat_c$conf2, alternative='greater')
t.test(dat_l$conf2, dat_c$conf2, alternative='less')
t.test(dat_l$diff, dat_c$diff, alternative='less')
t.test(dat_h$diff, dat_c$diff, alternative='greater')



####Новый старый####
# только вторая попытка (91 человек)
data2_old <- as.data.frame(read_excel("~/Downloads/experiment_old.xlsx", sheet = 1))
#сделаем рабочим
data2_old <- data2_old %>% mutate(get_signal=as.factor(get_signal), 
                                  female=case_when(female=="Мужской"~0, TRUE~1), all=wrong+true) %>% 
  dplyr::select(payoff, female, confidence, confidence2, quote, quote2, 
                number, predict, get_signal, capitals, wrong, true, confidence1_1, confidence2_1, all)


#разделение на группы и визуализация 
data2_h <- filter(data2_old, get_signal==2)
data2_l <- filter(data2_old, get_signal==1)
data2_n <- filter(data2_old, get_signal==0)

#confidence 
data_box <- dplyr::select(data2_old, get_signal, confidence, confidence1_1) %>% na.omit() %>% 
  mutate(get_signal=as.factor(case_when(get_signal==0~"Control", TRUE~case_when(get_signal==1~"Negative feedback", TRUE~"Positive feedback"))))
box1 <-  dplyr::select(data_box, get_signal, confidence) %>% mutate(stage="day I", conf=confidence) %>% dplyr::select(-confidence)
box2 <-  dplyr::select(data_box, get_signal, confidence1_1) %>% mutate(stage="day II", conf=confidence1_1) %>% dplyr::select(-confidence1_1)
data_box <- rbind(box1, box2) %>% mutate(stage=as.factor(stage))
bargraph.CI(x.factor = get_signal, response = conf, group=stage, data = data_box,
            xlab="Group", ylab="Confidence", err.col="black", col=c("#DBDBDB", "#838B8B"),
            ci.fun=function(x) c(mean(x)-0.645*se(x), mean(x)+0.645*se(x)), err.lty=1) 


