####Data Cleaning with Ethan####

##https://stackoverflow.com/questions/67767866/how-to-convert-a-data-frame-from-wide-to-long-with-multiple-columns
library(tidyverse)

data_file <- "Greenhouse"
df <- read.csv("2_Incremental/greenhouse_morphological_060323.csv", header=T)

### tell R what grouping variables to use - 
### these are what we use to identify each individual
grouping_variables <- c("-id", "-sp", "-block", "-seedling..")
df$verbose_id <- paste0(df$id, df$sp, df$block, df$seedling, df$trt)
df$ht_5 <- as.numeric(df$ht_5)
df$rcd_5 <- as.numeric(df$rcd_5)
df$trt <- gsub("\\s", "", df$trt)


cleaned_data <- pivot_longer(df, cols = -c(id, sp, block, seedling, trt, seed_source), 
                             names_to = c(".value", "Seq"), names_sep = "_") %>%
  mutate(converted_date=as.Date(date,"%m/%d/%Y"))

####Exploratory PIMO Figures####

pimo_explore<- 
  cleaned_data %>%
  filter(sp=="pimo") %>%
  select(converted_date,ht,trt) %>%
  na.omit()
pimo_explore$month<-format(pimo_explore$converted_date, "%b") %>%
  factor(levels=c("May", "Jun", "Sep", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=pimo_explore, aes(x=month, y=ht, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="Height")+
  ggtitle("Greenhouse PIMO HT's")+
  scale_fill_manual(values=cols)

pimorcd_explore<- 
  cleaned_data %>%
  filter(sp=="pimo") %>%
  select(converted_date,rcd,trt) %>%
  na.omit()
pimorcd_explore$month<-format(pimorcd_explore$converted_date, "%b") %>%
  factor(levels=c("May", "Jun", "Sep", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=pimorcd_explore, aes(x=month, y=rcd, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="RCD")+
  ggtitle("Greenhouse PIMO RCD's")+
  scale_fill_manual(values=cols)

pimorootshoot_explore<- 
  cleaned_data %>%
  filter(sp=="pimo") %>%
  select(converted_date,rootshoot,trt) %>%
  na.omit()
pimorootshoot_explore$month<-format(pimorootshoot_explore$converted_date, "%b") %>%
  factor(levels=c("Jun", "Sep", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=pimorootshoot_explore, aes(x=month, y=rootshoot, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="Root:Shoot")+
  ggtitle("Greenhouse PIMO Root:Shoot Ratio's")+
  scale_fill_manual(values=cols)

####PIMO Water potentials####

pimowp_explore<-
  cleaned_data %>%
  filter(sp=="pimo") %>%
  select(converted_date,X75mdwp,trt) %>%
  na.omit()
pimowp_explore$month<-format(pimowp_explore$converted_date, "%b") %>%
  factor(levels=c("Aug","Sep", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=pimowp_explore, aes(x=month, y=(X75mdwp), fill=factor(trt)))+
  geom_line()+
  geom_point()+
  labs(x="Month", y="Water Pontential MPa")+
  ggtitle("Greenhouse PIMO 75% Water potential's")+
  scale_fill_manual(values=cols)

####Exploratory PIPO Figures####
               
pipo_explore<- 
  cleaned_data %>%
  filter(sp=="pipo") %>%
  select(converted_date,ht,trt) %>%
  na.omit()
pipo_explore$month<-format(pipo_explore$converted_date, "%b") %>%
  factor(levels=c("May", "Jun","Aug", "Sep", "Dec"))

test<-format(pipo_explore$converted_date, "%b") %>%
  factor(levels=c("May", "Jun", "Sep", "Dec"))
pipo_explore$converted_date[which(is.na(test))]
  

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=pipo_explore, aes(x=month, y=ht, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="Height")+
  ggtitle("Greenhouse PIPO HT's")+
  scale_fill_manual(values=cols)

piporcd_explore<- 
  cleaned_data %>%
  filter(sp=="pipo") %>%
  select(converted_date,rcd,trt) %>%
  na.omit()
piporcd_explore$month<-format(piporcd_explore$converted_date, "%b") %>%
  factor(levels=c("May", "Jun", "Aug", "Sep", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=piporcd_explore, aes(x=month, y=rcd, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="RCD")+
  ggtitle("Greenhouse PIPO RCD's")+
  scale_fill_manual(values=cols)

piporootshoot_explore<- 
  cleaned_data %>%
  filter(sp=="pipo") %>%
  select(converted_date,rootshoot,trt) %>%
  na.omit()
piporootshoot_explore$month<-format(piporootshoot_explore$converted_date, "%b") %>%
  factor(levels=c("Jun","Aug", "Sep", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=piporootshoot_explore, aes(x=month, y=rootshoot, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="Root:Shoot")+
  ggtitle("Greenhouse PIPO Root:Shoot Ratio's")+
  scale_fill_manual(values=cols)

####Exploratory CDA LAOC Figures####

cda_explore<- 
  cleaned_data %>%
  filter(seed_source=="cda") %>%
  select(converted_date,ht,trt) %>%
  na.omit()
cda_explore$month<-format(cda_explore$converted_date, "%b") %>%
  factor(levels=c("May", "Jun", "Aug", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=cda_explore, aes(x=month, y=ht, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="Height")+
  ggtitle("Greenhouse CDA LAOC HT's")+
  scale_fill_manual(values=cols)

cdarcd_explore<- 
  cleaned_data %>%
  filter(seed_source=="cda") %>%
  select(converted_date,rcd,trt) %>%
  na.omit()
cdarcd_explore$month<-format(cdarcd_explore$converted_date, "%b") %>%
  factor(levels=c("May", "Jun", "Aug", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=cdarcd_explore, aes(x=month, y=rcd, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="RCD")+
  ggtitle("Greenhouse CDA LAOC RCD's")+
  scale_fill_manual(values=cols)

cdarootshoot_explore<- 
  cleaned_data %>%
  filter(seed_source=="cda") %>%
  select(converted_date,rootshoot,trt) %>%
  na.omit()
cdarootshoot_explore$month<-format(cdarootshoot_explore$converted_date, "%b") %>%
  factor(levels=c("Jun","Aug","Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=cdarootshoot_explore, aes(x=month, y=rootshoot, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="Root:Shoot")+
  ggtitle("Greenhouse CDA Root:Shoot Ratio's")+
  scale_fill_manual(values=cols)

####Exploratory UM LAOC Figures####

um_explore<- 
  cleaned_data %>%
  filter(seed_source=="um") %>%
  select(converted_date,ht,trt) %>%
  na.omit()
um_explore$month<-format(um_explore$converted_date, "%b") %>%
  factor(levels=c("May", "Jun", "Aug", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=um_explore, aes(x=month, y=ht, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="Height")+
  ggtitle("Greenhouse UM LAOC HT's")+
  scale_fill_manual(values=cols)

umrcd_explore<- 
  cleaned_data %>%
  filter(seed_source=="um") %>%
  select(converted_date,rcd,trt) %>%
  na.omit()
umrcd_explore$month<-format(umrcd_explore$converted_date, "%b") %>%
  factor(levels=c("May", "Jun", "Aug", "Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=umrcd_explore, aes(x=month, y=rcd, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="RCD")+
  ggtitle("Greenhouse UM LAOC RCD's")+
  scale_fill_manual(values=cols)  

umrootshoot_explore<- 
  cleaned_data %>%
  filter(seed_source=="um") %>%
  select(converted_date,rootshoot,trt) %>%
  na.omit()
umrootshoot_explore$month<-format(umrootshoot_explore$converted_date, "%b") %>%
  factor(levels=c("Dec"))

cols<-c("d"="firebrick", "c"="steelblue3")
ggplot(data=umrootshoot_explore, aes(x=month, y=rootshoot, fill=factor(trt)))+
  geom_boxplot()+
  labs(x="Month", y="Root:Shoot")+
  ggtitle("Greenhouse UM Root:Shoot Ratio's")+
  scale_fill_manual(values=cols)
               