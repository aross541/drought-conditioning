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

####Exploratory Figures####

pimo_explore<- 
  cleaned_data %>%
  filter(sp=="pimo") %>%
  select(converted_date,ht,trt) %>%
  na.omit()
ggplot(data=pimo_explore, aes(x=converted_date, y=ht))+
  geom_point()
plot(pimo_explore$ht~pimo_explore$converted_date)
               

  
               