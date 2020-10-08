library(lubridate)

library(tidyverse)

library(dtw)

source("functions.R")

#### load data ####
source("load_data.R")

erroneous<-find_erroneous(capacity_factors_subset)


### manual entry
erroneous<-c(erroneous,
             "PILOTO SOLAR CARDONES",
             "SOLAR LAS TERRAZAS",
             "CORDILLERILLA",
             "PARQUE SOLAR CUZ CUZ",
             "LASTURCAS",
             "SDGX01",
             "LAS ARAUCARIAS",
             "SOLAR ESPERANZA",
             "SOLAR CHUCHINI",
             #"SOLAR HORMIGA",
             #"SOLAR EL AGUILA I",
             "SOLAR ANTAY")

erroneous<-erroneous %>% unique()



data_clustering<- capacity_factors_subset %>%
  filter(!(Location %in% erroneous)) %>% 
  #filter(set_ == 1) %>% 
  group_by(Location, hour=hour(Time), set_=set_) %>%
  summarize(Capacity_Factor_mean=mean(Capacity_Factor,na.rm=TRUE)) %>% 
  ungroup() %>% group_by(Location,set_) %>% 
  mutate(Capacity_Factor_Normalized=Capacity_Factor_mean/max(Capacity_Factor_mean,na.rm=TRUE)) %>% 
  ungroup()  %>% 
  mutate(Capacity_Factor_mean=ifelse(hour<4, NA, Capacity_Factor_mean)) %>% 
  mutate(Capacity_Factor_Normalized=ifelse(hour<4, NA, Capacity_Factor_Normalized)) %>% 
  dplyr::select(Location, hour, Capacity_Factor_Normalized) %>% 
  spread(Location,Capacity_Factor_Normalized) 
  

data_plotting<-capacity_factors_subset %>% 
  filter(!(Location %in% erroneous)) %>% 
  mutate(Capacity_Factor=ifelse(hour(Time)<4,NA,Capacity_Factor))
  
colors<-c(COLORS10,COLORS10,COLORS10)
colors[1:23]<-c(COLORS10[10])
colors[5]<-COLORS10[7]

clustered_data<-cluster_ts(data_clustering, 
                           data_plotting, 
                           4, 
                           hour,
                           colors)

ggsave("../../figures/Figure2.png", width = 20, height = 12, units = c("cm"), dpi = 600)

manual_assignment <- c("LAS MOLLACAS")
#,
#                       "SOLAR ANTAY")
#1 ,3
non_tracking<-clustered_data %>% 
  filter(Cluster %in% c(1,3)) %>% 
  dplyr::select(Location) %>% 
  unique() %>% 
  unlist() %>% 
  union(manual_assignment)


#2, 4, 5
tracking<-clustered_data %>% 
  filter(Cluster %in% c(2,4)) %>% 
  dplyr::select(Location) %>% 
  unique() %>% 
  unlist() %>% 
  setdiff(manual_assignment)
  

selection<-tibble(Location=c(erroneous,tracking,non_tracking),
                  Type=c(rep("erroneous",length(erroneous)),
                         rep("tracking",length(tracking)),
                         rep("non-tracking",length(non_tracking))))

selection %>% group_by(Type) %>% summarize(s=n())

t<-quality_assess_selection(selection)

ts<-read_csv("../../data/pv_cf_reference_chile.csv", col_types = paste0(c("?", rep("d", 57)),collapse=""))
names(ts)[1]<-"Time"
ts<-ts %>% gather(Location, Capacity_Factor, -Time)


###plot examples of classification of errneous

library(lubridate)

loc<-"CARRERA PINTO ETAPA I"

e_loc<-selection %>% filter(Type=="erroneous")

#13
plot_classification_procedure(ts, e_loc[2, ]$Location)
ggsave("../../figures/figure1-errenous.png", width = 20, height = 10, units = c("cm"), dpi = 600)


fixed<-selection %>% filter(Type=="non-tracking")
plot_classification_procedure(ts, fixed$Location[6],FALSE)
ggsave("../../figures/fixed.png", width = 20, height = 10, units = c("cm"), dpi = 600)





