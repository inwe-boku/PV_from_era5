library(tidyverse)
library(irenabpdata)


### full dataset
theme_set(theme_classic(base_size = 14)) 


source("functions.R")
source("quality_assessment.R")




final_tab_join<-load_indicators(selection=selection)
boxplot_comparisons(final_tab_join,
                    "../../figures/Figure-3.png",
                    c("tracking", "fixed"))


boxplot_comparisons(final_tab_join,
                    "../../figures/Figure-A1.png",
                    c("tracking", "fixed"),
                    c("mbe"),
                    c("Hourly"))


final_tab_join %>% 
  filter(Indicator == "pearson") %>% 
  filter((Type == "fixed" & Set=="non-tracking") | (Type == "tracking" & Set=="tracking") | (Type == "fixed" & Set=="erroneous")) %>% 
  filter(Resolution=="Hourly") %>% 
  filter(Set !="erroneous") %>% 
  ggplot(aes(x=Value)) +geom_histogram(aes(col=Set))

### deseasonalized radiation
merra2_tracking_file<-"../../data/indicators_merra2_tracking_deseason_rad.csv"
era5l_tracking_file<-"../../data/indicators_era5l_tracking_deseason_rad.csv"
merra2_non_tracking_file<-"../../data/indicators_merra2_optimal_deseason_rad.csv"
era5l_non_tracking_file<-"../../data/indicators_era5l_optimal_deseason_rad.csv"


final_tab_join<-load_indicators(merra2_tracking_file,
                                era5l_tracking_file,
                                merra2_non_tracking_file,
                                era5l_non_tracking_file,
                                selection=selection)

boxplot_comparisons(final_tab_join,
                    "../../figures/figure-5-best-fit-deseasonalized-rad.png",
                    c("tracking","fixed"),
                    c("pearson"))

boxplot_comparisons(final_tab_join,
                    "../../figures/Figure-A2.png",
                    c("tracking", "fixed"),
                    c("mbe"),
                    c("Hourly"))

density_plot(selection=selection)

files<-c("../../data/deseason_rad_optimal_era5l.csv",
        "../../data/deseason_rad_tracking_era5l.csv",
        "../../data/deseason_rad_optimal_merra2.csv",
        "../../data/deseason_rad_tracking_merra2.csv",
        "../../data/deseason_rad_optimal_reference.csv",
        "../../data/deseason_rad_tracking_reference.csv")


density_plot(files,
             selection=selection,
             "../../figures/figure6-density-plot-deseasonalized.png")

########

capacity_factors_tracking<-read_csv("../../data/timeseries_capacity_factors_pv_tracking_era5l.csv")
capacity_factors_fixed<-read_csv("../../data/timeseries_capacity_factors_pv_optimal_era5l.csv")
capacity_factors_tracking_merra2<-read_csv("../../data/timeseries_capacity_factors_pv_tracking_merra2.csv")
capacity_factors_fixed_merra2<-read_csv("../../data/timeseries_capacity_factors_pv_optimal_merra2.csv")


capacity_factors_gather_tracking<-capacity_factors_tracking %>% 
  gather(Installation, Capacity_Factor, -X1) %>% 
  as_tibble() %>% 
  group_by(Installation) %>% 
  summarize(mean_cap=mean(Capacity_Factor,na.rm=TRUE)) %>% 
  filter()

capacity_factors_gather_fixed<-capacity_factors_fixed %>% 
  gather(Installation, Capacity_Factor, -X1) %>% 
  as_tibble() %>% 
  group_by(Installation) %>% 
  summarize(mean_cap=mean(Capacity_Factor,na.rm=TRUE)) %>% 
  filter()

capacity_factors_gather_tracking_merra2<-capacity_factors_tracking_merra2 %>% 
  gather(Installation, Capacity_Factor, -X1) %>% 
  as_tibble() %>% 
  group_by(Installation) %>% 
  summarize(mean_cap=mean(Capacity_Factor,na.rm=TRUE)) %>% 
  filter() %>% 
  mutate(Source="merra2")

capacity_factors_gather_fixed_merra2<-capacity_factors_fixed_merra2 %>% 
  gather(Installation, Capacity_Factor, -X1) %>% 
  as_tibble() %>% 
  group_by(Installation) %>% 
  summarize(mean_cap=mean(Capacity_Factor,na.rm=TRUE)) %>% 
  filter() %>% 
  mutate(Source="merra")


capacity_factors_mean<-full_join(capacity_factors_gather_tracking, 
                                 capacity_factors_gather_fixed,
                                 by=c("Installation"="Installation")) %>% 
  mutate(tracking=mean_cap.x,fixed=mean_cap.y,Source="era5-land") 

capacity_factors_mean_merra2<-full_join(capacity_factors_gather_tracking_merra2, 
                                 capacity_factors_gather_fixed_merra2,
                                 by=c("Installation"="Installation")) %>% 
  mutate(tracking=mean_cap.x,fixed=mean_cap.y,Source="merra2") 

capacity_factors_mean_full<-bind_rows(capacity_factors_mean,
                                      capacity_factors_mean_merra2)

final_tab_join<-load_indicators(selection=selection)

mean_cor_joined<-final_tab_join %>% 
  #  filter(Indicator == "mbe") %>% 
  filter((Type == "fixed" & Set=="non-tracking") | (Type == "tracking" & Set=="tracking") | (Type == "fixed" & Set=="erroneous")) %>% 
  filter(Indicator %in% c("pearson")) %>% 
  #filter(Source == "era5-land") %>% 
  filter(Resolution == "Hourly") %>% 
  full_join(capacity_factors_mean_full) %>% 
  filter(Set!="erroneous") %>% 
  mutate(mean_cap=ifelse(Set=="tracking",tracking,fixed)) %>% 
  mutate(Set=ifelse(Set=="non-tracking","fixed",Set))


mean_cor_joined%>% 
  ggplot(aes(x=mean_cap,y=Value)) + 
  geom_point(aes(col=Source), size = 4) + 
  scale_color_manual(values = COLORS3) +
  facet_wrap(.~Set, scale="free") +
  xlab("Mean capacity factor") +
  ylab("Correlation between simulation and observation") 
#  geom_text(aes(label=Installation))


mean_cor_joined %>% 
  filter(Set!="erroneous") %>% 
  filter(Installation != name) %>%  
  group_by(Set) %>% 
  summarize(cor=cor(mean_cap,Value))

####check if removal of 1 element changes correlations for fixed installations
names<-mean_cor_joined$Installation %>% unique() %>% unlist()
for(name in names){
  print(name)
  mean_cor_joined %>% 
    filter(Set!="erroneous") %>% 
    filter(Installation != name) %>%  
    group_by(Set) %>% 
    summarize(cor=cor(mean_cap,Value)) %>% 
    filter(Set=="fixed") %>% 
    #filter(Source=="era5-land") %>% 
    print()
}

####check if removal of 1 element changes correlations for fixed installations
names<-mean_cor_joined$Installation %>% unique() %>% unlist()
for(name in names){
  print(name)
  mean_cor_joined %>% 
    filter(Set!="erroneous") %>% 
    filter(Installation != name) %>%  
    group_by(Set) %>% 
    summarize(cor=cor(mean_cap,Value)) %>% 
    filter(Set=="tracking") %>% 
    #filter(Source=="era5-land") %>% 
    print()
}

ggsave("../../figures/figure7-explaining-quality.png")





#####
clear_sky_era5<-full_join(clear_sky,era5_irridiance, by=c("Time"="Time", "Location"="Location"))

clear_sky_era5_diff<-clear_sky_era5 %>% mutate(diff=abs(Irridiance.x-Irridiance.y))  %>% 
  group_by(Location) %>% summarize(diff=mean(diff,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(n=1:n()) %>% 
  na.omit()

mean_cor_joined<-final_tab_join %>% 
  #  filter(Indicator == "mbe") %>% 
  filter((Type == "fixed" & Set=="non-tracking") | (Type == "tracking" & Set=="tracking") | (Type == "fixed" & Set=="erroneous")) %>% 
  filter(Indicator %in% c("pearson")) %>% 
  #filter(Source == "era5-land") %>% 
  filter(Resolution == "Hourly") %>% 
  full_join(clear_sky_era5_diff,by=c("Installation"="Location")) %>% 
  filter(Set!="erroneous") %>% 
  mutate(Set=ifelse(Set=="non-tracking","fixed",Set))

mean_cor_joined%>% 
  ggplot(aes(x=diff,y=Value)) + 
  geom_point(aes(col=Source), size = 4) + 
  scale_color_manual(values = COLORS3) +
  facet_wrap(.~Set)+
  xlab("Mean absolute difference between clear-sky conditions \nand ERA5-land irradiation on surface") +
  ylab("Correlation between simulation and observation")

mean_cor_joined %>% 
  filter(Set!="erroneous") %>% 
  group_by(Set) %>% 
  summarize(cor=cor(diff,Value))

####check if removal of 1 element changes correlations for fixed installations
names<-mean_cor_joined$Installation %>% unique() %>% unlist()
for(name in names){
  print(name)
  mean_cor_joined %>% 
    filter(Set!="erroneous") %>% 
    filter(Installation != name) %>%  
    group_by(Set) %>% 
    summarize(cor=cor(diff,Value)) %>% 
    filter(Set=="fixed") %>% 
    #filter(Source=="era5-land") %>% 
    print()
}

####check if removal of 1 element changes correlations for fixed installations
names<-mean_cor_joined$Installation %>% unique() %>% unlist()
for(name in names){
  print(name)
  mean_cor_joined %>% 
    filter(Set!="erroneous") %>% 
    filter(Installation != name) %>%  
    group_by(Set,Source) %>% 
    summarize(cor=cor(diff,Value)) %>% 
    filter(Set=="tracking") %>% 
    #filter(Source=="era5-land") %>% 
    print()
}


ggsave("../../figures/A3-explaining-quality-deseas.png")