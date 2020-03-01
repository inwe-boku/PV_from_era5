library(tidyverse)
library(irenabpdata)

t1<-read_csv("../../results/tables/indicators_deseasonalized_era5land_and_rn.csv") %>% 
  mutate(Type = "deseasonalized") %>% 
  gather(Indicator, Value, -Type, -X1) %>% 
  mutate(Source = substr(Indicator, str_length(Indicator) - 3, 100)) %>% 
  mutate(Source = str_replace(Source, "[hdm]_", "")) %>% 
  mutate(Indicator = str_replace(Indicator, "_era5", "")) %>% 
  mutate(Indicator = str_replace(Indicator, "_rn", "")) %>% 
  dplyr::select(X1, Type, Source, Indicator, Value) 
  

names(t1)[1]<-"Installation"

t2<-read_csv("../../results/tables/indicators_era5land.csv") %>% 
  mutate(Type = "Raw", Source = "ERA5") %>% 
  gather(Indicator, Value, -X1, -Type, -Source)

names(t2)[1]<-"Installation"


t3<-read_csv("../../results/tables/indicators_rn.csv") %>% 
  mutate(Type = "Raw", Source = "RN") %>% 
  gather(Indicator, Value, -X1, -Type, -Source)

names(t3)[1]<-"Installation"


final_tab<-bind_rows(t1, t2, t3) %>% 
  mutate(Resolution = substr(Indicator, str_length(Indicator), 100)) %>% 
  mutate(Indicator = str_replace(Indicator, "_[hdm]","")) %>% 
  mutate(Source = tolower(Source)) %>% 
  mutate(Resolution = ifelse(Resolution == "d", "Daily", Resolution)) %>% 
  mutate(Resolution = ifelse(Resolution == "h", "Hourly", Resolution)) %>% 
  mutate(Resolution = ifelse(Resolution == "m", "Monthly", Resolution)) %>% 
  mutate(Source = ifelse(Source == "era5", "ERA5-Land", Source)) %>% 
  mutate(Source = ifelse(Source == "rn", "Rn", Source)) 


final_tab$Resolution<-factor(final_tab$Resolution, 
                             levels = final_tab$Resolution, 
                             labels= final_tab$Resolution)

write_csv(final_tab, "../../results/tables/all_indicators.csv")


t4<-read_csv("../../results/tables/installations_with_set.csv") %>% 
  dplyr::select(NOMBRE, set)

final_tab_join<-full_join(final_tab, t4, by=c("Installation" = "NOMBRE")) %>% 
  mutate(Set = set) %>% 
  mutate(Set = ifelse(Set == "subset_1", "S1", Set)) %>% 
  mutate(Set = ifelse(Set == "subset_2", "S2", Set)) %>% 
  mutate(Set = ifelse(Set == "all", "All", Set))
  

write_csv(final_tab_join, "../../results/tables/all_indicators_with_sets.csv")

theme_set(theme_classic(base_size = 14))

### full and deseasonalized

final_tab_join %>% 
#  filter(Indicator == "mbe") %>% 
  filter(Type == "Raw") %>% 
  filter(Indicator %in% c("pearson", "rmse")) %>% 
  ggplot(aes(x = Set, y = Value)) +
  geom_boxplot(aes(fill = Source, )) +
  facet_wrap(.~Resolution + Indicator, nrow = 3, scale = "free") +
  scale_fill_manual(values=COLORS3) 

ggsave("../../results/figures/all_raw.png")

final_tab_join %>% 
  #  filter(Indicator == "mbe") %>% 
  filter(Type == "deseasonalized") %>% 
  filter(Indicator %in% c("pearson", "rmse")) %>% 
  filter(Indicator %in% c("pearson", "rmse")) %>% 
  ggplot(aes(x = Set, y = Value)) +
  geom_boxplot(aes(fill = Source, )) +
  facet_wrap(.~Resolution + Indicator, nrow = 3, scale = "free") +
  scale_fill_manual(values=COLORS3) 

ggsave("../../results/figures/all_deseasonalized.png")



### single indicators full

final_tab_join %>% 
  filter(Indicator == "mbe") %>% 
  filter(Type == "Raw") %>% 
  ggplot(aes(x = Set, y = Value)) +
  geom_boxplot(aes(fill = Source, )) +
  facet_wrap(.~Resolution, ncol = 3) +
  scale_fill_manual(values=COLORS3)

ggsave("../../results/figures/mbe_raw.png")


final_tab_join %>% 
  filter(Indicator == "pearson_raw") %>% 
  filter(Type == "Raw") %>% 
  ggplot(aes(x = Set, y = Value)) +
  geom_boxplot(aes(fill = Source, )) +
  facet_wrap(.~Resolution, ncol = 3) +
  scale_fill_manual(values=COLORS3)

ggsave("../../results/figures/pearson_raw.png")


final_tab_join %>% 
  filter(Indicator == "rmse") %>% 
  ggplot(aes(x = Set, y = Value)) +
  geom_boxplot(aes(fill = Source, )) +
  facet_wrap(.~Resolution, ncol = 3) +
  scale_fill_manual(values=COLORS3)

ggsave("../../results/figures/rmse_raw.png")



dim(ts)

ts<-read_csv("../../results/figures/pv_cf_reference_chile.csv", col_types = paste0(c("?", rep("d", 57)),collapse=""))
names(ts)[1]<-"Time"
ts<-ts %>% gather(Location, Capacity_Factor, -Time)

# Figure1
#start = "2016-01-01"
#ende = "2018-12-31"
#plant = "BELLAVISTA"

library(lubridate)

ts %>% 
  filter(Location == "BELLAVISTA") %>% 
  filter(Time > ymd_hm("2016-01-01 23:00")) %>% 
  filter(Time < ymd_hm("2019-01-01 00:00")) %>% 
  ggplot(aes(x = Time, y = Capacity_Factor)) + 
  geom_line(col=COLORS3[2]) + 
  ylab("Capacity Factor")

ggsave("../../results/figures/BELLAVISTA.png")



#figure 2
#start = "2018-01-01"
#ende = "2018-01-07"
#8lant = "PUERTO SECO SOLAR"

ts %>% 
  mutate(Capacity_Factor = ifelse(is.na(Capacity_Factor), 0, Capacity_Factor)) %>% 
  filter(Location == "PUERTO SECO SOLAR") %>% 
  filter(Time > ymd_hm("2017-12-31 23:00")) %>% 
  filter(Time < ymd_hm("2018-01-08 00:00")) %>% 
  ggplot(aes(x = Time, y = Capacity_Factor)) + 
  geom_line(fill=COLORS3[2], size=1) + 
  ylab("Capacity Factor")

ggsave("../../results/figures/PUERTO SECO SOLAR.png")




