library(tidyverse)
library(irenabpdata)
library(lubridate)
library(dtw)


### visual appearance of plots
theme_set(theme_classic(base_size = 14))


source("src/R/load_data.R")
source("src/R/functions-era5l-merra2.R")


#### Paper section 2.1.1: 2.1.1.	Classifying generation profiles of large PV installations in Chile


# Find erroneous data
erroneous <- find_erroneous(capacity_factors_subset)

# Manually add installations which are erroneous from visual inspection
erroneous <- c(
  erroneous,
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
  "SOLAR ANTAY"
)

erroneous <- erroneous %>% unique()


#13
plot_classification_procedure(ts, erroneous[2])
ggsave("results/figures/Figure-2.png")


# Cluster non erroneous into fixed and tracking
selection <- cluster_ts(capacity_factors_subset,
                        erroneous,
                        4,
                        hour,
                        colors)

ggsave("results/figures/Figure-3.png")

selection %>% group_by(Type) %>% summarize(s = n())

#### Section 3.1: Raw timeseries
final_tab_join <- load_indicators(selection = selection)
boxplot_comparisons(final_tab_join,
                    "results/figures/Figure-4.png",
                    c("tracking", "fixed"))

density_plot(selection = selection,
             file_name = "results/figures/Figure-5.png")

boxplot_comparisons(final_tab_join,
                    "Figure-B1.png",
                    c("tracking", "fixed"),
                    c("mbe"),
                    c("Hourly"))



#### Section 3.2: Deseasonalized timeseries

merra2_tracking_file <-
  "data/indicators_merra2_tracking_deseason_rad.csv"
era5l_tracking_file <-
  "data/indicators_era5l_tracking_deseason_rad.csv"
merra2_non_tracking_file <-
  "data/indicators_merra2_optimal_deseason_rad.csv"
era5l_non_tracking_file <-
  "data/indicators_era5l_optimal_deseason_rad.csv"


final_tab_join <- load_indicators(
  merra2_tracking_file,
  era5l_tracking_file,
  merra2_non_tracking_file,
  era5l_non_tracking_file,
  selection = selection
)

boxplot_comparisons(final_tab_join,
                    "figure-6.png",
                    c("tracking", "fixed"),
                    c("pearson"))

files <- c(
  "data/deseason_rad_optimal_era5l.csv",
  "data/deseason_rad_tracking_era5l.csv",
  "data/deseason_rad_optimal_merra2.csv",
  "data/deseason_rad_tracking_merra2.csv",
  "data/deseason_rad_optimal_reference.csv",
  "data/deseason_rad_tracking_reference.csv"
)


density_plot(files,
             selection = selection,
             "figure6-density-plot-deseasonalized.png")

##### 3.4 Understanding simulation quality

mean_cor_joined <- calculate_capacity_factor_simulation_quality()


mean_cor_joined %>%
  ggplot(aes(x = mean_cap, y = Value)) +
  geom_point(aes(col = Source), size = 4) +
  scale_color_manual(values = COLORS3) +
  facet_wrap(. ~ Set, scale = "free") +
  xlab("Mean capacity factor") +
  ylab("Correlation between simulation and observation")

ggsave("Figure-8.png")

mean_cor_joined %>%
  filter(Set != "erroneous") %>%
  filter(Installation != name) %>%
  group_by(Set) %>%
  summarize(cor = cor(mean_cap, Value))

####check if removal of 1 element changes correlations for fixed installations
correlations_one_out <- correlation_one_out_mean("fixed",
                                                 mean_cor_joined)

correlations_one_out %>%
  ggplot(aes(x = Name, y = Cor)) + geom_bar(stat = "identity")

####check if removal of 1 element changes correlations for fixed installations
correlations_one_out <- correlation_one_out_mean("tracking",
                                                 mean_cor_joined)

correlations_one_out %>%
  ggplot(aes(x = Name, y = Cor)) + geom_bar(stat = "identity")



#####
mean_cor_joined <- calculate_irr_diff_factor_simulation_quality()

mean_cor_joined %>%
  ggplot(aes(x = diff, y = Value)) +
  geom_point(aes(col = Source), size = 4) +
  scale_color_manual(values = COLORS3) +
  facet_wrap(. ~ Set) +
  xlab(
    "Mean absolute difference between clear-sky conditions \nand ERA5-land irradiation on surface"
  ) +
  ylab("Correlation between simulation and observation")

ggsave("C-1.png")

mean_cor_joined %>%
  filter(Set != "erroneous") %>%
  group_by(Set) %>%
  summarize(cor = cor(diff, Value))

####check if removal of 1 element changes correlations for fixed installations
correlations_one_out <- correlation_one_out_diff("fixed",
                                                 mean_cor_joined)

correlations_one_out %>%
  ggplot(aes(x = Name, y = Cor)) + geom_bar(stat = "identity")

####check if removal of 1 element changes correlations for fixed installations
correlations_one_out <- correlation_one_out_diff("tracking",
                                                 mean_cor_joined)

correlations_one_out %>%
  ggplot(aes(x = Name, y = Cor)) + geom_bar(stat = "identity")
