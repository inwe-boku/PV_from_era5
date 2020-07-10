capacity_factors <-
  read_csv("data/pv_cf_reference_chile.csv",
           col_types = paste0(c("?", rep("d", 57)), collapse = ""))
names(capacity_factors)[1] <- "Time"
capacity_factors <-
  capacity_factors %>% gather(Location, Capacity_Factor,-Time) %>%
  mutate(Capacity_Factor = ifelse(Capacity_Factor == 0, NA, Capacity_Factor))

locations_subset <- read_csv("data/installations_with_set.csv") %>%
  dplyr::select(Location = NOMBRE, set) %>%
  mutate(set_ = 0) %>%
  mutate(set_ = ifelse(set == "subset_1", 1, set_)) %>%
  mutate(set_ = ifelse(set == "subset_2", 2, set_))

capacity_factors_subset <-
  full_join(capacity_factors, locations_subset)

meta_data <- read_csv("data/full_pv_set_chile.csv")

capacity_meta_subset <-
  full_join(capacity_factors_subset, meta_data, by = c("Location" = "NOMBRE"))


clear_sky <-
  read_csv("data/clear_sky_normalized_88_installations.csv") %>%
  mutate(Time = X1) %>% gather(Location, Irridiance,-Time,-X1) %>%
  dplyr::select(Time, Location, Irridiance)

all_pv_info <- meta_data

era5_irridiance <-
  read_csv("data/eral5_radiation_all_installations.csv") %>%
  mutate(Time = X1) %>% gather(Location, Irridiance,-Time,-X1) %>%
  dplyr::select(Time, Location, Irridiance) %>% group_by(Location) %>%
  mutate(Irridiance = Irridiance / max(Irridiance, na.rm = TRUE)) %>%
  ungroup()

ts <-
  read_csv("data/pv_cf_reference_chile.csv",
           col_types = paste0(c("?", rep("d", 57)), collapse = ""))
names(ts)[1] <- "Time"
ts <- ts %>% gather(Location, Capacity_Factor,-Time)

capacity_factors_tracking <-
  read_csv("data/timeseries_capacity_factors_pv_tracking_era5l.csv")

capacity_factors_fixed <-
  read_csv("data/timeseries_capacity_factors_pv_optimal_era5l.csv")

capacity_factors_tracking_merra2 <-
  read_csv("data/timeseries_capacity_factors_pv_tracking_merra2.csv")

capacity_factors_fixed_merra2 <-
  read_csv("data/timeseries_capacity_factors_pv_optimal_merra2.csv")
