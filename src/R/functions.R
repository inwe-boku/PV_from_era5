cluster_ts<-function(data_clustering, data_plotting, nmb_clusters, grouping,colors){
  
  
  
  data_clustering_mat<-data_clustering[,2:ncol(data_clustering)] %>% as.matrix()
  
  data_clustering_mat[is.na(data_clustering_mat)]<-0
  distMatrix <- dist(t(data_clustering_mat), method="DTW")
  
  print("dist matrix done!")
  
  set.seed(123)
  k <- kmeans(distMatrix, centers=nmb_clusters)
  
  cluster<-tibble(Location=names(k$cluster),
                  Cluster=k$cluster)
  
  ret <- data_plotting %>%
    full_join(cluster)
  
  COLORS<-c(COLORS10,COLORS10,COLORS10)
  
  p<-ret %>%
    #  filter(Location %in% cluster1) %>%
    #filter(set_ == 1) %>%
    group_by(Location, Time=grouping(Time), Cluster) %>%
    summarize(Capacity_Factor=mean(Capacity_Factor,na.rm=TRUE)) %>%
    ggplot(aes(x=Time,y=Capacity_Factor)) + geom_line(aes(color=Location), size = 0.5) +
    facet_wrap(.~Cluster) + 
    theme(legend.position = "none") +
    scale_color_manual(values = colors) +
    xlab("Hour of day") +
    ylab("Capacity Factor (Average)")
  
  plot(p)
  
  check_membership<-ret %>%
    dplyr::select(Location,set_,Cluster) %>%
    unique()
  
  check_membership %>% group_by(set_,Cluster) %>% summarize(n=n()) %>% print()
  
  
  return(ret)
}

check_with_manual_classification<-function(erroneous){
  
  t4<-read_csv("installations_with_set.csv") %>%
    dplyr::select(NOMBRE, set)
  
  erroneous<-tibble(Location=erroneous, Error=1)
  
  t4<-full_join(t4,erroneous,by=c("NOMBRE"="Location"))
  return(t4)
}

quality_assess_selection<-function(selection){
  
  t1<-read_csv("indicators_deseasonalized_era5land_and_rn.csv") %>%
    mutate(Type = "deseasonalized") %>%
    gather(Indicator, Value, -Type, -X1) %>%
    mutate(Source = substr(Indicator, str_length(Indicator) - 3, 100)) %>%
    mutate(Source = str_replace(Source, "[hdm]_", "")) %>%
    mutate(Indicator = str_replace(Indicator, "_era5", "")) %>%
    mutate(Indicator = str_replace(Indicator, "_rn", "")) %>%
    dplyr::select(X1, Type, Source, Indicator, Value)
  
  
  names(t1)[1]<-"Installation"
  
  t2<-read_csv("indicators_era5land.csv") %>%
    mutate(Type = "Raw", Source = "ERA5") %>%
    gather(Indicator, Value, -X1, -Type, -Source)
  
  names(t2)[1]<-"Installation"
  
  
  t3<-read_csv("indicators_rn.csv") %>%
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
  
  
  
  t4<-read_csv("installations_with_set.csv") %>%
    dplyr::select(NOMBRE, set)
  
  
  t4<-full_join(t4,selection,by=c("NOMBRE"="Location")) %>%
    mutate(Type_ = Type) %>%
    dplyr::select(-Type)
  
  
  final_tab_join<-full_join(final_tab, t4, by=c("Installation" = "NOMBRE")) %>%
    mutate(Set = set) %>%
    mutate(Set = ifelse(Type_ == "tracking", "tracking", Set)) %>%
    mutate(Set = ifelse(Type_ == "non-tracking", "non-tracking", Set)) %>%
    mutate(Set = ifelse(Type_ == "erroneous", "erroneous", Set))
  
  t4<-read_csv("installations_with_set.csv") %>%
    dplyr::select(NOMBRE, set)
  
  
  t4<-full_join(t4,selection,by=c("NOMBRE"="Location")) %>%
    mutate(Type_ = Type) %>%
    dplyr::select(-Type)
  
  
  final_tab_join<-full_join(final_tab, t4, by=c("Installation" = "NOMBRE")) %>%
    mutate(Set = set) %>%
    mutate(Set = ifelse(Type_ == "tracking", "tracking", Set)) %>%
    mutate(Set = ifelse(Type_ == "non-tracking", "non-tracking", Set)) %>%
    mutate(Set = ifelse(Type_ == "erroneous", "erroneous", Set))
  
  p<-final_tab_join %>%
    #  filter(Indicator == "mbe") %>%
    filter(Type == "Raw") %>%
    filter(Indicator %in% c("pearson", "rmse")) %>%
    ggplot(aes(x = Set, y = Value)) +
    geom_boxplot(aes(fill = Source, )) +
    facet_wrap(.~Resolution + Indicator, nrow = 3) +
    scale_fill_manual(values=COLORS3)
  
  plot(p)
  
  
  
  return(final_tab_join)
  
}

classify_erroneous<-function(capacity_factors_length_series, min_n, ylim){
  capacitiy_factors_length_series<-capacity_factors_classified %>%
    na.omit() %>%
    group_by(Location) %>%
    summarize(length=summarize_length(class),set_=mean(set_,na.rm=TRUE))
  
  p<-capacitiy_factors_length_series %>%
    ggplot(aes(x=set_,y=length))+
    geom_point(aes(col=as.character(set_))) +
    ylim(ylim)
  
  plot(p)
  
  set_remove_0<- capacitiy_factors_length_series %>% filter(length>min_n) %>% dplyr::select(Location)
  print(paste0("Locations removed:",nrow(set_remove_0)))
  return(set_remove_0)
}

find_erroneous<-function(capacity_factors_subset){
  capacity_factors_subset_mean_yearly<-capacity_factors_subset %>%
    na.omit() %>%
    group_by(Location) %>%
    mutate(yearly_mean=mean(Capacity_Factor, na.rm=TRUE))
  
  capacity_factors_classified <- capacity_factors_subset_mean_yearly %>%
    mutate(class=Capacity_Factor<0.5*yearly_mean)
  
  
  
  set_remove_rule1<-classify(capacity_factors_classified, 70, c(0,70))
  
  
  
  #Rule_2: series of values at less than 70% of monthly mean
  capacity_factors_subset_mean_monthly<-capacity_factors_subset %>%
    na.omit() %>%
    group_by(Location, month(Time)) %>%
    mutate(monthly_mean=mean(Capacity_Factor, na.rm=TRUE))
  
  capacity_factors_classified <- capacity_factors_subset_mean_monthly %>%
    mutate(class=Capacity_Factor<0.7*monthly_mean) %>% 
    ungroup()
  
  set_remove_rule2<-classify(capacity_factors_classified,
                             70,
                             c(0,100))
  
  
  
  erroneous<- union(set_remove_rule1,set_remove_rule2) %>% unlist()


    
  return(erroneous)
}


plot_classification_procedure<-function(ts, loc, thresholds=TRUE){
  yearly_cap<-ts %>%
    na.omit() %>%
    group_by(Location) %>%
    mutate(yearly_mean=mean(Capacity_Factor, na.rm=TRUE)) %>% 
    filter(Location ==loc)
  
  capacity_factors_classified_y <- yearly_cap %>%
    mutate(class=Capacity_Factor<0.5*yearly_mean)
  
  s_y<-rle(capacity_factors_classified_y$class)
  
  monthly_cap<-capacity_factors_subset %>%
    na.omit() %>% 
    group_by(Location, month(Time)) %>%
    mutate(monthly_mean=mean(Capacity_Factor, na.rm=TRUE))%>% 
    filter(Location ==loc) 
  
  capacity_factors_classified_m <- monthly_cap %>%
    mutate(class=Capacity_Factor<0.7*monthly_mean)
  
  s_m<-rle(capacity_factors_classified_m$class)
  
  monthly_cap <- monthly_cap %>% 
    filter(Time > ymd_hm("2016-01-01 23:00")) %>% 
    filter(Time < ymd_hm("2019-01-01 00:00"))

  yearly_cap <- yearly_cap %>% 
    filter(Time > ymd_hm("2016-01-01 23:00")) %>% 
    filter(Time < ymd_hm("2019-01-01 00:00"))
  
    
  p<-ts %>% 
    filter(Location == loc) %>% 
    na.omit() %>% 
    mutate(class_length_y=rep(s_y$lengths,s_y$lengths)) %>% 
    mutate(class_type_y=rep(s_y$values,s_y$lengths)) %>% 
    mutate(class_length_m=rep(s_m$lengths,s_m$lengths)) %>% 
    mutate(class_type_m=rep(s_m$values,s_m$lengths)) %>% 
    filter(Time > ymd_hm("2016-01-01 23:00")) %>% 
    filter(Time < ymd_hm("2019-01-01 00:00")) %>% 
    mutate(is_yearly_err=ifelse(class_type_y & class_length_y>70, Capacity_Factor, NA)) %>% 
    mutate(is_monthly_err=ifelse(class_type_m & class_length_m>70, Capacity_Factor, NA)) %>% 
    ggplot(aes(x = Time, y = Capacity_Factor)) + 
    geom_line(col=COLORS3[2],size=0.1) + 
    geom_line(data=monthly_cap,aes(x=Time,y=monthly_mean),col=COLORS3[1],linetype=2) +
    ylab("Capacity Factor") +
    geom_hline(yintercept = capacity_factors_classified_y$yearly_mean[1],linetype=2,size=1.2,col=COLORS3[3])
  
  if(thresholds){
    p<-p + 
    geom_line(aes(x=Time,y=is_yearly_err), col=COLORS3[3],size=0.1) +
    geom_line(aes(x=Time,y=is_monthly_err), col=COLORS3[1],size=0.1)
  }
  p
  
}

classify<-function(data,treshhold,vec){
  data %>% 
    group_by(Location) %>% 
    summarize(s=summarize_length(class)) %>% 
    filter(s>treshhold) %>% dplyr::select(Location) %>% unique() %>% return()
}


summarize_length<-function(s){
  r<-rle(s)
  return(max(r$lengths[which(r$values)]))
}



load_indicators<-function(merra2_tracking_file="../../data/indicators_merra2_tracking.csv",
                          era5l_tracking_file="../../data/indicators_era5l_tracking.csv",
                          merra2_non_tracking_file="../../data/indicators_merra2_optimal.csv",
                          era5l_non_tracking_file="../../data/indicators_era5l_optimal.csv",
                          selection=selection){
  
  t1<-read_csv(merra2_tracking_file) %>% 
    mutate(Type = "tracking") %>% 
    gather(Indicator, Value, -Type, -X1) %>% 
    mutate(Source = "MERRA2") %>% 
    dplyr::select(X1, Type, Source, Indicator, Value) 
  
  names(t1)[1]<-"Installation"
  
  t2<-read_csv(era5l_tracking_file) %>% 
    mutate(Type = "tracking") %>% 
    gather(Indicator, Value, -Type, -X1) %>% 
    mutate(Source = "ERA5-Land") %>% 
    dplyr::select(X1, Type, Source, Indicator, Value) 
  
  names(t2)[1]<-"Installation"
  
  t3<-read_csv(merra2_non_tracking_file) %>% 
    mutate(Type = "fixed") %>% 
    gather(Indicator, Value, -Type, -X1) %>% 
    mutate(Source = "MERRA2") %>% 
    dplyr::select(X1, Type, Source, Indicator, Value) 
  
  names(t3)[1]<-"Installation"
  
  t4<-read_csv(era5l_non_tracking_file) %>% 
    mutate(Type = "fixed") %>% 
    gather(Indicator, Value, -Type, -X1) %>% 
    mutate(Source = "ERA5-Land") %>% 
    dplyr::select(X1, Type, Source, Indicator, Value) 
  
  names(t4)[1]<-"Installation"
  
  final_tab<-bind_rows(t1, t2, t3, t4) %>% 
    mutate(Resolution = substr(Indicator, str_length(Indicator), 100)) %>% 
    mutate(Indicator = str_replace(Indicator, "_[hdm]","")) %>% 
    mutate(Source = tolower(Source)) %>% 
    mutate(Resolution = ifelse(Resolution == "d", "Daily", Resolution)) %>% 
    mutate(Resolution = ifelse(Resolution == "h", "Hourly", Resolution)) %>% 
    mutate(Resolution = ifelse(Resolution == "m", "Monthly", Resolution)) %>% 
    mutate(Source = ifelse(Source == "era5", "ERA5-Land", Source)) 
  
  
  final_tab$Resolution<-factor(final_tab$Resolution, 
                               levels = final_tab$Resolution, 
                               labels= final_tab$Resolution)
  
  selection<-dplyr::select(selection, Location, set=Type)
  
  final_tab_join<-full_join(final_tab, selection, by=c("Installation" = "Location")) %>% 
    mutate(Set = set) 
  
  return(final_tab_join)
  
}

boxplot_comparisons<-function(final_tab_join,
                              filename,
                              sets,
                              indicators=c("pearson","rmse"),
                              resolutions=c("Hourly", "Daily", "Monthly")){
  
  p1<-final_tab_join %>% 
    #  filter(Indicator == "mbe") %>% 
    filter(Type == "tracking") %>% 
    filter(Indicator %in% indicators) %>%
    filter(Resolution %in% resolutions) %>% 
    ggplot(aes(x = Set, y = Value)) +
    geom_boxplot(aes(fill = Source, )) +
    facet_wrap(.~Resolution + Indicator, nrow = 3, scale = "free") +
    scale_fill_manual(values=COLORS3) 
  
  plot(p1)
  
  p2<-final_tab_join %>% 
    #  filter(Indicator == "mbe") %>% 
    filter(Type == "tracking") %>% 
    filter(Indicator %in% indicators) %>%
    filter(Resolution %in% resolutions) %>% 
    ggplot(aes(x = Set, y = Value)) +
    geom_boxplot(aes(fill = Source, )) +
    facet_wrap(.~Resolution + Indicator, nrow = 3, scale = "free") +
    scale_fill_manual(values=COLORS3) 
  
  plot(p2)
  
  ###best fit
  p3<-final_tab_join %>% 
    #  filter(Indicator == "mbe") %>% 
    filter((Type == "fixed" & Set=="non-tracking") | (Type == "tracking" & Set=="tracking") | (Type == "fixed" & Set=="erroneous")) %>% 
    filter(Indicator %in% indicators) %>%
    filter(Resolution %in% resolutions) %>% 
    mutate(Set=ifelse(Set=="non-tracking","fixed",Set)) %>% 
    filter(Set %in% sets) %>% 
    ggplot(aes(x = Set, y = Value)) +
    geom_boxplot(aes(fill = Source), notch = FALSE) +
    facet_grid(Indicator ~ Resolution, scales="free_y") +
    scale_fill_manual(values=COLORS3) 
  #geom_dotplot(binaxis='y', stackdir='center',
  #               position=position_dodge(1))
  
  plot(p3)
  
  final_tab_join %>% 
    #  filter(Indicator == "mbe") %>% 
    filter((Type == "fixed" & Set=="non-tracking") | (Type == "tracking" & Set=="tracking") | (Type == "fixed" & Set=="erroneous")) %>% 
    #filter(Indicator %in% c("pearson", "rmse")) %>% 
    mutate(Set=ifelse(Set=="non-tracking","fixed",Set)) %>% 
    filter(Set %in% sets) %>% group_by(Resolution, Indicator, Source, Type) %>% 
    summarize(s=mean(Value)) %>% print()
  
  ggsave(filename, p3, width = 20, height = 10, units = c("cm"), dpi = 600)
}


violin_plot<-function(selection){
  
  ts_era5l_optimal<-read_csv("timeseries_capacity_factors_pv_optimal_era5l.csv") %>% 
    gather(Installation,Capacity_Factor,-X1) %>%
    dplyr::select(DateTime=X1, Installation, Capacity_Factor) %>% 
    full_join(selection,by=c("Installation"="Location")) %>% 
    filter(Type == "non-tracking") %>% 
    mutate(Source="era5-land")
  
  ts_era5l_tracking<-read_csv("timeseries_capacity_factors_pv_tracking_era5l.csv") %>% 
    gather(Installation,Capacity_Factor,-X1) %>%
    dplyr::select(DateTime=X1, Installation, Capacity_Factor) %>% 
    full_join(selection,by=c("Installation"="Location")) %>% 
    filter(Type == "tracking") %>% 
    mutate(Source="era5-land")
  
  ts_merra2_optimal<-read_csv("timeseries_capacity_factors_pv_optimal_merra2.csv")  %>% 
    gather(Installation,Capacity_Factor,-X1) %>%
    dplyr::select(DateTime=X1, Installation, Capacity_Factor) %>% 
    full_join(selection,by=c("Installation"="Location")) %>% 
    filter(Type == "non-tracking") %>% 
    mutate(Source="merra2")
  
  ts_merra2_tracking<-read_csv("timeseries_capacity_factors_pv_tracking_merra2.csv") %>% 
    gather(Installation,Capacity_Factor,-X1) %>%
    dplyr::select(DateTime=X1, Installation, Capacity_Factor) %>% 
    full_join(selection,by=c("Installation"="Location")) %>% 
    filter(Type == "tracking") %>% 
    mutate(Source="merra2")
  
  
  ts_reference<-read_csv("repo/PV_from_era5/results/tables/pv_cf_reference_chile.csv", col_types = paste0(c("?", rep("d", 57)),collapse=""))
  names(ts_reference)[1]<-"Timestamp"
  ts_reference<-ts_reference %>% gather(Location, Capacity_Factor, -Timestamp) %>% 
    mutate(Source = "Reference")
  
  all_capacity_factors_join<-bind_rows(ts_era5l_optimal,
                                       ts_era5l_tracking,
                                       ts_merra2_optimal,
                                       ts_merra2_tracking) %>% 
    full_join(ts_reference,by=c("Installation"="Location","DateTime"="Timestamp"))
  
  
  
  
  tt<-all_capacity_factors_join %>% 
    mutate(Capacity_Factor = ifelse(Capacity_Factor.y == 0, NA, Capacity_Factor.y)) %>% 
    na.omit() %>% 
    mutate(Reference=Capacity_Factor.y, Capacity_Factor = Capacity_Factor.x, Source = Source.x)
  
  grp <- cut(tt$Reference, breaks=hist(tt$Reference, breaks = 5)$breaks)
  
  ts_data_bins<-tt %>% mutate(Group = grp)
  
  p<-ts_data_bins %>% 
    #filter(Set == "subset_2") %>% 
    na.omit() %>% 
    ggplot(aes(x = Group, y = Capacity_Factor)) + 
    geom_bin2d() +
    scale_fill_gradientn(colours = c("white", COLORS3[1]))+
    
    
    #    geom_violin(aes(fill = Source)) +
    #scale_fill_manual(values = COLORS3) +
    #geom_line(data=tibble(x = c(0.5, 1.5), y = c(0.2, 0.2)),aes(x = x, y = y), linetype = 2) +
    #geom_line(data=tibble(x = c(1.5, 2.5), y = c(0.4, 0.4)),aes(x = x, y = y), linetype = 2) +
    #geom_line(data=tibble(x = c(2.5, 3.5), y = c(0.6, 0.6)),aes(x = x, y = y), linetype = 2) +
    #geom_line(data=tibble(x = c(3.5, 4.5), y = c(0.8, 0.8)),aes(x = x, y = y), linetype = 2) +
    #geom_line(data=tibble(x = c(4.5, 5.5), y = c(1, 1)),aes(x = x, y = y), linetype = 2) +
    facet_wrap(.~Source + Type) 
  plot(p)
  
  ggsave("../../figures/Figure-4-violine.png", p, width = 20, height = 10, units = c("cm"), dpi = 600)
  
}

trans<-function(x){
  return(x^(1/1.2))
}

density_plot<-function(files=c("timeseries_capacity_factors_pv_optimal_era5l.csv",
                               "timeseries_capacity_factors_pv_tracking_era5l.csv",
                               "timeseries_capacity_factors_pv_optimal_merra2.csv",
                               "timeseries_capacity_factors_pv_tracking_merra2.csv",
                               "repo/PV_from_era5/results/tables/pv_cf_reference_chile.csv",
                               "repo/PV_from_era5/results/tables/pv_cf_reference_chile.csv"),
                       selection,
                       file_name="Figure-4-density.png"){
  
  
  
  
  ts_era5l_optimal<-read_csv(files[1]) %>% 
    gather(Installation,Capacity_Factor,-X1) %>%
    dplyr::select(DateTime=X1, Installation, Capacity_Factor) %>% 
    full_join(selection,by=c("Installation"="Location")) %>% 
    filter(Type == "non-tracking") %>% 
    mutate(Source="era5-land")
  
  ts_era5l_tracking<-read_csv(files[2]) %>% 
    gather(Installation,Capacity_Factor,-X1) %>%
    dplyr::select(DateTime=X1, Installation, Capacity_Factor) %>% 
    full_join(selection,by=c("Installation"="Location")) %>% 
    filter(Type == "tracking") %>% 
    mutate(Source="era5-land")
  
  ts_merra2_optimal<-read_csv(files[3])  %>% 
    gather(Installation,Capacity_Factor,-X1) %>%
    dplyr::select(DateTime=X1, Installation, Capacity_Factor) %>% 
    full_join(selection,by=c("Installation"="Location")) %>% 
    filter(Type == "non-tracking") %>% 
    mutate(Source="merra2")
  
  ts_merra2_tracking<-read_csv(files[4]) %>% 
    gather(Installation,Capacity_Factor,-X1) %>%
    dplyr::select(DateTime=X1, Installation, Capacity_Factor) %>% 
    full_join(selection,by=c("Installation"="Location")) %>% 
    filter(Type == "tracking") %>% 
    mutate(Source="merra2")
  
  
  ts_reference_optimal<-read_csv(files[5], col_types = paste0(c("?", rep("d", 57)),collapse=""))
  names(ts_reference_optimal)[1]<-"Timestamp"
  ts_reference_optimal<-ts_reference_optimal %>% gather(Location, Capacity_Factor, -Timestamp) %>% 
    mutate(Source = "Reference", Type="non-tracking")
  
  ts_reference_tracking<-read_csv(files[6], col_types = paste0(c("?", rep("d", 57)),collapse=""))
  names(ts_reference_tracking)[1]<-"Timestamp"
  ts_reference_tracking<-ts_reference_tracking %>% gather(Location, Capacity_Factor, -Timestamp) %>% 
    mutate(Source = "Reference", Type = "tracking")
  
  ts_reference<-bind_rows(ts_reference_optimal,ts_reference_tracking)
  
  all_capacity_factors_join<-bind_rows(ts_era5l_optimal,
                                       ts_era5l_tracking,
                                       ts_merra2_optimal,
                                       ts_merra2_tracking) %>% 
    full_join(ts_reference,by=c("Installation"="Location","DateTime"="Timestamp","Type"="Type"))
  
  
  
  
  tt<-all_capacity_factors_join %>% 
    mutate(Capacity_Factor = ifelse(Capacity_Factor.y == 0, NA, Capacity_Factor.y)) %>% 
    na.omit() %>% 
    mutate(Reference=Capacity_Factor.y, Capacity_Factor = Capacity_Factor.x, Source = Source.x)
  
  #grp <- cut(tt$Reference, breaks=hist(tt$Reference, breaks = 5)$breaks)
  
  #ts_data_bins<-tt %>% mutate(Group = grp)
  
  p<-tt %>% 
    mutate(Type=ifelse(Type=="non-tracking","fixed",Type)) %>% 
    #filter(Set == "subset_2") %>% 
    na.omit() %>% 
    ggplot(aes(x = Capacity_Factor.y, y = Capacity_Factor)) + 
    geom_bin2d() +
    scale_fill_gradientn(colours = c("white", COLORS3[1],COLORS3[1],COLORS3[1],COLORS3[1]),trans="identity")+
    #COLORS3[1]
    
    #    geom_violin(aes(fill = Source)) +
    #scale_fill_manual(values = COLORS3) +
    #geom_line(data=tibble(x = c(0.5, 1.5), y = c(0.2, 0.2)),aes(x = x, y = y), linetype = 2) +
    #geom_line(data=tibble(x = c(1.5, 2.5), y = c(0.4, 0.4)),aes(x = x, y = y), linetype = 2) +
    #geom_line(data=tibble(x = c(2.5, 3.5), y = c(0.6, 0.6)),aes(x = x, y = y), linetype = 2) +
    #geom_line(data=tibble(x = c(3.5, 4.5), y = c(0.8, 0.8)),aes(x = x, y = y), linetype = 2) +
    #geom_line(data=tibble(x = c(4.5, 5.5), y = c(1, 1)),aes(x = x, y = y), linetype = 2) +
    facet_wrap(.~Source + Type) +
    xlab("Capacity Factor (Reference)") +
    ylab("Capacity Factor (Simulation)") +
    geom_abline(slope = 1)
  
  plot(p)
  
  ggsave(file_name, p, width = 20, height = 10, units = c("cm"), dpi = 600)
  
}
