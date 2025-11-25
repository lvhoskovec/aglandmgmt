#############################################################
## Visualize Agricultural Land Management Activity Dataset ##
## Lauren Hoskovec                                         ##
## last updated: 11/25/2025                                ##
#############################################################

rm(list = ls())
gc()

###############
## libraries ##
###############

library(dplyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(tidyverse)
library(ncdf4)
library(tidyr)
library(data.table)
library(terra)
library(foreign)
library(collapse)
library(viridisLite)
library(rasterVis)
library(RColorBrewer)
library(plyr)
library(raster)
library(maps)

###################################
## Planting Date Temporal Trends ##
###################################

# 10th, 50th, and 90th percentile planting dates by crop from 1980-2023
{
  nc <- nc_open("planting_date_quantiles.nc")
  print(nc)
  quantile_data <- ncvar_get(nc, "planting_day_quantiles")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_vals <- ncvar_get(nc, "crop"); crop_vals
  quantile_vals <- ncvar_get(nc, "quantile"); quantile_vals
  nc_close(nc)

  # gather subsets of data to plot   
  crop_sub_vals <- c(1,2,3,4,7,10) # corn, cotton, sorghum, soybeans, spring wheat, winter wheat 
  quantile_sub_vals <- c(1, 3, 5) # 10, 50, 90 percentiles
  year_sub_vals <- 1:44 # all years 1980-2023

  plant_day_list <- list()
  
  for(c in 1:length(crop_sub_vals)){
    print(c)
    
    c_val <- crop_sub_vals[c]
    plant_day_list[[c]] <- list() # list for each crop 
    crop_matrix <- matrix(NA, nrow = 44, ncol = 3) 
    
    for(q in 1:length(quantile_sub_vals)){
      
      print(q)
      # matrix for each crop, rows are years, columns are quantiles
      q_val <- quantile_sub_vals[q]; q_val
      
      for(y in year_sub_vals){
        # all years 
        # subset to this year, crop, and quantile 
        quantile_subset <- quantile_data[,,y,c_val,q_val]
        # create df with planting day 
        quantile_df <- expand.grid(
          Lat = lat_vals,
          Lon = lon_vals) %>%
          mutate(planting_day = as.vector(quantile_subset))
        # filter out NA and suppressed data (-9998)
        quantile_df_filter <- quantile_df %>% 
          dplyr::filter(!is.na(planting_day) & planting_day != -9998)
        # average of quantiles over the grid cells for this year and crop
        quantile_mean <- mean(quantile_df_filter$planting_day)
        # fill matrix 
        crop_matrix[y, q] <- quantile_mean
        
      }
    }
    
    # fill list 
    plant_day_list[[c]] <- crop_matrix
    # clear for next crop
    crop_matrix <- NULL
    
  }
  
  # for each crop the 10, 50, and 90 percentiles 
  text_size <- 11
  crop_names <- c("Corn", "Cotton", "Sorghum", "Soybean", "Spring Wheat", "Winter Wheat")
  
  ## -- corn 
  # create df
  corn_df <- data.frame(plant_day_list[[1]])
  names(corn_df) <- c("lwr", "median", "upr")
  corn_df <- cbind(1980:2023, corn_df)
  names(corn_df)[1] <- "crop_year"
  head(corn_df)
  # fit linear model through medians
  lm_corn <- lm(median ~ crop_year, data = corn_df)
  summary(lm_corn) # values hard coded below 
  # plot 
  p_corn <- ggplot(data = corn_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Planting Day of Year", limits = c(60, 200), breaks = seq(60, 200, by = 20)) + 
    ggtitle(expression(paste("Corn: ", beta[1], " = -0.02, R"^2, " = -0.02, p-value = 0.65"))) + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12))
  # view 
  p_corn
  
  ## -- cotton 
  # create df
  cotton_df <- data.frame(plant_day_list[[2]])
  names(cotton_df) <- c("lwr", "median", "upr")
  cotton_df <- cbind(1980:2023, cotton_df)
  names(cotton_df)[1] <- "crop_year"
  head(cotton_df)
  # fit linear model through medians
  lm_cotton <- lm(median ~ crop_year, data = cotton_df)
  summary(lm_cotton)
  # plot 
  p_cotton <- ggplot(data = cotton_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Planting Day of Year", limits = c(60, 200), breaks = seq(60, 200, by = 20)) + 
    ggtitle(expression(paste("Cotton: ", beta[1], " = 0.27, R"^2, " = 0.56, p-value = <0.01"))) + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12))
  # view 
  p_cotton
  
  ## -- sorghum 
  # create df
  sorghum_df <- data.frame(plant_day_list[[3]])
  names(sorghum_df) <- c("lwr", "median", "upr")
  sorghum_df <- cbind(1980:2023, sorghum_df)
  names(sorghum_df)[1] <- "crop_year"
  head(sorghum_df)
  # fit linear model through medians
  lm_sorghum <- lm(median ~ crop_year, data = sorghum_df)
  summary(lm_sorghum)
  # plot 
  p_sorghum <- ggplot(data = sorghum_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Planting Day of Year", limits = c(60, 200), breaks = seq(60, 200, by = 20)) + 
    ggtitle(expression(paste("Sorghum: ", beta[1], " = 0.03, R"^2, " = -0.02, p-value = 0.58"))) + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12))
  # view 
  p_sorghum
  
  ## -- soybeans 
  # create df
  soybeans_df <- data.frame(plant_day_list[[4]])
  names(soybeans_df) <- c("lwr", "median", "upr")
  soybeans_df <- cbind(1980:2023, soybeans_df)
  names(soybeans_df)[1] <- "crop_year"
  head(soybeans_df)
  # fit linear model through medians
  lm_soybeans <- lm(median ~ crop_year, data = soybeans_df)
  summary(lm_soybeans)
  # plot 
  p_soybeans <- ggplot(data = soybeans_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Planting Day of Year", limits = c(60, 200), breaks = seq(60, 200, by = 20)) + 
    ggtitle(expression(paste("Soybeans: ", beta[1], " = -0.27, R"^2, " = 0.32, p-value = <0.01"))) + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12))
  # view 
  p_soybeans
  
  ## -- Spring wheat
  # create df
  spring_wheat_df <- data.frame(plant_day_list[[5]])
  names(spring_wheat_df) <- c("lwr", "median", "upr")
  spring_wheat_df <- cbind(1980:2023, spring_wheat_df)
  names(spring_wheat_df)[1] <- "crop_year"
  head(spring_wheat_df)
  # fit linear model through medians
  lm_spring_wheat <- lm(median ~ crop_year, data = spring_wheat_df)
  summary(lm_spring_wheat)
  # plot 
  p_spring_wheat <- ggplot(data = spring_wheat_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Planting Day of Year", limits = c(60, 200), breaks = seq(60, 200, by = 20)) + 
    ggtitle(expression(paste("Spring Wheat: ", beta[1], " = 0.20, R"^2, " = 0.09, p-value = 0.03"))) + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12))
  # view 
  p_spring_wheat
  
  ## -- Winter wheat
  # create df
  winter_wheat_df <- data.frame(plant_day_list[[6]])
  names(winter_wheat_df) <- c("lwr", "median", "upr")
  winter_wheat_df <- cbind(1980:2023, winter_wheat_df)
  names(winter_wheat_df)[1] <- "crop_year"
  head(winter_wheat_df)
  # fit linear model through medians
  lm_winter_wheat <- lm(median ~ crop_year, data = winter_wheat_df)
  summary(lm_winter_wheat)
  # plot 
  p_winter_wheat <- ggplot(data = winter_wheat_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Planting Day of Year", limits = c(-120, 0), breaks = seq(-120, 0, by = 20)) + 
    ggtitle(expression(paste("Winter Wheat: ", beta[1], " = -0.02, R"^2, " = -0.01, p-value = 0.45"))) + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12))
  # view 
  p_winter_wheat
  
  # combine crops to create plot grid 
  crops_plant_doy <- plot_grid(p_corn, p_soybeans, p_cotton, p_sorghum, 
                               p_spring_wheat, 
                               p_winter_wheat, 
                               ncol = 2)
  crops_plant_doy

}

##################################
## Planting Date Spatial Trends ##
##################################

# Maps of median planting date for each grid cell in 1980, 1990, 2000, 2010, and 2020 for each crop
{
  nc <- nc_open("planting_date_quantiles.nc")
  print(nc)
  quantile_data <- ncvar_get(nc, "planting_day_quantiles")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_vals <- ncvar_get(nc, "crop"); crop_vals
  quantile_vals <- ncvar_get(nc, "quantile"); quantile_vals
  global_att <- ncatt_get(nc, varid = 0)
  nc_close(nc)
  
  # subset to data for plotting 
  crop_map <- unlist(strsplit(global_att$crop_mapping, split = ",")); crop_map
  crop_sub_vals <- c(1,2,3,4,7,10) # corn, cotton, sorghum, soybeans, spring wheat, winter wheat 
  year_sub_vals <- c(1, 11, 21, 31, 41) # decades
  
  plots <- list()
  
  for(c in 1:10){
    plots[[c]] <- list()
    
    for(year_idx in 1:length(year_sub_vals)){
      print(year_idx)
      y <- year_sub_vals[year_idx]; y
      year_name <- year_vals[y]; year_name
      
      quantile_subset <- quantile_data[,,y,c,3] # grab median 
      crop_name <- crop_map[c]; crop_name
      
      quantile_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        dplyr::mutate(planting_day = as.vector(quantile_subset)) %>%
        dplyr::filter(!is.na(planting_day) & planting_day != -9998)
      
      ## create color palettes
      pal2 <- viridis::viridis(8, direction = -1, option = "plasma")
      state_map <- map_data('state')
      
      range(quantile_df$planting_day) # 168
      
      crop_breaks <- seq(60, 180, by = 40)
      if(crop_name %in% c("Winter Wheat","Winter Oats","Winter Barley")){
        crop_breaks <- seq(-120, 20, by = 40)
      }
      
      # by crop 
      plots[[c]][[year_idx]] <- ggplot() +
        geom_tile(data = quantile_df %>% dplyr::filter(!is.na(planting_day) & planting_day != -9998),
                  aes(x = Lon, y = Lat, fill = planting_day), alpha = 0.8, 
                  show.legend = FALSE) +
        geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
        scale_fill_viridis_c(name = "Planting Day", option = "plasma",
                             limits = c(min(crop_breaks), max(crop_breaks)),
                             breaks = crop_breaks) +
        coord_fixed(1.3) +
        labs(
          title = paste0(year_name, " ", crop_name),
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal()
      
    }
    
  }
  
  leg <- get_legend(ggplot() +
                      geom_tile(data = quantile_df %>% dplyr::filter(!is.na(planting_day) & planting_day != -9998),
                                aes(x = Lon, y = Lat, fill = planting_day), alpha = 0.8) +
                      geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
                      scale_fill_viridis_c(name = "Planting Day", option = "plasma",
                                           limits = c(60, 180),
                                           breaks = seq(60, 180, by = 20)) +
                      #scale_fill_viridis_c(name = "Planting Day", option = "plasma") +
                      coord_fixed(1.3) +
                      labs(
                        title = paste0(year_name, " ", crop_name),
                        x = "Longitude",
                        y = "Latitude"
                      ) +
                      theme_minimal())
  
  leg_winter <- get_legend(ggplot() +
                             geom_tile(data = quantile_df %>% dplyr::filter(!is.na(planting_day) & planting_day != -9998),
                                       aes(x = Lon, y = Lat, fill = planting_day), alpha = 0.8) +
                             geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
                             scale_fill_viridis_c(name = "Planting Day", option = "plasma", 
                                                  limits = c(-120, 20), 
                                                  breaks = seq(-120, 20, by = 20)) +
                             coord_fixed(1.3) +
                             labs(
                               title = paste0(year_name, " ", crop_name),
                               x = "Longitude",
                               y = "Latitude"
                             ) +
                             theme_minimal())
  
  crop_map 
  # [1] "Corn"          "Cotton"        "Sorghum"       "Soybeans"      "Spring Barley"
  # [6] "Spring Oats"   "Spring Wheat"  "Winter Barley" "Winter Oats"   "Winter Wheat" 
  
  # corn map 
  plant_map_corn <- plot_grid(plots[[1]][[1]], plots[[1]][[2]], plots[[1]][[3]], 
                              plots[[1]][[4]], plots[[1]][[5]], leg, ncol = 2)
  
  plant_map_corn

  # cotton 
  plant_map_cotton <- plot_grid(plots[[2]][[1]], plots[[2]][[2]], plots[[2]][[3]], 
                                plots[[2]][[4]], plots[[2]][[5]], leg, ncol = 2)
  
  plant_map_cotton

  # sorghum map 
  plant_map_sorghum <- plot_grid(plots[[3]][[1]], plots[[3]][[2]], plots[[3]][[3]], 
                                 plots[[3]][[4]], plots[[3]][[5]], leg, ncol = 2)
  
  plant_map_sorghum

  # soybeans map 
  plant_map_soybeans <- plot_grid(plots[[4]][[1]], plots[[4]][[2]], plots[[4]][[3]], 
                                  plots[[4]][[4]], plots[[4]][[5]], leg, ncol = 2)
  
  plant_map_soybeans

  # spring barley 
  plant_map_spring_barley <- plot_grid(plots[[5]][[1]], plots[[5]][[2]], plots[[5]][[3]], 
                                       plots[[5]][[4]], plots[[5]][[5]], leg, ncol = 2)
  
  plant_map_spring_barley

  # spring oats
  plant_map_spring_oats <- plot_grid(plots[[6]][[1]], plots[[6]][[2]], plots[[6]][[3]], 
                                     plots[[6]][[4]], plots[[6]][[5]], leg, ncol = 2)
  
  plant_map_spring_oats

  # spring wheat 
  plant_map_spring_wheat <- plot_grid(plots[[7]][[1]], plots[[7]][[2]], plots[[7]][[3]], 
                                      plots[[7]][[4]], plots[[7]][[5]], leg, ncol = 2)
  
  plant_map_spring_wheat

  # winter barley 
  plant_map_winter_barley <- plot_grid(plots[[8]][[1]], plots[[8]][[2]], plots[[8]][[3]], 
                                       plots[[8]][[4]], plots[[8]][[5]], leg_winter, ncol = 2)
  
  plant_map_winter_barley

  # winter oats
  plant_map_winter_oats <- plot_grid(plots[[9]][[1]], plots[[9]][[2]], plots[[9]][[3]], 
                                     plots[[9]][[4]], plots[[9]][[5]], leg_winter, ncol = 2)
  
  plant_map_winter_oats

  # winter wheat 
  plant_map_winter_wheat <- plot_grid(plots[[10]][[1]], plots[[10]][[2]], plots[[10]][[3]], 
                                      plots[[10]][[4]], plots[[10]][[5]], leg_winter, ncol = 2)
  
  plant_map_winter_wheat

}

##################################
## Harvest Date Temporal Trends ##
##################################

# 10th, 50th, and 90th percentile harvest dates by crop from 1980-2023
{
  nc <- nc_open("harvest_date_quantiles.nc")
  print(nc)
  quantile_data <- ncvar_get(nc, "harvest_day_quantiles")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_vals <- ncvar_get(nc, "crop"); crop_vals
  quantile_vals <- ncvar_get(nc, "quantile"); quantile_vals
  nc_close(nc)
  
  crop_sub_vals <- c(1,2,3,4,7,10) # corn, cotton, sorghum, soybeans, spring wheat, winter wheat 
  quantile_sub_vals <- c(1, 3, 5) # 10, 50, 90 percentiles
  year_sub_vals <- 1:44 # all years 1980-2023
  
  harvest_day_list <- list()
  
  for(c in 1:length(crop_sub_vals)){
    print(c)
    
    c_val <- crop_sub_vals[c]
    harvest_day_list[[c]] <- list() # list for each crop 
    crop_matrix <- matrix(NA, nrow = 44, ncol = 3) 
    
    for(q in 1:length(quantile_sub_vals)){
      
      print(q)
      # matrix for each crop, rows are years, columns are quantiles
      q_val <- quantile_sub_vals[q]; q_val
      
      for(y in year_sub_vals){
        # all years 
        # subset to this year, crop, and quantile 
        quantile_subset <- quantile_data[,,y,c_val,q_val]
        # create df with harvest day 
        quantile_df <- expand.grid(
          Lat = lat_vals,
          Lon = lon_vals) %>%
          mutate(harvest_day = as.vector(quantile_subset))
        # filter out NA and suppressed data (-9998)
        quantile_df_filter <- quantile_df %>% 
          dplyr::filter(!is.na(harvest_day) & harvest_day != -9998)
        # average of quantiles over the grid cells for this year and crop
        quantile_mean <- mean(quantile_df_filter$harvest_day)
        # fill matrix 
        crop_matrix[y, q] <- quantile_mean
        
      }
    }
    
    # fill list 
    harvest_day_list[[c]] <- crop_matrix
    # clear for next crop
    crop_matrix <- NULL
    
  }
  
  # for each crop the 10, 50, and 90 percentiles 
  text_size <- 11
  crop_names <- c("Corn", "Cotton", "Sorghum", "Soybean", "Spring Wheat", "Winter Wheat")
  
  ## -- corn 
  # create df
  corn_df <- data.frame(harvest_day_list[[1]])
  names(corn_df) <- c("lwr", "median", "upr")
  corn_df <- cbind(1980:2023, corn_df)
  names(corn_df)[1] <- "crop_year"
  head(corn_df)
  # fit linear model through medians
  lm_corn <- lm(median ~ crop_year, data = corn_df)
  summary(lm_corn)
  # plot 
  p_corn <- ggplot(data = corn_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Harvest Day of Year") + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12)) + 
    ggtitle("Corn")
  # view 
  p_corn
  
  ## -- cotton 
  # create df
  cotton_df <- data.frame(harvest_day_list[[2]])
  names(cotton_df) <- c("lwr", "median", "upr")
  cotton_df <- cbind(1980:2023, cotton_df)
  names(cotton_df)[1] <- "crop_year"
  head(cotton_df)
  # fit linear model through medians
  lm_cotton <- lm(median ~ crop_year, data = cotton_df)
  summary(lm_cotton)
  # plot 
  p_cotton <- ggplot(data = cotton_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Harvest Day of Year") + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12)) + 
    ggtitle("Cotton")
  # view 
  p_cotton
  
  ## -- sorghum 
  # create df
  sorghum_df <- data.frame(harvest_day_list[[3]])
  names(sorghum_df) <- c("lwr", "median", "upr")
  sorghum_df <- cbind(1980:2023, sorghum_df)
  names(sorghum_df)[1] <- "crop_year"
  head(sorghum_df)
  # fit linear model through medians
  lm_sorghum <- lm(median ~ crop_year, data = sorghum_df)
  summary(lm_sorghum)
  # plot 
  p_sorghum <- ggplot(data = sorghum_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Harvest Day of Year") + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12)) + 
    ggtitle("Sorghum")
  # view 
  p_sorghum
  
  ## -- soybeans 
  # create df
  soybeans_df <- data.frame(harvest_day_list[[4]])
  names(soybeans_df) <- c("lwr", "median", "upr")
  soybeans_df <- cbind(1980:2023, soybeans_df)
  names(soybeans_df)[1] <- "crop_year"
  head(soybeans_df)
  # fit linear model through medians
  lm_soybeans <- lm(median ~ crop_year, data = soybeans_df)
  summary(lm_soybeans)
  # plot 
  p_soybeans <- ggplot(data = soybeans_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Harvest Day of Year") + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12)) + 
    ggtitle("Soybeans")
  # view 
  p_soybeans
  
  ## -- Spring wheat
  # create df
  spring_wheat_df <- data.frame(harvest_day_list[[5]])
  names(spring_wheat_df) <- c("lwr", "median", "upr")
  spring_wheat_df <- cbind(1980:2023, spring_wheat_df)
  names(spring_wheat_df)[1] <- "crop_year"
  head(spring_wheat_df)
  # fit linear model through medians
  lm_spring_wheat <- lm(median ~ crop_year, data = spring_wheat_df)
  summary(lm_spring_wheat)
  # plot 
  p_spring_wheat <- ggplot(data = spring_wheat_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Harvest Day of Year") + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12)) + 
    ggtitle("Spring Wheat")
  # view 
  p_spring_wheat
  
  ## -- Winter wheat
  # create df
  winter_wheat_df <- data.frame(harvest_day_list[[6]])
  names(winter_wheat_df) <- c("lwr", "median", "upr")
  winter_wheat_df <- cbind(1980:2023, winter_wheat_df)
  names(winter_wheat_df)[1] <- "crop_year"
  head(winter_wheat_df)
  # fit linear model through medians
  lm_winter_wheat <- lm(median ~ crop_year, data = winter_wheat_df)
  summary(lm_winter_wheat)
  # plot 
  p_winter_wheat <- ggplot(data = winter_wheat_df) + 
    geom_line(aes(x = crop_year, y = median)) + 
    geom_point(aes(x = crop_year, y = median)) + 
    geom_errorbar(aes(x = crop_year, ymin = lwr, ymax = upr)) + 
    theme_minimal() + 
    scale_x_continuous("Year", limits = c(1979, 2024)) + 
    scale_y_continuous("Harvest Day of Year") + 
    theme(text = element_text(size = text_size), 
          plot.title = element_text(size = 12)) + 
    ggtitle("Winter Wheat")
  # view 
  p_winter_wheat
  
  # combine crops to create plot grid 
  crops_harvest_doy <- plot_grid(p_corn, p_soybeans, p_cotton, p_sorghum, 
                               p_spring_wheat, 
                               p_winter_wheat, 
                               ncol = 2)
  crops_harvest_doy
  
}

######################
## Fertilizer Rates ##
######################

# Map of 2023 total fertilizer N rate across CONUS by crop 
{
  nc <- nc_open("fertilizer_Nrate.nc")
  print(nc)
  total_rate_data <- ncvar_get(nc, "Total_Nrate_kgha_mean")
  fall_rate_data <- ncvar_get(nc, "Fall_Nrate_kgha_mean")
  spring_rate_data <- ncvar_get(nc, "Spring_Nrate_kgha_mean")
  planting_rate_data <- ncvar_get(nc, "Planting_Nrate_kgha_mean")
  after_planting_rate_data <- ncvar_get(nc, "After_Planting_Nrate_kgha_mean")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_vals <- ncvar_get(nc, "crop"); crop_vals
  nc_close(nc)
  
  # subset to data for plotting 
  year_sub_vals <- 44 # year 2023
  crop_sub_vals <- c(1, 2, 4, 7, 10) # corn, cotton, soybeans, spring wheat, winter wheat
  
  state_map <- ggplot2::map_data("state")

  ## Total Nrate plots 
  
  ## -- corn 
  corn_fert_rate_subset <- total_rate_data[,,44,1] # corn 2023 
  # filter out NA and suppressed data 
  corn_fert_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(fert_Nrate_kgha = as.vector(corn_fert_rate_subset)) %>% 
    dplyr::filter(!is.na(fert_Nrate_kgha) & fert_Nrate_kgha != -9998)
  # make plot 
  corn_fert <- ggplot() +
    geom_tile(data = corn_fert_rate_df,
              aes(x = Lon, y = Lat, fill = fert_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total Fertilizer N rate kg/ha", option = "plasma", 
                         limits = c(0,300)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Corn",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  corn_fert
  
  ## -- cotton 
  cotton_fert_rate_subset <- total_rate_data[,,44,2] # cotton 2023 
  # filter out NA and suppressed data 
  cotton_fert_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(fert_Nrate_kgha = as.vector(cotton_fert_rate_subset)) %>% 
    dplyr::filter(!is.na(fert_Nrate_kgha) & fert_Nrate_kgha != -9998)
  # make plot 
  cotton_fert <- ggplot() +
    geom_tile(data = cotton_fert_rate_df,
              aes(x = Lon, y = Lat, fill = fert_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total Fertilizer N rate kg/ha", option = "plasma", 
                         limits = c(0,300)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Cotton",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  cotton_fert
  
  ## -- soybean 
  soybean_fert_rate_subset <- total_rate_data[,,44,4] # soybean 2023 
  # filter out NA and suppressed data 
  soybean_fert_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(fert_Nrate_kgha = as.vector(soybean_fert_rate_subset)) %>% 
    dplyr::filter(!is.na(fert_Nrate_kgha) & fert_Nrate_kgha != -9998)
  # make plot 
  soybean_fert <- ggplot() +
    geom_tile(data = soybean_fert_rate_df,
              aes(x = Lon, y = Lat, fill = fert_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total Fertilizer N rate kg/ha", option = "plasma", 
                         limits = c(0,300)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Soybeans",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  soybean_fert
  
  ## -- spring wheat 
  spring_wheat_fert_rate_subset <- total_rate_data[,,44,7] # spring_wheat 2023 
  # filter out NA and suppressed data 
  spring_wheat_fert_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(fert_Nrate_kgha = as.vector(spring_wheat_fert_rate_subset)) %>% 
    dplyr::filter(!is.na(fert_Nrate_kgha) & fert_Nrate_kgha != -9998)
  # make plot 
  spring_wheat_fert <- ggplot() +
    geom_tile(data = spring_wheat_fert_rate_df,
              aes(x = Lon, y = Lat, fill = fert_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total Fertilizer N rate kg/ha", option = "plasma", 
                         limits = c(0,300)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Spring Wheat",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  spring_wheat_fert
  
  ## -- winter wheat 
  winter_wheat_fert_rate_subset <- total_rate_data[,,44,10] # winter_wheat 2023 
  # filter out NA and suppressed data 
  winter_wheat_fert_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(fert_Nrate_kgha = as.vector(winter_wheat_fert_rate_subset)) %>% 
    dplyr::filter(!is.na(fert_Nrate_kgha) & fert_Nrate_kgha != -9998)
  # make plot 
  winter_wheat_fert <- ggplot() +
    geom_tile(data = winter_wheat_fert_rate_df,
              aes(x = Lon, y = Lat, fill = fert_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total Fertilizer N rate kg/ha", option = "plasma", 
                         limits = c(0,300)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Winter Wheat",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  winter_wheat_fert
  
  # make legend 
  leg <- get_legend(ggplot() +
                      geom_tile(data = winter_wheat_fert_rate_df,
                                aes(x = Lon, y = Lat, fill = fert_Nrate_kgha), alpha = 0.8) +
                      geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
                      scale_fill_viridis_c(name = "Total Fertilizer N Rate kg/ha", option = "plasma", 
                                           limits = c(0,300)) +
                      coord_fixed(1.3) +
                      labs(
                        title = "2023 Winter Wheat",
                        x = "Longitude",
                        y = "Latitude"
                      ) +
                      theme_minimal())
  
  colz <- viridis(n = 5, option = "turbo")
  
  fert_map <- plot_grid(corn_fert, cotton_fert, 
                        soybean_fert, spring_wheat_fert, 
                        winter_wheat_fert, leg, ncol = 2)
  
  fert_map
  
}

#######################
## Fertilizer Timing ##
#######################

# Line plot of crop-specific fertilizer N rates in each timing category from 1980-2023
{
  nc <- nc_open("fertilizer_Nrate.nc")
  print(nc)
  total_rate_data <- ncvar_get(nc, "Total_Nrate_kgha_mean")
  fall_rate_data <- ncvar_get(nc, "Fall_Nrate_kgha_mean")
  spring_rate_data <- ncvar_get(nc, "Spring_Nrate_kgha_mean")
  planting_rate_data <- ncvar_get(nc, "Planting_Nrate_kgha_mean")
  after_planting_rate_data <- ncvar_get(nc, "After_Planting_Nrate_kgha_mean")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_vals <- ncvar_get(nc, "crop"); crop_vals
  nc_close(nc)
  
  crop_rates_over_time <- data.frame(year = rep(1980:2023,5), 
                                     crop = c(rep("Corn", 44), 
                                              rep("Cotton",44), 
                                              rep("Soybean",44), 
                                              rep("Spring Wheat", 44), 
                                              rep("Winter Wheat", 44)), 
                                     total_rate = NA, 
                                     fall_rate = NA, 
                                     spring_rate = NA,
                                     planting_rate = NA, 
                                     after_planting_rate = NA)

  i <- 1 # for indexing 
  for(c in crop_sub_vals){
    for(y in 1:44){
      
      # total rate 
      dat <- total_rate_data[,,y,c]
      df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(total = as.vector(dat)) %>% 
        dplyr::filter(!is.na(total) & total != -9998)
      # average over all the grid cells
      total <- mean(df$total)
      
      # fall rate 
      fall_dat <- fall_rate_data[,,y,c]
      fall_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(fall = as.vector(fall_dat)) %>% 
        dplyr::filter(!is.na(fall) & fall != -9998)
      # average over all the grid cells
      fall <- mean(fall_df$fall)
      
      # spring rate 
      spring_dat <- spring_rate_data[,,y,c]
      spring_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(spring = as.vector(spring_dat)) %>% 
        dplyr::filter(!is.na(spring) & spring != -9998)
      # average over all the grid cells
      spring <- mean(spring_df$spring)
      
      # planting rate 
      planting_dat <- planting_rate_data[,,y,c]
      planting_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(planting = as.vector(planting_dat)) %>% 
        dplyr::filter(!is.na(planting) & planting != -9998)
      # average over all the grid cells
      planting <- mean(planting_df$planting)
      
      # after planting rate 
      after_planting_dat <- after_planting_rate_data[,,y,c]
      after_planting_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(after_planting = as.vector(after_planting_dat)) %>% 
        dplyr::filter(!is.na(after_planting) & after_planting != -9998)
      # average over all the grid cells
      after_planting <- mean(after_planting_df$after_planting)
      
      crop_rates_over_time$total_rate[i] <- total
      crop_rates_over_time$fall_rate[i] <- fall
      crop_rates_over_time$spring_rate[i] <- spring
      crop_rates_over_time$planting_rate[i] <- planting
      crop_rates_over_time$after_planting_rate[i] <- after_planting
      i <- i + 1
      
    }
  }
  
  
  total_plot <- ggplot(data = crop_rates_over_time) + 
    geom_line(aes(x = year, y = total_rate, col = crop), linewidth = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Fertilizer Rate (kg N/ha)", limits = c(0,180)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Total")
  
  fall_plot <- ggplot(data = crop_rates_over_time) + 
    geom_line(aes(x = year, y = fall_rate, col = crop), linewidth = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Fertilizer Rate (kg N/ha)", limits = c(0,80)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Fall")
  
  spring_plot <- ggplot(data = crop_rates_over_time) + 
    geom_line(aes(x = year, y = spring_rate, col = crop), linewidth = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Fertilizer Rate (kg N/ha)", limits = c(0,80)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Spring")
  
  planting_plot <- ggplot(data = crop_rates_over_time) + 
    geom_line(aes(x = year, y = planting_rate, col = crop), linewidth = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Fertilizer Rate (kg N/ha)", limits = c(0,80)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Planting")
  
  after_planting_plot <- ggplot(data = crop_rates_over_time) + 
    geom_line(aes(x = year, y = after_planting_rate, col = crop), linewidth = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Fertilizer Rate (kg N/ha)", limits = c(0,80)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("After Planting")
  
  leg2 <- get_legend(ggplot(data = crop_rates_over_time) + 
                       geom_line(aes(x = year, y = after_planting_rate, col = crop), linewidth = 1) + 
                       scale_color_manual("Crop", values = colz) + 
                       theme_minimal() + 
                       scale_y_continuous("After Planting Fertilizer Rate (kg N/ha)") + 
                       scale_x_continuous("Year"))
  
  time_plot <- plot_grid(total_plot, fall_plot, spring_plot, 
                         planting_plot, after_planting_plot, leg2, ncol = 2)
  time_plot

}

####################
## Manure N Rates ##
####################

# Map of 2023 total manure N rate across CONUS by crop 
{
  nc <- nc_open("manure_Nrate.nc")
  print(nc)
  total_rate_data <- ncvar_get(nc, "Total_manure_Nrate_kgha_mean")
  fall_rate_data <- ncvar_get(nc, "Fall_manure_Nrate_kgha_mean")
  spring_rate_data <- ncvar_get(nc, "Spring_manure_Nrate_kgha_mean")
  after_planting_rate_data <- ncvar_get(nc, "After_Planting_manure_Nrate_kgha_mean")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_vals <- ncvar_get(nc, "crop"); crop_vals 
  nc_close(nc)
  
  year_sub_vals <- 44 # year 2023
  crop_sub_vals <- c(1, 2, 4, 7, 10) # corn, cotton, soybeans, spring wheat, winter wheat
  
  state_map <- ggplot2::map_data("state")
  
  ## -- corn 
  corn_manure_rate_subset <- total_rate_data[,,44,1] # corn 2023 
  # filter out NA and suppressed data 
  corn_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_Nrate_kgha = as.vector(corn_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_Nrate_kgha) & manure_Nrate_kgha != -9998)
  # make plot 
  range(corn_manure_rate_df$manure_Nrate_kgha)
  corn_manure <- ggplot() +
    geom_tile(data = corn_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure N rate kg/ha", option = "plasma", 
                         limits = c(0,100)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Corn",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  corn_manure
  
  ## -- cotton 
  cotton_manure_rate_subset <- total_rate_data[,,44,2] # cotton 2023 
  # filter out NA and suppressed data 
  cotton_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_Nrate_kgha = as.vector(cotton_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_Nrate_kgha) & manure_Nrate_kgha != -9998)
  # make plot 
  cotton_manure <- ggplot() +
    geom_tile(data = cotton_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure N rate kg/ha", option = "plasma", 
                         limits = c(0,100)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Cotton",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  cotton_manure
  
  ## -- soybean 
  soybean_manure_rate_subset <- total_rate_data[,,44,4] # soybean 2023 
  # filter out NA and suppressed data 
  soybean_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_Nrate_kgha = as.vector(soybean_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_Nrate_kgha) & manure_Nrate_kgha != -9998)
  # make plot 
  soybean_manure <- ggplot() +
    geom_tile(data = soybean_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure N rate kg/ha", option = "plasma", 
                         limits = c(0,100)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Soybeans",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  soybean_manure
  
  ## -- spring wheat 
  spring_wheat_manure_rate_subset <- total_rate_data[,,44,7] # spring_wheat 2023 
  # filter out NA and suppressed data 
  spring_wheat_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_Nrate_kgha = as.vector(spring_wheat_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_Nrate_kgha) & manure_Nrate_kgha != -9998)
  # make plot 
  spring_wheat_manure <- ggplot() +
    geom_tile(data = spring_wheat_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure N rate kg/ha", option = "plasma", 
                         limits = c(0,100)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Spring Wheat",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  spring_wheat_manure
  
  ## -- winter wheat 
  winter_wheat_manure_rate_subset <- total_rate_data[,,44,10] # winter_wheat 2023 
  # filter out NA and suppressed data 
  winter_wheat_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_Nrate_kgha = as.vector(winter_wheat_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_Nrate_kgha) & manure_Nrate_kgha != -9998)
  # make plot 
  winter_wheat_manure <- ggplot() +
    geom_tile(data = winter_wheat_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_Nrate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure N rate kg/ha", option = "plasma", 
                         limits = c(0,100)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Winter Wheat",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  winter_wheat_manure
  
  # make legend 
  leg <- get_legend(ggplot() +
                      geom_tile(data = winter_wheat_manure_rate_df,
                                aes(x = Lon, y = Lat, fill = manure_Nrate_kgha), alpha = 0.8) +
                      geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
                      scale_fill_viridis_c(name = "Total Manure N Rate kg/ha", option = "plasma", 
                                           limits = c(0,100)) +
                      coord_fixed(1.3) +
                      labs(
                        title = "2023 Winter Wheat",
                        x = "Longitude",
                        y = "Latitude"
                      ) +
                      theme_minimal())
  
  colz <- viridis(n = 5, option = "turbo")
  
  manure_map <- plot_grid(corn_manure, cotton_manure, 
                          soybean_manure, spring_wheat_manure, 
                          winter_wheat_manure, leg, ncol = 2)
  
  manure_map

}

#####################
## Manure N Timing ##
#####################

# Line plot of crop-specific manure N rates in each timing category from 1980-2023
{
  nc <- nc_open("manure_Nrate.nc")
  print(nc)
  total_rate_data <- ncvar_get(nc, "Total_manure_Nrate_kgha_mean")
  fall_rate_data <- ncvar_get(nc, "Fall_manure_Nrate_kgha_mean")
  spring_rate_data <- ncvar_get(nc, "Spring_manure_Nrate_kgha_mean")
  after_planting_rate_data <- ncvar_get(nc, "After_Planting_manure_Nrate_kgha_mean")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_vals <- ncvar_get(nc, "crop"); crop_vals 
  nc_close(nc)
  
  manure_crop_rates_over_time <- data.frame(year = rep(1980:2023,5), 
                                            crop = c(rep("Corn", 44), 
                                                     rep("Cotton",44), 
                                                     rep("Soybean",44), 
                                                     rep("Spring Wheat", 44), 
                                                     rep("Winter Wheat", 44)), 
                                            total_rate = NA, 
                                            fall_rate = NA, 
                                            spring_rate = NA,
                                            after_planting_rate = NA)
  
  i <- 1 # for indexing 
  for(c in crop_sub_vals){
    for(y in 1:44){
      
      # total rate 
      dat <- total_rate_data[,,y,c]
      df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(total = as.vector(dat)) %>% 
        dplyr::filter(!is.na(total) & total != -9998)
      # average over all the grid cells
      total <- mean(df$total)
      
      # fall rate 
      fall_dat <- fall_rate_data[,,y,c]
      fall_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(fall = as.vector(fall_dat)) %>% 
        dplyr::filter(!is.na(fall) & fall != -9998)
      # average over all the grid cells
      fall <- mean(fall_df$fall)
      
      # spring rate 
      spring_dat <- spring_rate_data[,,y,c]
      spring_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(spring = as.vector(spring_dat)) %>% 
        dplyr::filter(!is.na(spring) & spring != -9998)
      # average over all the grid cells
      spring <- mean(spring_df$spring)
      
      # after planting rate 
      after_planting_dat <- after_planting_rate_data[,,y,c]
      after_planting_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(after_planting = as.vector(after_planting_dat)) %>% 
        dplyr::filter(!is.na(after_planting) & after_planting != -9998)
      # average over all the grid cells
      after_planting <- mean(after_planting_df$after_planting)
      
      manure_crop_rates_over_time$total_rate[i] <- total
      manure_crop_rates_over_time$fall_rate[i] <- fall
      manure_crop_rates_over_time$spring_rate[i] <- spring
      manure_crop_rates_over_time$after_planting_rate[i] <- after_planting
      i <- i + 1
      
    }
  }
  
  total_plot <- ggplot(data = manure_crop_rates_over_time) + 
    geom_line(aes(x = year, y = total_rate, col = crop), size = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Manure Rate (kg N/ha)", limits = c(0,30)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Total")
  
  fall_plot <- ggplot(data = manure_crop_rates_over_time) + 
    geom_line(aes(x = year, y = fall_rate, col = crop), size = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Manure Rate (kg N/ha)", limits = c(0,30)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Fall")
  
  spring_plot <- ggplot(data = manure_crop_rates_over_time) + 
    geom_line(aes(x = year, y = spring_rate, col = crop), size = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Manure Rate (kg N/ha)", limits = c(0,30)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Spring")
  
  after_planting_plot <- ggplot(data = manure_crop_rates_over_time) + 
    geom_line(aes(x = year, y = after_planting_rate, col = crop), size = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Manure Rate (kg N/ha)", limits = c(0,30)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("After Planting")
  
  leg2 <- get_legend(ggplot(data = manure_crop_rates_over_time) + 
                       geom_line(aes(x = year, y = after_planting_rate, col = crop), size = 1) + 
                       scale_color_manual("Crop", values = colz) + 
                       theme_minimal() + 
                       scale_y_continuous("After Planting Manure Rate (kg N/ha)") + 
                       scale_x_continuous("Year"))
  
  time_plot <- plot_grid(total_plot, fall_plot, spring_plot, 
                         after_planting_plot, ncol = 2)
  
  time_plot_leg <- plot_grid(time_plot, leg2, rel_widths = c(4,1))
  time_plot_leg
}

####################
## Manure C Rates ##
####################

# Map of 2023 total manure C rate across CONUS by crop 
{
  nc <- nc_open("manure_C_rate.nc")
  print(nc)
  total_rate_data <- ncvar_get(nc, "Total_manure_C_rate_kgha_mean")
  fall_rate_data <- ncvar_get(nc, "Fall_manure_C_rate_kgha_mean")
  spring_rate_data <- ncvar_get(nc, "Spring_manure_C_rate_kgha_mean")
  after_planting_rate_data <- ncvar_get(nc, "After_Planting_manure_C_rate_kgha_mean")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_vals <- ncvar_get(nc, "crop"); crop_vals 
  nc_close(nc)
  
  year_sub_vals <- 44 # year 2023
  crop_sub_vals <- c(1, 2, 4, 7, 10) # corn, cotton, soybeans, spring wheat, winter wheat
  
  state_map <- ggplot2::map_data("state")
  
  ## -- corn 
  corn_manure_rate_subset <- total_rate_data[,,44,1] # corn 2023 
  # filter out NA and suppressed data 
  corn_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_C_rate_kgha = as.vector(corn_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_C_rate_kgha) & manure_C_rate_kgha != -9998)
  # make plot 
  range(corn_manure_rate_df$manure_C_rate_kgha)
  corn_manure <- ggplot() +
    geom_tile(data = corn_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_C_rate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure C rate kg/ha", option = "plasma", 
                         limits = c(0, 1500)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Corn",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  corn_manure
  
  ## -- cotton 
  cotton_manure_rate_subset <- total_rate_data[,,44,2] # cotton 2023 
  # filter out NA and suppressed data 
  cotton_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_C_rate_kgha = as.vector(cotton_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_C_rate_kgha) & manure_C_rate_kgha != -9998)
  # make plot 
  cotton_manure <- ggplot() +
    geom_tile(data = cotton_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_C_rate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure C rate kg/ha", option = "plasma", 
                         limits = c(0,1500)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Cotton",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  cotton_manure
  
  ## -- soybean 
  soybean_manure_rate_subset <- total_rate_data[,,44,4] # soybean 2023 
  # filter out NA and suppressed data 
  soybean_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_C_rate_kgha = as.vector(soybean_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_C_rate_kgha) & manure_C_rate_kgha != -9998)
  # make plot 
  soybean_manure <- ggplot() +
    geom_tile(data = soybean_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_C_rate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure C rate kg/ha", option = "plasma", 
                         limits = c(0,1500)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Soybeans",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  soybean_manure
  
  ## -- spring wheat 
  spring_wheat_manure_rate_subset <- total_rate_data[,,44,7] # spring_wheat 2023 
  # filter out NA and suppressed data 
  spring_wheat_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_C_rate_kgha = as.vector(spring_wheat_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_C_rate_kgha) & manure_C_rate_kgha != -9998)
  # make plot 
  spring_wheat_manure <- ggplot() +
    geom_tile(data = spring_wheat_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_C_rate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure C rate kg/ha", option = "plasma", 
                         limits = c(0,1500)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Spring Wheat",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  spring_wheat_manure
  
  ## -- winter wheat 
  winter_wheat_manure_rate_subset <- total_rate_data[,,44,10] # winter_wheat 2023 
  # filter out NA and suppressed data 
  winter_wheat_manure_rate_df <- expand.grid(
    Lat = lat_vals,
    Lon = lon_vals) %>%
    mutate(manure_C_rate_kgha = as.vector(winter_wheat_manure_rate_subset)) %>% 
    dplyr::filter(!is.na(manure_C_rate_kgha) & manure_C_rate_kgha != -9998)
  # make plot 
  winter_wheat_manure <- ggplot() +
    geom_tile(data = winter_wheat_manure_rate_df,
              aes(x = Lon, y = Lat, fill = manure_C_rate_kgha), alpha = 0.8) +
    geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
    scale_fill_viridis_c(name = "Total manure C rate kg/ha", option = "plasma", 
                         limits = c(0,1500)) +
    coord_fixed(1.3) +
    labs(
      title = "2023 Winter Wheat",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() + 
    theme(legend.position = "none")
  # view 
  winter_wheat_manure
  
  # make legend 
  leg <- get_legend(ggplot() +
                      geom_tile(data = winter_wheat_manure_rate_df,
                                aes(x = Lon, y = Lat, fill = manure_C_rate_kgha), alpha = 0.8) +
                      geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
                      scale_fill_viridis_c(name = "Total Manure C Rate kg/ha", option = "plasma", 
                                           limits = c(0,1500)) +
                      coord_fixed(1.3) +
                      labs(
                        title = "2023 Winter Wheat",
                        x = "Longitude",
                        y = "Latitude"
                      ) +
                      theme_minimal())
  
  colz <- viridis(n = 5, option = "turbo")
  
  manure_map <- plot_grid(corn_manure, cotton_manure, 
                          soybean_manure, spring_wheat_manure, 
                          winter_wheat_manure, leg, ncol = 2)
  
  manure_map
  
}

#####################
## Manure C Timing ##
#####################

# Line plot of crop-specific manure C rates in each timing category from 1980-2023
{
  nc <- nc_open("manure_C_rate.nc")
  print(nc)
  total_rate_data <- ncvar_get(nc, "Total_manure_C_rate_kgha_mean")
  fall_rate_data <- ncvar_get(nc, "Fall_manure_C_rate_kgha_mean")
  spring_rate_data <- ncvar_get(nc, "Spring_manure_C_rate_kgha_mean")
  after_planting_rate_data <- ncvar_get(nc, "After_Planting_manure_C_rate_kgha_mean")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_vals <- ncvar_get(nc, "crop"); crop_vals 
  nc_close(nc)
  
  manure_crop_rates_over_time <- data.frame(year = rep(1980:2023,5), 
                                            crop = c(rep("Corn", 44), 
                                                     rep("Cotton",44), 
                                                     rep("Soybean",44), 
                                                     rep("Spring Wheat", 44), 
                                                     rep("Winter Wheat", 44)), 
                                            total_rate = NA, 
                                            fall_rate = NA, 
                                            spring_rate = NA,
                                            after_planting_rate = NA)
  
  i <- 1 # for indexing 
  for(c in crop_sub_vals){
    for(y in 1:44){
      
      # total rate 
      dat <- total_rate_data[,,y,c]
      df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(total = as.vector(dat)) %>% 
        dplyr::filter(!is.na(total) & total != -9998)
      # average over all the grid cells
      total <- mean(df$total)
      
      # fall rate 
      fall_dat <- fall_rate_data[,,y,c]
      fall_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(fall = as.vector(fall_dat)) %>% 
        dplyr::filter(!is.na(fall) & fall != -9998)
      # average over all the grid cells
      fall <- mean(fall_df$fall)
      
      # spring rate 
      spring_dat <- spring_rate_data[,,y,c]
      spring_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(spring = as.vector(spring_dat)) %>% 
        dplyr::filter(!is.na(spring) & spring != -9998)
      # average over all the grid cells
      spring <- mean(spring_df$spring)
      
      # after planting rate 
      after_planting_dat <- after_planting_rate_data[,,y,c]
      after_planting_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(after_planting = as.vector(after_planting_dat)) %>% 
        dplyr::filter(!is.na(after_planting) & after_planting != -9998)
      # average over all the grid cells
      after_planting <- mean(after_planting_df$after_planting)
      
      manure_crop_rates_over_time$total_rate[i] <- total
      manure_crop_rates_over_time$fall_rate[i] <- fall
      manure_crop_rates_over_time$spring_rate[i] <- spring
      manure_crop_rates_over_time$after_planting_rate[i] <- after_planting
      i <- i + 1
      
    }
  }
  
  total_plot <- ggplot(data = manure_crop_rates_over_time) + 
    geom_line(aes(x = year, y = total_rate, col = crop), size = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Manure Rate (kg C/ha)", limits = c(0,300)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Total")
  
  fall_plot <- ggplot(data = manure_crop_rates_over_time) + 
    geom_line(aes(x = year, y = fall_rate, col = crop), size = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Manure Rate (kg C/ha)", limits = c(0,300)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Fall")
  
  spring_plot <- ggplot(data = manure_crop_rates_over_time) + 
    geom_line(aes(x = year, y = spring_rate, col = crop), size = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Manure Rate (kg C/ha)", limits = c(0,300)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("Spring")
  
  after_planting_plot <- ggplot(data = manure_crop_rates_over_time) + 
    geom_line(aes(x = year, y = after_planting_rate, col = crop), size = 1) + 
    scale_color_manual("Crop", values = colz) + 
    theme_minimal() + 
    scale_y_continuous("Manure Rate (kg C/ha)", limits = c(0,300)) + 
    scale_x_continuous("Year") + 
    theme(legend.position = "none") + 
    ggtitle("After Planting")
  
  leg2 <- get_legend(ggplot(data = manure_crop_rates_over_time) + 
                       geom_line(aes(x = year, y = after_planting_rate, col = crop), size = 1) + 
                       scale_color_manual("Crop", values = colz) + 
                       theme_minimal() + 
                       scale_y_continuous("After Planting Manure Rate (kg C/ha)") + 
                       scale_x_continuous("Year"))
  
  time_plot <- plot_grid(total_plot, fall_plot, spring_plot, 
                         after_planting_plot, ncol = 2)
  
  time_plot_leg <- plot_grid(time_plot, leg2, rel_widths = c(4,1))
  time_plot_leg
}

#########################
## Manure animal types ##
#########################

# Line plot of proportion of animal types across CONUS from 1980-2023
{
  nc <- nc_open("manure_animal_proportion.nc")
  print(nc)
  proportion_data <- ncvar_get(nc, "manure_animal_proportion")
  animal_mapping <- ncatt_get(nc, 0, "animal_mapping"); animal_mapping
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  animal_vals <- ncvar_get(nc, "animal_type"); animal_vals
  nc_close(nc)
  
  animal_map <- unlist(strsplit(animal_mapping$value, split = ",")); animal_map
  
  animal_avg_props <- matrix(NA, ncol = length(animal_map)+1, nrow = 44)
  animal_avg_props[,1] <- 1980:2023 # fill year column 

  for(animal_num in 1:length(animal_map)){
    
    animal_name <- animal_map[animal_num]; animal_name
    animal_ts <- rep(NA, 44)
    
    for(y in 1:44){
      
      year_name <- year_vals[y]; year_name
      prop_subset <- proportion_data[,,y, animal_num]
      
      # remove NA and suppressed values 
      animal_prop_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(proportion = as.vector(prop_subset)) %>% 
        dplyr::filter(!is.na(proportion) & proportion != -9998)
      
      # create time series vector 
      animal_ts[y] <- mean(animal_prop_df$proportion, na.rm = TRUE)
    }
    
    animal_avg_props[,1+animal_num] <- animal_ts
    
  }
  
  animal_avg_props <- as.data.frame(animal_avg_props)
  names(animal_avg_props) <- c("Year", "Beef", "Biosolids", "Dairy", "Horse",
                               "Poultry", "Sheep", "Swine")
  
  head(animal_avg_props)
  
  animal_avg_df <- animal_avg_props %>% 
    pivot_longer(cols = c("Beef", "Biosolids", "Dairy", "Horse",
                          "Poultry", "Sheep", "Swine"), 
                 names_to = "Animal", 
                 values_to = "Proportion")
  
  animal <- ggplot(animal_avg_df) + 
    geom_line(aes(x = Year, y = Proportion, col = Animal), linewidth = 1) + 
    scale_color_viridis_d(name = "Animal", option = "turbo") + 
    scale_y_continuous("Proportion", limits = c(0, .5)) + 
    theme_minimal() + 
    theme(text = element_text(size = 16))
  
  animal

}

#####################
## Tillage Systems ##
#####################

# Maps of proportion of tillage system types (FT, NT, RT) in 1980, 2000, and 2020 for each crop group 
{
  nc <- nc_open("tillage_systems.nc")
  print(nc)
  ct_data <- ncvar_get(nc, "CT_proportion_mean")
  nt_data <- ncvar_get(nc, "NT_proportion_mean")
  rt_data <- ncvar_get(nc, "RT_proportion_mean")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  crop_group_vals <- ncvar_get(nc, "crop_group"); crop_group_vals 
  nc_close(nc)
  
  # subset to data for plotting 
  year_sub <- c(1, 21, 41) # 1980, 2000, 2020
  crop_groups = c("low residue", "row crops", "small grains")
  tillage_system_map_list <- list()
  
  for(c in 1:length(crop_groups)){
    
    crop_group_name <- crop_groups[c]; crop_group_name
    
    ct_list <- list()
    i <- 1 # indexing 
    for(y in year_sub){
      
      year_name <- year_vals[y]; year_name
      
      ct_subset <- ct_data[,,y,c]
      
      ct_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(CT_proportion = as.vector(ct_subset)) %>% 
        dplyr::filter(!is.na(CT_proportion) & CT_proportion != -9998)
      
      # tillage proportions
      ct_list[[i]] <- ggplot() +
        geom_tile(data = ct_df %>% dplyr::filter(!is.na(CT_proportion)),
                  aes(x = Lon, y = Lat, fill = CT_proportion), alpha = 0.8) +
        geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
        scale_fill_viridis_c(name = "Proportion", option = "plasma", 
                             limits = c(0,1)) +
        coord_fixed(1.3) +
        labs(
          title = paste0(year_name, " FT ", crop_group_name),
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() + 
        theme(legend.position = "none", 
              plot.title = element_text(size = 10))
      
      i <- i + 1
      
    }
    
    # RT 
    rt_list <- list()
    i <- 1
    for(y in year_sub){
      
      year_name <- year_vals[y]; year_name
      
      rt_subset <- rt_data[,,y,c]
      
      rt_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(RT_proportion = as.vector(rt_subset)) %>% 
        dplyr::filter(!is.na(RT_proportion) & RT_proportion != -9998)
      
      # tillage proportions
      rt_list[[i]] <- ggplot() +
        geom_tile(data = rt_df %>% dplyr::filter(!is.na(RT_proportion)),
                  aes(x = Lon, y = Lat, fill = RT_proportion), alpha = 0.8) +
        geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
        scale_fill_viridis_c(name = "Proportion", option = "plasma", 
                             limits = c(0,1)) +
        coord_fixed(1.3) +
        labs(
          title = paste0(year_name, " RT ", crop_group_name),
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() + 
        theme(legend.position = "none", 
              plot.title = element_text(size = 10))
      
      i <- i + 1
      
    }
    
    # NT 
    nt_list <- list()
    i <- 1
    for(y in year_sub){
      
      year_name <- year_vals[y]; year_name
      
      nt_subset <- nt_data[,,y,c]
      
      nt_df <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(NT_proportion = as.vector(nt_subset)) %>% 
        dplyr::filter(!is.na(NT_proportion) & NT_proportion != -9998)
      
      # tillage proportions
      nt_list[[i]] <- ggplot() +
        geom_tile(data = nt_df %>% dplyr::filter(!is.na(NT_proportion)),
                  aes(x = Lon, y = Lat, fill = NT_proportion), alpha = 0.8) +
        geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
        scale_fill_viridis_c(name = "Proportion", option = "plasma", 
                             limits = c(0,1)) +
        coord_fixed(1.3) +
        labs(
          title = paste0(year_name, " NT ", crop_group_name),
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() + 
        theme(legend.position = "none", 
              plot.title = element_text(size = 10))
      
      i <- i + 1
      
    }
    
    till_plots <- plot_grid(nt_list[[1]], nt_list[[2]], nt_list[[3]],
                            rt_list[[1]], rt_list[[2]], rt_list[[3]],
                            ct_list[[1]], ct_list[[2]], ct_list[[3]])
    
    leg <- get_legend(ggplot() +
                        geom_tile(data = ct_df %>% dplyr::filter(!is.na(CT_proportion)),
                                  aes(x = Lon, y = Lat, fill = CT_proportion), alpha = 0.8) +
                        geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
                        scale_fill_viridis_c(name = "Proportion", option = "plasma") +
                        coord_fixed(1.3) +
                        theme_minimal())
    
    till_system_map <- plot_grid(till_plots, leg, rel_widths = c(9,1))
    tillage_system_map_list[[c]] <- till_system_map
    
  }
  
  # view plots 
  tillage_system_map_list[[1]] # low resiude
  tillage_system_map_list[[2]] # row crops
  tillage_system_map_list[[3]] # small grains 
}

#####################################
## Tillage Intensity Classes (A-K) ##
#####################################

# Stacked bar chart of proportion of tillage intensity classes (A-K) for each operating window in 2023
{
  # prep the data frame 
  op_groups <- c("field_prep", "before_planting", 
                 "planting", "after_planting", "post_harvest")
  
  tillak_df <- data.frame(op_window = c(rep(op_groups[1], 13), 
                                        rep(op_groups[2], 13), 
                                        rep(op_groups[3], 13), 
                                        rep(op_groups[4], 13), 
                                        rep(op_groups[5], 13)), 
                          intensity = rep(c("A", "B", "C", "D", 
                                            "E", "EGS", "F", "G", "H", 
                                            "I", "J", "K", "no_tillage"), 5), 
                          proportion = NA)
  
  # read in netcdf file 
  nc <- nc_open("tillage_intensity.nc")
  print(nc)
  
  # use 1 year of data 2023
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  op_vals <- ncvar_get(nc, "operating_window"); op_vals
  
  global_att <- ncatt_get(nc, varid = 0)
  op_map <- global_att$operating_window_mapping; op_map
  op_groups <- strsplit(op_map, ",")[[1]]; op_groups
  
  for(op_idx in 1:length(op_groups)){
    
    op_win <- op_groups[op_idx]; op_win
    
    for(intensity_class in c("A", "B", "C", "D",
                             "EGS", "E", "F", "G", "H",
                             "I", "J", "K", "no_tillage")){
      
      
      till_dat <- ncvar_get(nc, paste0(intensity_class, "_proportion"))
      
      dat <- till_dat[,,44,op_idx] # year 2023
      # filter out NA and suppressed data
      dat_filter <- expand.grid(
        Lat = lat_vals,
        Lon = lon_vals) %>%
        mutate(proportion = as.vector(dat)) %>%
        dplyr::filter(!is.na(proportion) & proportion != -9998)
      
      mean_prop <- mean(dat_filter$proportion); mean_prop
      
      tillak_df <- tillak_df %>%
        dplyr::mutate(proportion = case_when(op_window == op_win &
                                               intensity == intensity_class ~ mean_prop,
                                             TRUE ~ proportion))
    }
  }
  
  nc_close(nc) # close file 
  
  # stacked bar chart of A-K for each op_group
  tillak_df$op_window <- factor(tillak_df$op_window,
                                levels = c("field_prep",
                                           "before_planting",
                                           "planting",
                                           "after_planting",
                                           "post_harvest"))
  
  tillak_plot <- ggplot(data = tillak_df %>% dplyr::filter(intensity != "no_tillage")) +
    geom_bar(stat = "identity",
             aes(x = op_window, y = proportion, fill = intensity)) +
    scale_fill_viridis_d("Tillage Intensity", option = "turbo") + 
    theme_minimal() + 
    scale_y_continuous("Proportion", breaks = seq(0,1, by = 0.2)) + 
    scale_x_discrete("", labels = c("Field Prep", "Before Planting", 
                                    "Planting", "After Planting", 
                                    "Post Harvest")) + 
    theme(text = element_text(size = 14))
  
  # remaining proportion is no tillage 
  tillak_plot 
  
}

#################
## Cover crops ##
#################

# Map of cover crop proportions in each grid cell for 1990, 2000, 2010, and 2020
{
  
  nc <- nc_open("covercrop_proportion.nc")
  print(nc)
  cc_mean_data <- ncvar_get(nc, "covercrop_proportion_mean")
  lat_vals <- ncvar_get(nc, "lat"); lat_vals
  lon_vals <- ncvar_get(nc, "lon"); lon_vals
  year_vals <- ncvar_get(nc, "time"); year_vals
  nc_close(nc)
  
  # years 1990, 2000, 2010, 2020 
  state_map <- ggplot2::map_data("state")
  year_nums <- c(11, 21, 31, 41)
  year_vals[year_nums]
  plot_cc <- list()
  
  for(y_idx in 1:4){
    
    y <- year_nums[y_idx]
    
    cc_subset <- cc_mean_data[,,y]
    
    cc_prop_df <- expand.grid(
      Lat = lat_vals,
      Lon = lon_vals) %>%
      mutate(proportion = as.vector(cc_subset)) %>% 
      dplyr::filter(!is.na(proportion) & proportion != -9998)
    
    year_name <- year_vals[y]
    
    # cover crop proportions
    plot_cc[[y_idx]] <- ggplot() +
      geom_tile(data = cc_prop_df,
                aes(x = Lon, y = Lat, fill = proportion), alpha = 0.8, 
                show.legend = FALSE) +
      geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
      scale_fill_viridis_c(name = "Proportion", option = "turbo") +
      coord_fixed(1.3) +
      labs(
        title = paste0(year_name, " Proportion Cover Crops"),
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal() + 
      theme(text = element_text(size = 12))
    
    
  }
  
  leg_cc <- get_legend(ggplot() +
                         geom_tile(data = cc_prop_df,
                                   aes(x = Lon, y = Lat, fill = proportion), alpha = 0.8, 
                                   show.legend = TRUE) +
                         geom_polygon(data=state_map,aes(x=long,y=lat,group=group),color='black',fill=NA) +  # Add US map as the base layer
                         scale_fill_viridis_c(name = "Proportion", option = "turbo") +
                         coord_fixed(1.3) +
                         labs(
                           title = paste0(year_name),
                           x = "Longitude",
                           y = "Latitude"
                         ) +
                         theme_minimal())
  
  plots <- plot_grid(plot_cc[[1]], plot_cc[[2]], plot_cc[[3]], plot_cc[[4]], 
                     nrow = 2)
  
  
  plot_fin <- plot_grid(plots, leg_cc, rel_widths = c(5, 1))
  plot_fin

}

## end -------------------------------------------------------------------------

