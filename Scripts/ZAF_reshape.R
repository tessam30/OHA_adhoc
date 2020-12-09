# Purpose: Reshape data long to use with GIS
# Author: Tim E. & Mashudu MD
# Date: 2020-10-14
# Notes:



# Libraries needed --------------------------------------------------------

library(readxl)
library(tidyverse)
library(tidyr)
library(sf)
library(glamr)
library(glitr)
library(gisr)
library(scales)


# LOAD DATA ---------------------------------------------------------------

    df <- read_excel("../../../Downloads/District Cascade.xlsx") %>% 
        mutate(District = if_else(District == "kz King Cetshwayo District Municipality", 
                                  "kz Uthungulu District Municipality", District))
    
    # Fix the name of one of the Districts
    shp <- sf::st_read("../../../Documents/GEODATA/PEPFAR/SouthAfricaDistrictLsib2016July.shp")
    #     mutate(
    #         name = ifelse(name == "kz Uthungulu District Municipality", "kz King Cetshwayo District Municipality", name),
    #         level5name = ifelse(name == "kz Uthungulu District Municipality", "kz King Cetshwayo District Municipality", level5name)
    #     )
    # 


    # Compare districts
    setdiff(
        df %>% count(District) %>% pull(District),
        shp %>% count(name, name = "District") %>% st_drop_geometry() %>% pull(name)
            )
    
    # Reshape district casecade wide
    
        df_wide <-
            df %>% 
            pivot_wider(names_from = Period,
                        names_sep = "_",
                        values_from = HTS_TST:VLS)
    
    
    # Join the long data with shapefile
    sa_wide <- shp %>%
        left_join(df_wide, by = c("name" = "District"))
    
    sa <- shp %>%
        right_join(df, by = c("name" = "District")) %>% 
        mutate(name_fixed = if_else(name == "kz Uthungulu District Municipality", "kz King Cetshwayo District Municipality", name)
        )
        
    
    
    
    sa %>%
        ggplot(aes(geometry = geometry)) +
        geom_sf(aes(fill = HTS_TST)) +
        facet_wrap(~Period) +
        si_style_map() +
        scale_fill_viridis_c(option = "viridis",
                             direction = -1,
                             labels = comma)
    
    sa %>%
        mutate(name = str_remove_all(name, "Metropolitan Municipality*|District Municipality*")) %>%
        ggplot(aes(x = fct_reorder(name, HTS_TST), y = (HTS_TST), fill = log(HTS_TST))) +
        geom_col() +
        coord_flip() +
        facet_wrap(~Period, nrow = 1) +
        scale_fill_viridis_c(option = "viridis",
                             direction = -1,
                             labels =  comma) +
        si_style_xgrid() +
        labs(x = NULL, y = NULL)
    
    st_write(sa, "../../../Downloads/ZAF_district_cascade_long.shp", append = FALSE)
    
    write_csv(df_wide, "../../../Downloads/ZAF_District_cascade_wide.csv")


# REQUEST 2 - 2020-12-09 --------------------------------------------------

    df <- read_excel("Data/District Cascade.xlsx") %>% 
    mutate(District = if_else(District == "kz King Cetshwayo District Municipality", 
                                  "kz Uthungulu District Municipality", District))

    df %>% count(District) %>% prinf()
    
    # Load GIS data
    shp <- sf::st_read("GIS/SouthAfricaDistrictLsib2016July.shp")
    
    # Compare districts
    setdiff(
        df %>% count(District) %>% pull(District),
        shp %>% count(name, name = "District") %>% st_drop_geometry() %>% pull(name)
    )
    
    # Reshape district casecade wide
    df_wide <-
        df %>% 
        pivot_wider(names_from = Period,
                    names_sep = "_",
                    values_from = HTS_TST:VLS)
    
    # Join the long data with shapefile
    sa_wide <- shp %>%
        left_join(df_wide, by = c("name" = "District"))
    
    sa <- shp %>%
        right_join(df, by = c("name" = "District")) %>% 
        mutate(name_fixed = if_else(name == "kz Uthungulu District Municipality", "kz King Cetshwayo District Municipality", name)
        )
    
    st_write(sa, "GIS/ZAF_district_cascade_long.shp", append = FALSE)
    write_csv(df_wide, "Dataout/ZAF_District_cascade_wide.csv")
    
    