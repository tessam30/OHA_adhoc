library(tidyverse)

path_out <- c("~/Documents/USAID/GH - OHA - SIEI - Work/")

df <- tibble::tribble(
  ~indicator, ~USAID, ~PEPFAR, ~share, ~target,
  "TX_CURR", 5.75, 15, 0.383333333, 1L,
  "MMD", 2.1, 5.2, 0.403846154, 1L,
  "TX_PVLS", 0.91, 1, 0.91, 1L,
  "PTMCT_ART", 60000, 174238, 0.34435657, 1L,
  "HTS_TST_POS", 275000, 701860, 0.391816032, 1L,
  "VMMC", 225000, 870058, 0.258603449, 1L
)


df_long <- 
  df %>% pivot_longer(cols = USAID:PEPFAR,
    names_to = "Agency",
    values_to = "Value")

    
    bar_spark <- function(ind) {
      viz <- df_long %>% 
        filter(indicator == {{ind}}, Agency == "USAID") %>% 
        ggplot() + 
        geom_col(aes(Agency, target), fill = "#C0C0C0") +
        geom_col(aes(Agency, share), fill = "#e04745") +
        coord_flip() +
        theme_void() +
        theme(legend.position = "none",
          plot.background = element_rect(fill = "#f3f3f3",
            color = "#f3f3f3"),
          )
      
      ggsave(file.path(path_out, paste0("Q1_sparks", {{ind}}, ".png")),
        plot = viz, dpi = 330, width = 1.25, height = 0.1)
    } 
    
#output for each ind
unique(df_long$indicator) %>% 
  walk(bar_spark)

  

