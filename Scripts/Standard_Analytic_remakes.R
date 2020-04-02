# Replotting



tgrowth <-tibble::tribble(
                ~Time, ~Agency, ~Treatment.Growth, ~linkage,
            "2018 Q1",   "CDC",                NA,     0.83,
            "2018 Q2",   "CDC",              0.02,     0.85,
            "2018 Q3",   "CDC",             -0.01,     0.92,
            "2018 Q4",   "CDC",              0.01,     0.91,
            "2019 Q1",   "CDC",             -0.12,     0.97,
            "2019 Q2",   "CDC",              0.05,     0.97,
            "2019 Q3",   "CDC",              0.06,     0.98,
            "2019 Q4",   "CDC",              0.09,     1.01,
            "2020 Q1",   "CDC",              0.03,     0.94,
            "2018 Q1", "USAID",                NA,     0.71,
            "2018 Q2", "USAID",              0.02,     0.78,
            "2018 Q3", "USAID",              0.01,     0.82,
            "2018 Q4", "USAID",              0.02,     0.86,
            "2019 Q1", "USAID",             -0.14,     0.89,
            "2019 Q2", "USAID",               0.1,     0.91,
            "2019 Q3", "USAID",              0.03,     0.98,
            "2019 Q4", "USAID",              0.11,     0.99,
            "2020 Q1", "USAID",              0.09,     0.98
            ) %>% 
  janitor::clean_names()



tgrowth %>% 
  mutate(label = ifelse(time == "2020 Q1", agency, NA_character_)) %>% 
  filter(time != "2018 Q1") %>% 
ggplot(., aes(x = time, y = `treatment_growth`, group = agency,
  colour = agency)) +
  geom_hline(yintercept = 0, colour = grey20K, linetype = "dotted") +
  geom_ribbon(aes(ymin = 0, ymax = -Inf), fill = grey10K, alpha = 0.25, colour = NA) +
  geom_line(size = 1) +
  geom_point(aes(fill = agency), colour = "white", size = 3, stroke = 2, shape = 21) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("CDC" = grey30K, "USAID" = "#002F6C")) +
  scale_fill_manual(values = c("CDC" = grey30K, "USAID" = "#002F6C")) +
  labs(x = "", y = "",
    title = "USAID and CDC had similar trends in quarterly treatment growth.",
    caption = "Treatment growth = TX_NET_NEW_Current_Q / TX_CURR_Previous_Q") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.15, 0.15)) +
  ggrepel::geom_text_repel(aes(label = label))


tgrowth %>% 
  mutate(label = ifelse(time == "2020 Q1", agency, NA_character_)) %>% 
  ggplot(., aes(x = time, y = linkage, group = agency,
    colour = agency)) +
  geom_hline(yintercept = 0, colour = grey20K, linetype = "dotted") +
  geom_line(size = 1) +
  geom_point(aes(fill = agency), colour = "white", size = 3, stroke = 2, shape = 21) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("CDC" = grey30K, "USAID" = "#002F6C")) +
  scale_fill_manual(values = c("CDC" = grey30K, "USAID" = "#002F6C")) +
  labs(x = "", y = "",
    title = "In FY2019, USAID trends in linkage to treatment closely resembled that of CDC.",
    caption = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
    limits = c(.6, 1.1)) +
  ggrepel::geom_text_repel(aes(label = label), force = 8)


tgrowth %>% 
  mutate(label = ifelse(time == "2020 Q1", agency, NA_character_)) %>% 
  ggplot(., aes(x = time, y = linkage, group = agency,
    colour = agency)) +
  geom_hline(yintercept = 0, colour = grey20K, linetype = "dotted") +
  geom_line(size = 1) +
  geom_point(aes(fill = agency), colour = "white", size = 3, stroke = 2, shape = 21) +
  theme_xaxis() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("CDC" = grey30K, "USAID" = "#002F6C")) +
  scale_fill_manual(values = c("CDC" = grey30K, "USAID" = "#002F6C")) +
  labs(x = "", y = "",
    title = "In FY2019, USAID trends in linkage to treatment closely resembled that of CDC.",
    caption = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 10),
    limits = c(.6, 1.1)) +
  ggrepel::geom_text_repel(aes(label = label), force = 3) +
  ggrepel::geom_text_repel(aes(label = percent(linkage)), force = 10 )



ovc <- tibble::tribble(
                      ~country, ~ovc_hivstat, ~tx_curr, ~group,
                 "South Sudan",          51L,     181L,    "A",
                 "Asia Region",         126L,      63L,    "A",
                     "Burundi",         515L,    1878L,    "A",
                     "Lesotho",        1074L,    5192L,    "B",
                         "DRC",        1520L,    4363L,    "B",
               "Cote D'Ivoire",        1525L,     109L,    "B",
                    "Eswatini",        1925L,    3822L,    "B",
                       "Haiti",        2339L,     443L,    "B",
                     "Namibia",        3035L,    2385L,    "B",
                      "Malawi",        5955L,   22584L,    "B",
                      "Uganda",        9042L,   23889L,    "C",
                     "Nigeria",        9465L,   12915L,    "C",
                    "Zimbabwe",       16961L,   26110L,    "C",
                      "Zambia",       18113L,   18662L,    "C",
                    "Tanzania",       19614L,   22425L,    "C",
                  "Mozambique",       26202L,   12933L,    "D",
                       "Kenya",       41659L,   29106L,    "D",
                "South Africa",       84069L,   68886L,    "D"
              ) %>% 
  janitor::clean_names()

ovc %>% 
  mutate(ovc_cov = ovc_hivstat / tx_curr,
    country_sort = reorder_within(country, tx_curr, group),
    achieved = if_else(ovc_cov >= 1, 1, 0),
    remaining = if_else(ovc_hivstat < tx_curr, (tx_curr - ovc_hivstat), NULL)) %>% 
  ggplot(., aes(x = country_sort)) +
  geom_segment(aes(y = if_else(ovc_hivstat < tx_curr, ovc_hivstat, NULL), 
   yend = tx_curr, xend = country_sort), size = 6, colour = "#f6e4ed") + 
  geom_segment(aes(y = 0, yend = if_else(ovc_hivstat >= tx_curr, tx_curr, ovc_hivstat),
    xend = country_sort), size = 6, colour = grey10K) +
  geom_segment(aes(y = tx_curr, yend = if_else(ovc_hivstat >= tx_curr, ovc_hivstat, NULL),
    xend = country_sort), size = 6, colour = "#d1f4ef") +
  #geom_col(aes(y = ovc_hivstat, fill = factor(achieved)), width = 0.5,) +
  geom_errorbar(aes(ymin = tx_curr, ymax = tx_curr), color = grey50K , width = 0.5, size = 1) +
  geom_text(aes(y = ovc_hivstat, label = percent(ovc_cov, 2)), size = 3,
    hjust = 1) +
  coord_flip() + facet_wrap(~rev(group), scales = "free") +
  scale_x_reordered() +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c(grey10K, "#205540"), guide = FALSE) +
  scale_y_continuous(labels = label_comma()) 



# Remaking tables ---------------------------------------------------------

tabl <- tibble::tribble(
            ~Agency,    ~Indicator, ~Year,   ~Target,   ~Actual,
            "USAID",     "HTS_TST", 2018L,   4177414,   2177266,
            "USAID", "HTS_TST_POS", 2018L,    140875,     69137,
            "USAID",     "TX_CURR", 2018L,    655964,    284998,
            "USAID",      "TX_NEW", 2018L,    118342,     54239,
          "HHS/CDC",     "HTS_TST", 2018L,   5744407,   5184042,
          "HHS/CDC", "HTS_TST_POS", 2018L,    146523,     87741,
          "HHS/CDC",     "TX_CURR", 2018L,    569826,    491995,
          "HHS/CDC",  "TX_NET_NEW", 2018L,    569826,     11788,
          "HHS/CDC",      "TX_NEW", 2018L,    142874,     76826,
              "DOD",     "HTS_TST", 2018L,    147846,    103969,
              "DOD", "HTS_TST_POS", 2018L,      8964,      4392,
              "DOD",     "TX_CURR", 2018L,     32257,     30101,
              "DOD",      "TX_NEW", 2018L,      7308,      3876,
            "USAID",     "HTS_TST", 2019L,   1771411,   1827168,
            "USAID", "HTS_TST_POS", 2019L,     71410,     80640,
            "USAID",     "TX_CURR", 2019L,    369529,    309420,
            "USAID",      "TX_NEW", 2019L,     63076,     77274,
          "HHS/CDC",     "HTS_TST", 2019L,   3426898,   2864947,
          "HHS/CDC", "HTS_TST_POS", 2019L,     91645,     82055,
          "HHS/CDC",     "TX_CURR", 2019L,    609794,    525832,
          "HHS/CDC",  "TX_NET_NEW", 2019L,    117799,     33837,
          "HHS/CDC",      "TX_NEW", 2019L,     84142,     80955,
              "DOD",     "HTS_TST", 2019L,     67251,     61657,
              "DOD", "HTS_TST_POS", 2019L,      4027,      3842,
              "DOD",     "TX_CURR", 2019L,     32156,     31482,
              "DOD",      "TX_NEW", 2019L,      3697,      3724,
            "USAID",     "HTS_TST", 2020L,   2969619,    635216,
            "USAID", "HTS_TST_POS", 2020L,    148000,     26288,
            "USAID",     "TX_CURR", 2020L,    440023,    279016,
            "USAID",      "TX_NEW", 2020L,    146558,     25659,
          "HHS/CDC",     "HTS_TST", 2020L,   4590197,        NA,
          "HHS/CDC", "HTS_TST_POS", 2020L,    241521,        NA,
          "HHS/CDC",     "TX_CURR", 2020L,    793864,        NA,
          "HHS/CDC",  "TX_NET_NEW", 2020L,    268032,   -525832,
          "HHS/CDC",      "TX_NEW", 2020L,    233480,        NA,
              "DOD",     "HTS_TST", 2020L,     85897,     13283,
              "DOD", "HTS_TST_POS", 2020L,      4721,       911,
              "DOD",     "TX_CURR", 2020L,     34649,     32796,
              "DOD",      "TX_NEW", 2020L,      4557,       856
          ) %>% janitor::clean_names() %>% 
  mutate(pct_achieved = (actual / target)) 




# West Africa Remake ------------------------------------------------------

wa_df <- tibble::tribble(
                 ~Country,   ~Indicator, ~Value,
                "Senegal",      "PLHIV",  42400,
                   "Mali",      "PLHIV", 151900,
           "Burkina Faso",      "PLHIV",  96100,
                   "Togo",      "PLHIV", 110000,
                  "Ghana",      "PLHIV", 330000,
                "Liberia",      "PLHIV",  39500,
           "Sierra Leone",      "PLHIV",  70000,
                "Senegal",  "Incidence",    0.1,
                   "Mali",  "Incidence",    1.3,
           "Burkina Faso",  "Incidence",    0.2,
                   "Togo",  "Incidence",    1.1,
                  "Ghana",  "Incidence",    1.1,
                "Liberia",  "Incidence",    0.6,
           "Sierra Leone",  "Incidence",    0.6,
                "Senegal", "Prevalence",    0.4,
                   "Mali", "Prevalence",    1.4,
           "Burkina Faso", "Prevalence",    0.7,
                   "Togo", "Prevalence",    2.3,
                  "Ghana", "Prevalence",    1.7,
                "Liberia", "Prevalence",    1.3,
           "Sierra Leone", "Prevalence",    1.5
           ) %>% 
  janitor::clean_names()

glimpse(wa_df)

library(maps)
library(sf)
library(rgeos)
library(llamar)
library(rnaturalearth)


# Prep world polygons as a sf object to filter and map --------------------
world <- sf::st_as_sf(map('world2', plot = FALSE, fill = TRUE)) 

world <- ne_countries(scale = 'medium', type = 'map_units',
  returnclass = 'sf')

world %>% 
  count(sovereignt) %>% 
  print(n = Inf)


# List of provided countries for the Indo-Pacific Map
wafr <- c("Togo", "Mali", "Senegal", "Sierra Leone", "Ghana", "Burkina Faso", "Liberia") 

wafr_map <- world %>% 
  filter(sovereignt %in% wafr) %>% 
  left_join(., wa_df, by = c("sovereignt" = "country"))

bbox <- sf::st_bbox(wafr_map)

crs <- "+proj=eqc +lon_0=-5.449218749999998"

# Reusable function to create the maps based on filtered sf object --------

custom_map <- function(df, title = "placeholder") {
  ggplot() +
    geom_sf(data = world, 
      fill = grey20K, colour = "#FFFFFF", size = 0.1) +
    #geom_sf(data = df, fill = grey40K, colour = "#FFFFFF", size = 0.1) +
    geom_sf(data = df, aes(fill = value, geometry = geometry)) +
    geom_sf_text(data = df, aes(label = sovereignt),
      check_overlap = FALSE,
      colour = grey90K) +
    coord_sf(xlim = c(bbox[1], bbox[3]),
      ylim = c(bbox[2], bbox[4])) +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    labs(title = title, x = "", y = "") +
    theme_minimal()+
    theme(axis.text = element_blank())
}


custom_map(wafr_map %>% filter(indicator == "PLHIV")) +
  scale_fill_viridis_c(option = "viridis", begin = 0.45, 
    direction = -1, alpha = 0.75, trans = "log",
    labels = comma)
  

