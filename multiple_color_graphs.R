# Experiment with multiple colors scales for tables
# Avoid horrendous reds, yellows and greens

df <- tibble::tribble(
    ~disagg,     ~sex, ~VL_Cov, ~group,
    "40-49",   "Male",      9L,     1L,
  "1<",   "Male",     33L,     1L,
  "15 - 19",   "Male",     45L,     1L,
    "20-24",   "Male",     76L,     1L,
    "20-24", "female",     85L,     2L,
    "25-29", "female",     91L,     2L,
    "1 - 5", "female",     94L,     2L,
    "35-39",   "Male",     98L,     2L,
    "1 - 5",   "Male",    102L,     3L,
    "30-34", "female",    103L,     3L,
       "50",   "Male",    104L,     3L,
    "35-39", "female",    112L,     3L,
    "30-34",   "Male",    112L,     3L,
  "15 - 19", "female",    115L,     3L,
    "40-49", "female",    115L,     3L,
       "50", "female",    115L,     3L,
  "10 - 14", "female",    117L,     3L,
    "25-29",   "Male",    125L,     3L,
  "10 - 14",   "Male",    128L,     3L,
  "1<", "female",    267L,     3L
  )

pacman::p_load("tidyverse", "RColorBrewer", "ggnewscale", "scales",
  "llamar", "tidytext")
#devtools::install_github("eliocamp/ggnewscale")

greens <- colorRampPalette(brewer.pal(9,"Greens")[4:9])(6)
reds <- colorRampPalette(rev(brewer.pal(9,"Reds")[3:6]))(6)
yellows <- colorRampPalette(colors = c("#FCBBA1", "#fee08b","#41AB5D"),
  interpolate = c("spline"))(6)

lower_lim = unlist(df %>% filter(group == 1) %>% summarise(max_dev = max((VL_Cov))))
Upper_lim = unlist(df %>% filter(group == 3) %>% summarise(max_dev = min((VL_Cov))))

ggplot(df, aes(x = disagg, y = sex)) +
  geom_tile(data = df %>% filter(group == 3), aes(fill = VL_Cov),
    colour = grey80K) +
  scale_fill_gradientn(colours = greens) +
  new_scale_fill() +
  geom_tile(data = df %>% filter(group == 2), aes(fill = VL_Cov),
    colour = grey80K) +
  scale_fill_gradientn(colours = yellows,
    limits = c(lower_lim, Upper_lim)) +
  new_scale_fill() +
  geom_tile(data = df %>% filter(group == 1), aes(fill = VL_Cov),
    colour = grey80K) +
  scale_fill_gradientn(colours = reds) +
  geom_text(aes(label = VL_Cov)) +
  llamar::theme_xygrid() +
  coord_fixed() +
  theme(legend.position = "none") +
  labs(x = "", y = "")


upper_lim = unlist(df %>% summarise(max_dev = max((VL_Cov))))
lower_lim = unlist(df %>% summarise(min_dev = min(VL_Cov)))

# Versus using spectral
ggplot(df, aes(x = disagg, y = sex)) +
  geom_tile(data = df, aes(fill = VL_Cov)) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'Spectral'),
    limits = c(lower_lim, upper_lim))
