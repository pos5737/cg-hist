
# load packages
library(tidyverse)
library(hrbrthemes)
# load data
df <- read_rds("data/parties.rds") %>%
  glimpse()

# create dataframe without faceting variables (for grey background)
tmp_df <- df %>%
  select(-social_heterogeneity, -electoral_system) %>%
  glimpse()

# create data frame of plot annotations
ann_df <- data.frame(x = 7, y = 0.15, xend = 5, yend = 0.1, 
                     electoral_system = "Single-Member District",
                     social_heterogeneity = "Bottom 3rd of ENEG", 
                     label = "density of entire data set")

# create data frame of theoretical expectations
exp_df <- crossing(social_heterogeneity = unique(df$social_heterogeneity), 
                   electoral_system = unique(df$electoral_system)) %>%
  mutate(hypothesis = ifelse(test = electoral_system == "Single-Member District" | 
                               social_heterogeneity == "Bottom 3rd of ENEG", 
                             yes = "A Small Number of Parties", 
                             no = ifelse(test = electoral_system == "Large-Magnitude PR" & 
                                           social_heterogeneity == "Top 3rd of ENEG",
                                         yes = "A Large Number of Parties", 
                                         no = "A Moderate Number of Parties")),
         x = Inf, 
         y = Inf) %>% 
  mutate(hypothesis = factor(hypothesis, levels = c("A Small Number of Parties", 
                                                    "A Moderate Number of Parties", 
                                                    "A Large Number of Parties"))) %>%
  glimpse()

# merge theoretical expectations with data
df2 <- left_join(df, exp_df)


# create histogram
ggplot(df2, aes(x = enep, y = ..density.., color = hypothesis)) + 
  geom_density(data = tmp_df, fill = "grey70", color = NA) + 
  geom_histogram(aes(fill = hypothesis), color = NA, alpha = 0.6, bins = 15) +
  stat_density(geom = "line", size = 1, alpha = 0.6) +
  facet_grid(rows = vars(social_heterogeneity), cols = vars(electoral_system)) + 
  geom_text(data = ann_df, aes(x = x, y = y, label = label), size = 2.5, hjust = -0.05, vjust = 0, color = "grey50") + 
  geom_segment(data = ann_df, aes(x = x, y = y, xend = xend, yend = yend), size = 0.5, color = "grey70") + 
  scale_color_manual(values = c("#fdd49e", "#fc8d59", "#b30000")) + 
  scale_fill_manual(values = c("#fdd49e", "#fc8d59", "#b30000")) + 
  theme_minimal() + 
  labs(x = "Effective Number of Electoral Parties",
       y = "Density",
       color = "Clark and Golder's Expectation", 
       fill = "Clark and Golder's Expectation")
ggsave("histogram.png", height = 7, width = 10)
