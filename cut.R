
# load packages
library(tidyverse)


# load data
df <- read_rds("data/parties.rds") %>%
  glimpse()

weight_width <- 1
eneg0 <- 10
df$weights <- dnorm(df$eneg, mean = eneg0, sd = weight_width)
df$weights <- df$weights/sum(df$weights)
sum(df$weights)
ggplot(df, aes(x = enep, color = electoral_system)) + 
  geom_density(aes(weight = weights))
