# Get the Data

# Read in with tidytuesdayR package
# Install from CRAN via:
# install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
tuesdata <- tidytuesdayR::tt_load(2020, week = 38)
rm(tuesdata)
kids <- tuesdata$kids

states <- state.x77 %>%
  as_tibble(rownames = NA) %>%
  rownames_to_column("state") %>%
  mutate(region = state.region)

library(tidyverse)

# DC is not in state.region, put it in "South" because Maryland and Virginia
# are
PK12ed <- kids %>%
  mutate_at(c("state", "variable"), as.factor) %>%
  filter(variable == "PK12ed") %>%
  left_join(select(states, state, region)) %>%
  mutate(region = replace(region, state == "District of Columbia", "South")) %>%
  compact()

ggplot(PK12ed, aes(x = year, y = inf_adj_perchild, color = state)) +
  geom_point() +
  geom_line() +
  geom_text(data = filter(PK12ed, year == last(year)),
                          aes(label = state))
  facet_wrap(.~region)
