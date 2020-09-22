library(tidyverse)
library(corrplot)
library(viridis)

# Load data and separate into semantically named dfs
friends <- tidytuesdayR::tt_load('2020-09-08')
friends_info <- friends[[1]]
friends_dirs <- friends[[2]]
friends_emot <- friends[[3]]

# Join emotion info with who said it
lines_emot <- right_join(friends_info, friends_emot)

# Number of lines by emotion
ggplot(lines_emot, aes(x = emotion)) +
  geom_histogram(stat = "count")

# Identify which characters had more than 50 lines (so we don't get weird
# proportions)
lines_per_character <- table(lines_emot$speaker)
gt_50_lines <- names(lines_per_character[lines_per_character > 50])

# Filter down to those people
char_emot <- lines_emot %>%
  filter(speaker %in% gt_50_lines)

# Now tabulate, and remove "#ALL#"
ce_table <- table(char_emot$speaker, char_emot$emotion) %>%
  as.data.frame(col.names = c("speaker", "emotion", "freq")) %>%
  filter(Var1 != "#ALL#") %>%
  pivot_wider(id_cols = "Var1", names_from = "Var2", values_from = "Freq",
              names_prefix = "emotion_") %>%
  rename(speaker = Var1)

# Calculate total lines, removing Neutral because it skews
total_lines <- ce_table %>%
  select(starts_with("emotion_"), -emotion_Neutral) %>%
  rowSums()

# Lines by percent of speakers' total lines with that emotion
ce_table_pct <- ce_table %>%
  mutate_at(vars(starts_with("emotion_")), function(x) x / total_lines) %>%
  select(-emotion_Neutral)

# Scaled so we can see who stands out
ce_table_pct_z <- ce_table_pct %>%
  mutate_at(vars(starts_with("emotion_")), scale)

# Pivot longer for plotting
ce_pct_long <- ce_table_pct %>%
  pivot_longer(-speaker)

ggplot(ce_pct_long, aes(x = name, y = speaker, fill = value)) +
  geom_tile() +
  scale_fill_viridis() +
  scale_x_discrete(labels = unique(ce_pct_long$name) %>%
                              gsub("emotion_", "", .)) +
  labs(x = "Emotion", y = "Character", fill = "Proportion",
       title = "Characters' lines by emotion")

ce_z_long <- ce_table_pct_z %>%
  pivot_longer(-speaker)

ggplot(ce_z_long, aes(x = name, y = speaker, fill = value)) +
  geom_tile() +
  scale_x_discrete(labels = unique(ce_pct_long$name) %>%
                     gsub("emotion_", "", .)) +
  scale_fill_viridis(option = "magma") +
  labs(x = "Emotion", y = "Character", fill = "Z score",
       title = "Characters' lines by unique emotions")

