library(ggplot2)

df <- read.csv("RawData/merged_clips.csv", na.strings = c("#VALUE!", "na"))
df_clean <- na.omit(df)

# Optional: extract info from clip name (adjust as needed)
df <- df %>%
  mutate(
    Species = word(clip_name, 1, sep = "_"),
    Replicate = word(clip_name, 4, sep = "_")
  )

# Histogram with vertical line and annotation
delay_hist <- ggplot(df, aes(x = delay)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  annotate("text", x = max(df$delay, na.rm = TRUE)*0.7, y = 10, 
           label = "Positive delay: Dorsal ahead", color = "red", size = 3) +
  annotate("text", x = min(df$delay, na.rm = TRUE)*0.7, y = 10, 
           label = "Negative delay: Lateral ahead", color = "blue", size = 3) +
  labs(title = "Distribution of Delay Between Lateral and Dorsal Footage",
       x = "Delay (frames)",
       y = "Count") +
  theme_minimal()


delay_hist
ggsave("Figures/LD-Delay_hist.svg", plot = delay_hist, width = 10, height = 6)

