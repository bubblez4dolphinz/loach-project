
library(tidyr)
library(dplyr)
library(ggplot2)

df <- read.csv("RawData/pointplot.csv", na.strings = c("#VALUE!", "na"))
df_clean <- na.omit(df)

# Add row ID
df_clean$row <- seq_len(nrow(df_clean))

# Pivot longer
df_long <- df_clean %>%
  pivot_longer(cols = c(x_pred, x_Ljar, x_Rjar, y_pred, y_Ljar, y_Rjar),
               names_to = c(".value", "point"),
               names_pattern = "(.+)_(.+)") %>%
  arrange(row, point)

# Close each triangle
df_closed <- df_long %>%
  group_by(row) %>%
  summarise(x = c(x, x[1]),
            y = c(y, y[1]),
            real.calc = first(df_clean$real.calc[row[1]]),
            .groups = "drop")

red_points <- df_long %>%
  filter(point == "pred", real.calc == "calc")

triangle_plot <- ggplot() +
  # Triangle outlines with alpha
  geom_polygon(data = df_closed, aes(x = x, y = y, group = row, color = real.calc),
               fill = NA, linewidth = 0.5, alpha = 0.5) +
  
  # Red pred points for 'calc' only, mapped to a new legend group
  geom_point(data = red_points, aes(x = x, y = y, color = "pred_eye"), size = 2) +
  
  # Define all colors in one scale, including the red point
  scale_color_manual(
    name = "Type",
    values = c("real" = "green", "calc" = "yellow", "pred_eye" = "red"),
    labels = c("real" = "Real triangle", 
               "calc" = "Calculated triangle", 
               "pred_eye" = "Predicted predator eye position")
  ) +
  
  coord_fixed() +
  scale_y_reverse() +
  theme_minimal() +
  labs(title = "Extrapolating predator eye position relative to the jars")


triangle_plot
# Save the combined plot
ggsave("Figures/PredatorTriangles.svg", plot = triangle_plot, width = 10, height = 6)
