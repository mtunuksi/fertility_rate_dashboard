if(!require("pacman")) install.packages(pacman)
pacman::p_load(jsonlite,
               tidyverse,
               janitor,
               here,
               plotly,
               gganimate,
               gifski
               )

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/children-born-per-woman.csv?v=1&csvType=full&useColumnShortNames=true")

metadata <- fromJSON("https://ourworldindata.org/grapher/children-born-per-woman.metadata.json?v=1&csvType=full&useColumnShortNames=true")



df <- df %>% 
  clean_names()


df <- df %>% 
  rename(country_name = entity,
         country_code = code,
          year = year,
         fertility_rate_hist = fertility_rate_hist)

kenya_df <- df %>% 
  filter(country_name == "Kenya")


# ggplot(data = kenya_df, aes(x = year,
#                             y = fertility_rate_hist)) +
#   geom_line()




# Create the enhanced plot
kenya_plot <- ggplot(data = kenya_df, aes(x = year, y = fertility_rate_hist)) +
  # Use a smooth line with a custom color and size
  geom_line(color = "#0072B2", size = 1.2, alpha = 0.8) +
  # Add points at data points for emphasis
  geom_point(color = "#D55E00", size = 2.5, alpha = 0.9) +
  # Apply a modern theme
  theme_minimal(base_size = 14) +
  # Customize theme elements
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#333333"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#666666"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10, color = "#333333"),
    panel.grid.major = element_line(color = "#E0E0E0", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  # Add informative labels
  labs(
    title = "Trends in Fertility Rate in Kenya",
    subtitle = "Historical Data Over Time",
    x = "Year",
    y = "Fertility Rate (Births per Woman)",
    caption = "Source: Your Data Source"
  ) +
  # Add a trend line (optional, using loess smoothing)
  geom_smooth(method = "loess", color = "#009E73", linetype = "dashed", size = 1, se = FALSE) +
  # Ensure proper axis scaling
  scale_x_continuous(breaks = seq(min(kenya_df$year), max(kenya_df$year), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(kenya_df$fertility_rate_hist, na.rm = TRUE), by = 0.5))



ggplotly(kenya_plot)








# Install required packages if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gganimate", quietly = TRUE)) install.packages("gganimate")
if (!requireNamespace("gifski", quietly = TRUE)) install.packages("gifski") # For rendering GIFs

# Load packages
library(ggplot2)
library(gganimate)

# Create the base ggplot with enhancements
p <- ggplot(data = kenya_df, aes(x = year, y = fertility_rate_hist)) +
  # Smooth line with custom color and size
  geom_line(color = "#0072B2", size = 1.2, alpha = 0.8) +
  # Points to highlight data
  geom_point(color = "#D55E00", size = 2.5, alpha = 0.9) +
  # Modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#333333"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#666666"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10, color = "#333333"),
    panel.grid.major = element_line(color = "#E0E0E0", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  # Labels
  labs(
    title = "Trends in Fertility Rate in Kenya: {frame_time}",
    subtitle = "Historical Data Over Time",
    x = "Year",
    y = "Fertility Rate (Births per Woman)",
    caption = "Source: Your Data Source"
  ) +
  # Set axis scales
  scale_x_continuous(breaks = seq(min(kenya_df$year), max(kenya_df$year), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(kenya_df$fertility_rate_hist, na.rm = TRUE), by = 0.5)) +
  # Animation: Reveal line and points over time
  transition_reveal(year)

# Animate the plot
anim <- animate(
  p,
  nframes = 100, # Number of frames (adjust for smoother or faster animation)
  fps = 10,      # Frames per second
  width = 800,   # Output width in pixels
  height = 600,  # Output height in pixels
  renderer = gifski_renderer() # Use gifski for GIF output
)

# Save the animation as a GIF
anim_save("kenya_fertility_timelapse.gif", animation = anim)


