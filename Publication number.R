library(readr)
library(dplyr)
library(ggplot2)
library(grid)

# Read data
file_path <- "data_github/kplant_papers_included_publication_year.csv"
dat <- read_csv("data_github/kplant_papers_included_publication_year.csv")

# df <- read_delim("database_versions/Kplant_0.1.5.csv",
#                  delim = "\t", escape_double = FALSE, 
#                  trim_ws = TRUE)
# df_id <- unique(df$IDref)
dat <- dat |> 
  # filter(IDref %in% df_id) |> 
  group_by(Year) |> 
  summarise(Number_of_publications = n())


# Line widths
border_w   <- 0.8
tick_w_x   <- 0.5
tick_w_y   <- 0.5
bar_border <- 0.3

# Plot
p <- ggplot(dat, aes(x = Year, y = Number_of_publications)) +
  geom_col(
    width = 0.9,
    fill = "#d9d9d9",
    color = "#bdbdbd",
    linewidth = bar_border
  ) +
  scale_x_continuous(
    limits = c(1967.5, 2027.5),
    breaks = seq(1975, 2025, by = 10),
    minor_breaks = seq(1967.5, 2027.5, by = 5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    minor_breaks = seq(0, 25, by = 2.5),
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Number of publications"
  ) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks.x = element_line(
      color = "black",
      linewidth = tick_w_x,
      lineend = "square"
    ),
    axis.ticks.y = element_line(
      color = "black",
      linewidth = tick_w_y,
      lineend = "square"
    ),
    axis.ticks.length = unit(0.18, "cm"),
    axis.text.x = element_text(
      size = 12,
      family = "serif",
      color = "black"
    ),
    axis.text.y = element_text(
      size = 12,
      family = "serif",
      color = "black"
    ),
    axis.title.x = element_text(
      size = 16,
      family = "serif",
      color = "black"
    ),
    axis.title.y = element_text(
      size = 16,
      family = "serif",
      color = "black"
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = border_w
    ),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)

# Save figure
ggsave(
  "plots/Paper_number_plot.png",
  plot = p,
  width = 6,
  height = 5,
  dpi = 600
)