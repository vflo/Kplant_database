library(ggplot2)
library(ggalluvial)
library(dplyr)

df <- read_delim("database_versions/Kplant_0.1.5.csv",
                 delim = "\t", escape_double = FALSE, 
                 trim_ws = TRUE)

names(df)

# Clean strings and standardize category names
df <- df %>%
  mutate(
    normalization_level = trimws(normalization_level),
    life_form           = case_when(pl_growth_form %in% 
                                      c("tree","shrub","liana")~"Woody",
                                    TRUE ~ "Herbaceous"),
    life_form           = trimws(life_form),
    k_method            = trimws(k_method),
    Flux_method         = trimws(Flux_method)
  ) %>%
  mutate(
    normalization_level = case_when(
      tolower(normalization_level) == "leaf"    ~ "Leaf",
      tolower(normalization_level) == "plant"   ~ "Plant",
      tolower(normalization_level) == "sapwood" ~ "Sapwood",
      TRUE ~ normalization_level
    ),
    life_form = case_when(
      tolower(life_form) == "woody"      ~ "Woody",
      tolower(life_form) == "herbaceous" ~ "Herbaceous",
      TRUE ~ life_form
    )
  ) |> 
  filter(normalization_level %in% c("Leaf","Sapwood", "Plant"),
         Flux_method != "HPFM")

# Count observations for each combination
df_sankey <- df %>%
  count(normalization_level, life_form, k_method, Flux_method, name = "N")

# Order categories by total frequency
Level_keep <- df_sankey %>%
  group_by(normalization_level) %>%
  summarise(totalN = sum(N), .groups = "drop") %>%
  arrange(desc(totalN)) %>%
  pull(normalization_level)

Life_keep <- df_sankey %>%
  group_by(life_form) %>%
  summarise(totalN = sum(N), .groups = "drop") %>%
  arrange(desc(totalN)) %>%
  pull(life_form)

K_keep_order <- df_sankey %>%
  group_by(k_method) %>%
  summarise(totalN = sum(N), .groups = "drop") %>%
  arrange(desc(totalN)) %>%
  pull(k_method)

Flow_keep <- df_sankey %>%
  group_by(Flux_method) %>%
  summarise(totalN = sum(N), .groups = "drop") %>%
  arrange(desc(totalN)) %>%
  pull(Flux_method)

df_sankey <- df_sankey %>%
  mutate(
    normalization_level = factor(normalization_level, levels = Level_keep),
    life_form           = factor(life_form, levels = Life_keep),
    k_method            = factor(k_method, levels = K_keep_order),
    Flux_method         = factor(Flux_method, levels = Flow_keep),
    Level_fill          = as.character(normalization_level)
  )

# Color palette
fill_cols <- c(
  "Leaf"    = "#66C2A5",
  "Plant"   = "#FC8D62",
  "Sapwood" = "#8DA0CB",
  "Other"   = "grey80"
)

# Plot
p <- ggplot(
  df_sankey,
  aes(
    axis1 = normalization_level,
    axis2 = life_form,
    axis3 = k_method,
    axis4 = Flux_method,
    y = N
  )
) +
  scale_x_discrete(
    limits = c("normalization_level", "life_form", "k_method", "Flux_method"),
    labels = c(
      "Normalization level",
      "Life form",
      "k method",
      "Flux method"
    ),
    expand = c(.08, .08)
  ) +
  geom_alluvium(
    aes(fill = Level_fill),
    width = 1/12,
    alpha = 0.80,
    knot.pos = 0.45,
    cement.alluvia = TRUE,
    aes.bind = "alluvia",
    lode.guidance = "forward",
    decreasing = FALSE,
    reverse = FALSE
  ) +
  geom_stratum(
    fill = "grey80",
    width = 1/10,
    color = "black",
    decreasing = FALSE,
    reverse = FALSE
  ) +
  geom_stratum(
    aes(fill = after_stat(ifelse(x == 1, as.character(stratum), "Other"))),
    width = 1/10,
    color = "black",
    decreasing = FALSE,
    reverse = FALSE
  ) +
  geom_text(
    stat = "stratum",
    aes(
      label = after_stat(stratum),
      x = after_stat(x) + ifelse(after_stat(x) == 2, 0.12, 0.10)
    ),
    hjust = 0,
    size = 4,
    check_overlap = TRUE
  ) +
  scale_fill_manual(
    values = fill_cols,
    guide = "none"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length = grid::unit(2, "pt"),
    plot.margin = margin(5.5, 40, 5.5, 10)
  )

ggsave("plots/Kwp_sankey_plot_clean.png", p, width = 12, height = 6, dpi = 300)
