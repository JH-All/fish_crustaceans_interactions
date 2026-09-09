# Packages ------------------------
load_packages <- function(packages){
  
  for(pkg in packages){
    
    if(!requireNamespace(pkg, quietly = TRUE)){
      install.packages(pkg)
    }
    
    library(pkg, character.only = TRUE)
  }
}

packages <- c(
  "readxl",
  "tidyverse",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf",
  "scales",
  "ggplot2",
  "ggalluvial",
  "networkD3",
  "patchwork",
  "readr",
  "MASS",
  "broom",
  "htmltools",
  "htmlwidgets",
  "webshot2",
  "magick"
)

load_packages(packages)

# Data -------------------------------
data = read_excel("data.xlsx")
str(data)

# Figure 2 -----------------------------------------
data$interaction = as.factor(data$interaction)
levels(data$interaction)

## Figure 2A ---------------------
my_cols <- c(
  "Commensalism" = "grey70",
  "Competition" = "#E6AB02",
  "Parasitism" = "#E67E22",
  "Predation"   = "#B23A2F"
)

fig2_A <- ggplot(data,
                 aes(y = fct_rev(fct_infreq(interaction)),
                     fill = interaction)) +
  geom_bar(
    color = "black",
    alpha = 0.7
  ) +
  scale_fill_manual(values = my_cols) +
  labs(
    y = NULL,
    x = "Number of studies"
  ) +
  theme_classic(base_size = 16) +
  scale_x_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, by = 10),
    expand = c(0, 0)
  ) +
  theme(
    legend.position = "none"
  )

fig2_A

## Figure 2B --------------------------------
accum <- data %>%
  filter(interaction != "Commensalism") %>%
  count(year, interaction) %>%
  arrange(interaction, year) %>%
  group_by(interaction) %>%
  mutate(cumulative_studies = cumsum(n)) %>%
  ungroup()

my_cols <- c(
  "Competition" = "#E6AB02",
  "Parasitism" = "#E67E22",
  "Predation"   = "#B23A2F"
)

fig2_B = ggplot(accum,
                aes(x = year,
                    y = cumulative_studies,
                    color = interaction,
                    group = interaction)) +
  geom_line(linewidth = 1.1, lineend = "round") +
  scale_color_manual(values = my_cols) +
  labs(
    x = "Year",
    y = "Cumulative number of studies",
    color = NULL
  ) +
  theme_classic(base_size = 16) +
  scale_x_continuous(limits = c(1960, 2025),
                     breaks = seq(1960, 2025 , by = 5))+
  scale_y_continuous(limits = c(0, 70),
                     breaks = seq(0, 70, by = 10))+
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(linewidth = 0.6),
    plot.margin = margin(10, 15, 10, 10)
  )+
  guides(
    color = guide_legend(reverse = TRUE)
  )

fig2_B

## Figure 2 Complete ------------------------
fig2_c = (fig2_A + fig2_B) +
  plot_annotation(tag_levels = "A")

fig2_c

ggsave("Figure_2.jpg", fig2_c, width = 12, height = 6, dpi = 300)

# Environments ------------------
data$environment = as.factor(data$environment )
levels(data$environment )

# Figure 3 -----------------------------------
dados_clean <- data %>%
  filter(
    !is.na(country),
    !is.na(interaction),
    interaction != "Commensalism"
  ) %>%
  mutate(
    interaction = as.character(interaction),
    country = str_split(country, ";")
  ) %>%
  unnest(country) %>%
  mutate(country = str_trim(country))

country_counts <- dados_clean %>%
  count(interaction, country, name = "n_studies") %>%
  mutate(country = dplyr::recode(
    country,
    "USA" = "United States of America",
    "UK" = "United Kingdom",
    "Czech Republic" = "Czechia",
    "Noruega" = "Norway",
    "Suécia" = "Sweden"
  ))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")

countries_no_match <- country_counts %>%
  distinct(country) %>%
  anti_join(
    world %>% 
      st_drop_geometry() %>% 
      distinct(name),
    by = c("country" = "name")
  )

countries_no_match

interactions <- c("Competition", "Predation", "Parasitism")

world_by_interaction <- tidyr::crossing(
  interaction = interactions,
  world
) %>%
  st_as_sf()

map_data <- world_by_interaction %>%
  left_join(
    country_counts,
    by = c("interaction", "name" = "country")
  ) %>%
  mutate(
    interaction = factor(
      interaction,
      levels = c("Competition", "Predation", "Parasitism")
    )
  )

fig3 <- ggplot(map_data) +
  geom_sf(aes(fill = n_studies), color = "gray40", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#FDBE85",
    high = "darkred",
    na.value = "gray90",
    limits = c(0, max(map_data$n_studies, na.rm = TRUE))
  ) +
  facet_wrap(~ interaction, ncol = 1) +
  coord_sf(
    crs = "+proj=robin",
    expand = FALSE
  ) +
  theme_void(base_size = 18) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.margin = margin(l = 10),
    legend.box.margin = margin(l = 10),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.6, "cm"),
    panel.spacing = unit(0.6, "cm"),
    strip.text = element_text(size = 16),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(fill = "Number\nof studies")

fig3

ggsave("Figure_3.jpg", fig3, dpi = 300)


# Figure S1 --------------------------------
land_area <- read_csv("land_area_km.csv")

land_area_2023 <- land_area %>%
  filter(Year == 2023) %>%
  transmute(
    country = Entity,
    land_area_km2 = `Land area (sq. km)`
  )

dados_clean <- data %>%
  filter(
    !is.na(country),
    !is.na(interaction),
    interaction != "Commensalism"
  ) %>%
  mutate(
    interaction = as.character(interaction),
    country = str_split(country, ";")
  ) %>%
  unnest(country) %>%
  mutate(
    country = str_trim(country),
    country = dplyr::recode(
      country,
      "USA" = "United States",
      "UK" = "United Kingdom",
      "Czech Republic" = "Czechia",
      "Noruega" = "Norway",
      "Suécia" = "Sweden"
    )
  )


country_area_summary <- dados_clean %>%
  count(country, interaction, name = "n_studies") %>%
  left_join(land_area_2023, by = "country") %>%
  mutate(
    studies_per_million_km2 = n_studies / (land_area_km2 / 1e6)
  )

country_area_summary %>%
  arrange(interaction, desc(studies_per_million_km2))


country_area_wide <- country_area_summary %>%
  dplyr::select(country, interaction, studies_per_million_km2) %>%
  pivot_wider(
    names_from = interaction,
    values_from = studies_per_million_km2,
    values_fill = 0
  )

country_area_wide

country_area_total <- dados_clean %>%
  count(country, name = "total_studies") %>%
  left_join(land_area_2023, by = "country") %>%
  mutate(
    total_studies_per_million_km2 =
      total_studies / (land_area_km2 / 1e6)
  ) %>%
  arrange(desc(total_studies_per_million_km2))

country_area_total

map_data_area <- world_by_interaction %>%
  left_join(
    country_area_summary,
    by = c("interaction", "name" = "country")
  ) %>%
  mutate(
    interaction = factor(
      interaction,
      levels = c("Competition", "Predation", "Parasitism")
    )
  )

fig_s1 <- ggplot(map_data_area) +
  geom_sf(aes(fill = studies_per_million_km2),
          color = "gray40",
          linewidth = 0.2) +
  scale_fill_gradient(
    low = "#FDBE85",
    high = "darkred",
    na.value = "gray90",
    limits = c(
      0,
      max(map_data_area$studies_per_million_km2, na.rm = TRUE)
    )
  ) +
  facet_wrap(~ interaction, ncol = 1) +
  coord_sf(
    crs = "+proj=robin",
    expand = FALSE
  ) +
  theme_void(base_size = 18) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.margin = margin(l = 10),
    legend.box.margin = margin(l = 10),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.6, "cm"),
    panel.spacing = unit(0.6, "cm"),
    strip.text = element_text(size = 16),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(fill = "Studies per\nmillion km²")

fig_s1

ggsave("Figure_S1.jpg", fig_s1, dpi = 300)

head(data$fish)
head(data$crustacean)
head(data$if_predation_who)
head(data$interaction)

# Figure 4 & 5 ----------------------
my_colour <- '
d3.scaleOrdinal()
  .domain(["Fish", "Crustacean"])
  .range(["#6BAED6", "#FDBE85"])
'

prepare_sankey <- function(df, source_col, target_col,
                           source_group = "Fish",
                           target_group = "Crustacean") {
  
  links <- df %>%
    count({{ source_col }}, {{ target_col }}, name = "value") %>%
    rename(
      source_name = {{ source_col }},
      target_name = {{ target_col }}
    ) %>%
    mutate(
      source_name = ifelse(source_name == "Assemblage", 
                           paste(source_group, "assemblage"), 
                           source_name),
      target_name = ifelse(target_name == "Assemblage", 
                           paste(target_group, "assemblage"), 
                           target_name)
    )
  
  source_nodes <- links %>%
    distinct(name = source_name) %>%
    mutate(group = source_group)
  
  target_nodes <- links %>%
    distinct(name = target_name) %>%
    mutate(group = target_group)
  
  nodes <- bind_rows(source_nodes, target_nodes) %>%
    distinct(name, .keep_all = TRUE)
  
  links_d3 <- links %>%
    mutate(
      source = match(source_name, nodes$name) - 1,
      target = match(target_name, nodes$name) - 1
    ) %>%
    dplyr::select(source, target, value)
  
  list(nodes = nodes, links = links_d3)
}

save_sankey <- function(sankey_data, title, html_file, png_file) {
  
  widget <- browsable(
    tagList(
      tags$h2(
        title,
        style = "
          text-align: left;
          margin-left: 25px;
          margin-bottom: 5px;
          font-weight: bold;
          font-family: Arial;
        "
      ),
      sankeyNetwork(
        Links = as.data.frame(sankey_data$links),
        Nodes = as.data.frame(sankey_data$nodes),
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "name",
        NodeGroup = "group",
        colourScale = my_colour,
        fontSize = 16,
        nodeWidth = 30,
        sinksRight = TRUE,
        width = "100%",
        height = 700
      )
    )
  )
  
  htmltools::save_html(widget, file = html_file)
  
  webshot2::webshot(
    url = html_file,
    file = png_file,
    vwidth = 1400,
    vheight = 900,
    zoom = 3
  )
}

## Figure 4A — Competition -----------------------------

fig4A_data <- data %>%
  filter(interaction == "Competition") %>%
  filter(!is.na(fish), !is.na(crustacean)) %>%
  prepare_sankey(fish, crustacean)

save_sankey(
  fig4A_data,
  "A) Competition",
  "figure4A_competition.html",
  "figure4A_competition.png"
)

## Figure 4B — Parasitism ------------------------------

fig4B_data <- data %>%
  filter(interaction == "Parasitism") %>%
  filter(!is.na(fish), !is.na(crustacean)) %>%
  prepare_sankey(fish, crustacean)

save_sankey(
  fig4B_data,
  "B) Parasitism",
  "figure4B_parasitism.html",
  "figure4B_parasitism.png"
)

## Figure 5A — Predation, fish as predator -------------

fig5A_data <- data %>%
  filter(interaction == "Predation") %>%
  filter(!is.na(fish), !is.na(crustacean), !is.na(if_predation_who)) %>%
  filter(if_predation_who == "F") %>%
  prepare_sankey(fish, crustacean)

save_sankey(
  fig5A_data,
  "A) Predation — fishes as predators",
  "figure5A_predation_fish.html",
  "figure5A_predation_fish.png"
)

## Figure 5B — Predation, crustacean as predator --------

fig5B_data <- data %>%
  filter(interaction == "Predation") %>%
  filter(!is.na(fish), !is.na(crustacean), !is.na(if_predation_who)) %>%
  filter(if_predation_who == "C") %>%
  prepare_sankey(
    crustacean, fish,
    source_group = "Crustacean",
    target_group = "Fish"
  )

save_sankey(
  fig5B_data,
  "B) Predation — macrocrustaceans as predators",
  "figure5B_predation_crustacean.html",
  "figure5B_predation_crustacean.png"
)

## Figure 4 Complete ----------------------

figure4_final <- image_append(
  c(
    image_read("figure4A_competition.png"),
    image_read("figure4B_parasitism.png")
  ),
  stack = TRUE
)

image_write(
  figure4_final,
  "Figure_4_AB.png"
)

## Figure 5 Complete  ----------

figure5_final <- image_append(
  c(
    image_read("figure5A_predation_fish.png"),
    image_read("figure5B_predation_crustacean.png")
  ),
  stack = TRUE
)

image_write(
  figure5_final,
  "Figure_5_AB.png"
)

# Figure 6 -------------------------------------------
data$status_fish = as.factor(data$status_fish)
levels(data$status_fish)
data$status_crust = as.factor(data$status_crust)
levels(data$status_crust)


status_summary <- data %>%
  filter(!is.na(status_fish), !is.na(status_crust)) %>%
  mutate(
    status_group = case_when(
      status_fish == "Exotic" & status_crust == "Exotic" ~ "Both non-native",
      status_fish == "Exotic" & status_crust == "Native" ~ "Only fish non-native",
      status_fish == "Native" & status_crust == "Exotic" ~ "Only crustacean non-native",
      status_fish == "Native" & status_crust == "Native" ~ "Both native"
    )
  ) %>%
  count(status_group, interaction, name = "n_studies")

my_cols <- c(
  "Commensalism" = "grey70",
  "Competition"  = "#E6AB02",
  "Parasitism"   = "#E67E22",
  "Predation"    = "#B23A2F"
)

status_summary$interaction <- factor(
  status_summary$interaction,
  levels = names(my_cols)
)

fig6 = ggplot(
  status_summary,
  aes(
    x = n_studies,
    y = fct_reorder(status_group, n_studies, sum),
    fill = interaction
  )
) +
  geom_col(color = "black", alpha = 0.8) +
  scale_fill_manual(
    values = my_cols,
    limits = names(my_cols),
    breaks = names(my_cols),
    drop = FALSE
  ) +
  guides(
    fill = guide_legend(reverse = TRUE)
  ) +
  labs(
    x = "Number of studies",
    y = NULL,
    fill = "Interaction type"
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = c(0.99, 0.02),
    legend.justification = c(1, 0)
  )+
  scale_x_continuous(expand = c(0,0))

fig6

ggsave("Figure_6.jpg", fig6, dpi = 300)

# Binomial test ----------------------
data_glm <- data %>%
  filter(!is.na(status_fish), !is.na(status_crust)) %>%
  mutate(
    has_exotic = ifelse(
      status_fish == "Exotic" | status_crust == "Exotic",
      1, 0
    )
  )

binom.test(
  sum(data_glm$has_exotic),
  nrow(data_glm),
  p = 0.5
)
