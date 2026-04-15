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
  "webshot2"
)

load_packages(packages)

# Data -------------------------------
data = read_excel("patterns.xlsx")
str(data)
data$journal = as.factor(data$journal)
data$environment = as.factor(data$environment)
data$interaction = as.factor(data$interaction)
levels(data$environment)
levels(data$interaction)

data %>% count(environment)


dados = read_excel("freshwater.xlsx")
dados$country = as.factor(dados$country)
levels(dados$country)
dados$environment = as.factor(dados$environment)
levels(dados$environment)
dados$interaction = as.factor(dados$interaction)
levels(dados$interaction)

# Figure 2A ----------------------------------
bar_env <- data %>%
  filter(!is.na(interaction), !is.na(environment)) %>%
  count(interaction, environment, name = "n") %>%
  group_by(interaction) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

bar_env$interaction <- factor(
  bar_env$interaction,
  levels = c("Competition", "Predation", "Parasitism",
             "Commensalism", "Mutualism")
)

fig2_A = ggplot(bar_env, aes(x = interaction, y = prop, fill = environment)) +
  geom_col(width = 0.9, color = "black", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0,0)) +
  scale_fill_manual(values = c(
    "Freshwater" = "#FDBE85", 
    "Marine" = "#9ECAE1"
  )) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.key.size = unit(1.2, "cm"),
    legend.spacing.x = unit(0.5, "cm")
  ) +
  labs(
    x = NULL,
    y = "Percentage of studies",
    fill = "Environment"
  )

fig2_A

# Figure 2B-E ---------------------------------
cum_studies <- data %>%
  filter(!is.na(year), !is.na(interaction), !is.na(environment)) %>%
  filter(interaction != "Commensalism") %>%
  count(year, interaction, environment, name = "n_studies") %>%
  complete(
    year = full_seq(seq(min(data$year, na.rm = TRUE),
                        max(data$year, na.rm = TRUE), 1), 1),
    interaction,
    environment,
    fill = list(n_studies = 0)
  ) %>%
  arrange(interaction, environment, year) %>%
  group_by(interaction, environment) %>%
  mutate(cum_studies = cumsum(n_studies)) %>%
  ungroup()


cum_studies$interaction <- factor(
  cum_studies$interaction,
  levels = c("Competition", "Predation", "Parasitism", "Mutualism")
)

cum_studies$environment <- factor(
  cum_studies$environment,
  levels = c("Freshwater", "Marine")
)

## Figure 2B ------------------------------------------
fig2_B = ggplot(filter(cum_studies, interaction == "Competition"),
                aes(x = year, y = cum_studies, color = environment)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c(
    "Freshwater" = "#FDBE85",
    "Marine" = "#9ECAE1"
  )) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5) ,
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Competition",
    x = "Year",
    y = "Cumulative number \nof studies"
  ) +
  scale_x_continuous(limits = c(1945, 2025),
                     breaks = seq(1945, 2025, by = 10)) +
  scale_y_continuous(limits = c(0,32),
                     breaks = seq(0,30, by = 5))

fig2_B

## Figure 2C ------------------------------------------
fig2_C = ggplot(filter(cum_studies, interaction == "Predation"),
       aes(x = year, y = cum_studies, color = environment)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c(
    "Freshwater" = "#FDBE85",
    "Marine" = "#9ECAE1"
  )) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5) ,
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Predation",
    x = "Year",
    y = "Cumulative number \nof studies"
  ) +
  scale_x_continuous(limits = c(1945, 2025),
                     breaks = seq(1945, 2025, by = 10))+
  scale_y_continuous(limits = c(0,182),
                     breaks = seq(0, 180, by = 30))

fig2_C 

## Figure 2D ------------------------------------------
fig2_D = ggplot(filter(cum_studies, interaction == "Parasitism"),
       aes(x = year, y = cum_studies, color = environment)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c(
    "Freshwater" = "#FDBE85",
    "Marine" = "#9ECAE1"
  )) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5) ,
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Parasitism",
    x = "Year",
    y = "Cumulative number \nof studies"
  ) +
  scale_x_continuous(limits = c(1945, 2025),
                     breaks = seq(1945, 2025, by = 10))+
  scale_y_continuous(limits = c(0,135),
                     breaks = seq(0, 135, by = 30))

fig2_D

## Figure 2E ------------------------------------------
fig2_E = ggplot(filter(cum_studies, interaction == "Mutualism",
              environment == "Marine"),
       aes(x = year, y = cum_studies, color = environment)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c(
    "Marine" = "#9ECAE1"
  )) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5) ,
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Mutualism",
    x = "Year",
    y = "Cumulative number \nof studies"
  )   +
  scale_x_continuous(limits = c(1945, 2025),
                         breaks = seq(1945, 2025, by = 10))+
  scale_y_continuous(limits = c(0,46),
                     breaks = seq(0, 45, by = 10))

fig2_E

# Figure 2 Complete -------------------------------
fig2_final <- (fig2_A | ((fig2_B + fig2_C) / (fig2_D + fig2_E))) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(tag_levels = "A")

fig2_final

ggsave("Figure_2.jpg", fig2_final, width = 15, height = 8)

# Figure 3 --------------------------
dados_clean <- dados %>%
  filter(!is.na(country), !is.na(interaction)) %>%
  mutate(country = str_split(country, ";")) %>%
  unnest(country) %>%
  mutate(country = str_trim(country))

country_counts <- dados_clean %>%
  count(interaction, country, name = "n_studies") %>%
  mutate(country = dplyr::recode(
    country,
    "USA" = "United States of America",
    "United Kingdom" = "United Kingdom",
    "Czech Republic" = "Czechia",
    "Noruega" = "Norway",
    "Suécia" = "Sweden"
  ))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")

interactions <- sort(unique(country_counts$interaction))

world_by_interaction <- tidyr::crossing(
  interaction = interactions,
  world
) %>%
  st_as_sf()

map_data <- world_by_interaction %>%
  left_join(country_counts, by = c("interaction", "name" = "country"))

map_data$interaction <- factor(
  map_data$interaction,
  levels = c("Competition", "Predation", "Parasitism")
)


fig3 = ggplot(map_data) +
  geom_sf(aes(fill = n_studies), color = "gray40", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#FDBE85",  
    high = "darkred", 
    na.value = "gray90",
    limits = c(0, max(map_data$n_studies, na.rm = TRUE))
  )+
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
  labs(fill = "Studies \nnumber")

fig3

ggsave("Figure_3.jpg", fig3)

# Figure 4  ------------------------------------------
## Data ---------------------------------------------
population_size <- read_csv("population_size.csv")
summary(population_size$Year)
str(population_size$Population)

land_area <- read_csv("land_area_km.csv")
summary(land_area$Year)
str(land_area$`Land area (sq. km)`)

country_summary <- dados_clean %>%
  mutate(country = dplyr::recode(
    country,
    "USA" = "United States",
    "Czech Republic" = "Czechia",
    "Suécia" = "Sweden",
    "Noruega" = "Norway",
    "Taiwan" = "Taiwan"
  )) %>%
  count(country, interaction, name = "n_studies") %>%
  pivot_wider(
    names_from = interaction,
    values_from = n_studies,
    values_fill = 0
  ) %>%
  mutate(
    total_studies = Competition + Predation + Parasitism
  ) %>%
  dplyr::select(country, total_studies, Competition, Predation, Parasitism) %>%
  arrange(desc(total_studies))

population_2023 <- population_size %>%
  filter(Year == 2023) %>%
  dplyr::select(
    country = Entity,
    population_2023 = Population
  )


land_area_2023 <- land_area %>%
  filter(Year == 2023) %>%
  dplyr::select(
    country = Entity,
    land_area_km2_2023 = `Land area (sq. km)`
  )



population_2023 <- population_2023 %>%
  filter(country %in% country_summary$country)

land_area_2023 <- land_area_2023 %>%
  filter(country %in% country_summary$country)


country_summary_final <- country_summary %>%
  left_join(population_2023, by = "country") %>%
  left_join(land_area_2023, by = "country") 

colSums(is.na(country_summary_final))

country_summary_final %>%
  filter(is.na(population_2023)) %>%
  dplyr::select(country)

country_summary_final %>%
  filter(is.na(land_area_km2_2023)) %>%
  dplyr::select(country)

country_summary_final <- country_summary_final %>%
  mutate(
    log_pop = log10(population_2023),
    log_area = log10(land_area_km2_2023),
    
    log_pop_scaled = as.numeric(scale(log_pop)),
    log_area_scaled = as.numeric(scale(log_area))
  )


## Models ---------------------------------
m_total_pop <- glm.nb(total_studies ~ log_pop_scaled, data = country_summary_final)
summary(m_total_pop)
m_comp_pop  <- glm.nb(Competition ~ log_pop_scaled, data = country_summary_final)
summary(m_comp_pop)
m_pred_pop  <- glm.nb(Predation ~ log_pop_scaled, data = country_summary_final)
summary(m_pred_pop)
m_para_pop  <- glm.nb(Parasitism ~ log_pop_scaled, data = country_summary_final)
summary(m_para_pop)


m_total_area <- glm.nb(total_studies ~ log_area_scaled, data = country_summary_final)
summary(m_total_area)
m_comp_area  <- glm.nb(Competition ~ log_area_scaled, data = country_summary_final)
summary(m_comp_area)
m_pred_area  <- glm.nb(Predation ~ log_area_scaled, data = country_summary_final)
summary(m_pred_area)
m_para_area  <- glm.nb(Parasitism ~ log_area_scaled, data = country_summary_final)
summary(m_para_area)


extract_coef <- function(model, response, predictor){
  tidy(model) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      response = response,
      predictor = predictor,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )
}


coef_df <- bind_rows(
  extract_coef(m_total_pop, "Total", "Population"),
  extract_coef(m_comp_pop,  "Competition", "Population"),
  extract_coef(m_pred_pop,  "Predation", "Population"),
  extract_coef(m_para_pop,  "Parasitism", "Population"),
  
  extract_coef(m_total_area, "Total", "Area"),
  extract_coef(m_comp_area,  "Competition", "Area"),
  extract_coef(m_pred_area,  "Predation", "Area"),
  extract_coef(m_para_area,  "Parasitism", "Area")
)

coef_df$response <- factor(
  coef_df$response,
  levels = c("Parasitism",
             "Predation", "Competition", "Total")
  
)

coef_df$predictor <- factor(
  coef_df$predictor,
  levels = c("Population", "Area")
)

## FigA --------------------------------------
top5_raw <- country_summary_final %>%
  arrange(desc(total_studies)) %>%
  slice(1:5)


fig_new_A = ggplot(top5_raw,
                   aes(x = reorder(country, total_studies),
                       y = total_studies)) +
  geom_col(fill = "#FDBE85", color = "black",
           alpha = 0.7) +
  coord_flip() +
  theme_classic(base_size = 16) +
  labs(
    x = NULL,
    y = "Total number of studies"
  )+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 80))

fig_new_A

## FigB --------------------------------
country_summary_final <- country_summary_final %>%
  mutate(
    studies_per_person = total_studies / population_2023,
    studies_per_million_people = (total_studies / population_2023) * 1e6,
    studies_per_km2 = total_studies / land_area_km2_2023
  )

top5_pop <- country_summary_final %>%
  filter(!is.na(studies_per_million_people)) %>%
  arrange(desc(studies_per_million_people)) %>%
  slice(1:5)


fig_new_B = ggplot(top5_pop,
                   aes(x = reorder(country, studies_per_million_people),
                       y = studies_per_million_people)) +
  geom_col(fill = "#FDBE85", color = "black",
           alpha = 0.7) +
  coord_flip() +
  theme_classic(base_size = 16) +
  labs(
    x = NULL,
    y = "Studies per million people"
  )+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 4))

fig_new_B

## FigC --------------------------------
top5_area <- country_summary_final %>%
  filter(!is.na(studies_per_km2)) %>%
  arrange(desc(studies_per_km2)) %>%
  slice(1:5)


fig_new_C = ggplot(top5_area,
                   aes(x = reorder(country, studies_per_km2),
                       y = studies_per_km2)) +
  geom_col(fill = "#FDBE85", color = "black",
           alpha = 0.7) +
  coord_flip() +
  theme_classic(base_size = 16) +
  labs(
    x = NULL,
    y = "Studies per km²"
  )+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.001))

fig_new_C

## FigD ------------------------------
fig_new_D = ggplot(coef_df, aes(x = response, y = estimate, color = predictor)) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width = 0.55,
                position = position_dodge(width = 0.55) ,
                size = 1.5) +
  geom_point(size = 5.5, position = position_dodge(width = 0.55)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_color_manual(
    values = c(
      "Population" = "#FDBE85",
      "Area" = "darkred"
    ),
    breaks = c("Area", "Population")
  )+
  theme_classic(base_size = 16) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1)
  ) +
  labs(
    x = NULL,
    y = "Coefficient (95% CI)",
    color = "Predictor"
  ) +
  coord_flip()

fig_new_D

## Complete -----------------------------------
fig_4 = (fig_new_A + fig_new_B) / (fig_new_C + fig_new_D)+
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(tag_levels = "A")

fig_4

ggsave("Figure_4.jpg", fig_4, width = 13, height = 8)

# Figure 5 -------------------------------
links_comp <- dados %>%
  filter(interaction == "Competition") %>%
  filter(!is.na(fish), !is.na(crustacean)) %>%
  count(fish, crustacean, name = "value") %>%
  mutate(
    fish = ifelse(fish == "Assemblage", "Fish assemblage", fish),
    crustacean = ifelse(crustacean == "Assemblage", "Crustacean assemblage", crustacean)
  )

n_links <- nrow(links_comp)
n_links

fish_nodes <- links_comp %>%
  distinct(name = fish) %>%
  mutate(group = "Fish")

crust_nodes <- links_comp %>%
  distinct(name = crustacean) %>%
  mutate(group = "Crustacean")

nodes <- bind_rows(fish_nodes, crust_nodes)

links_d3 <- links_comp %>%
  mutate(
    source = match(fish, nodes$name) - 1,
    target = match(crustacean, nodes$name) - 1
  ) %>%
  dplyr::select(source, target, value)

my_colour <- '
d3.scaleOrdinal()
  .domain(["Fish", "Crustacean"])
  .range(["#6BAED6", "#FDBE85"])
'

sankeyNetwork(
  Links = links_d3,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  colourScale = my_colour,
  fontSize = 16,
  nodeWidth = 30,
  sinksRight = TRUE
)

widget <- browsable(
  tagList(
    tags$h2(
      "Competition",
      style = "text-align: center; margin-bottom: 5px; font-weight: bold;"
    ),
    sankeyNetwork(
      Links = as.data.frame(links_d3),
      Nodes = as.data.frame(nodes),
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

htmltools::save_html(widget, file = "competition_sankey.html")

file.exists("competition_sankey.html")

webshot2::webshot(
  url = "competition_sankey.html",
  file = "Figure_5.png",
  vwidth = 1200,
  vheight = 900,
  zoom = 3
)

# Figure 6 ------------------------------------------------
my_colour <- '
d3.scaleOrdinal()
  .domain(["Fish", "Crustacean"])
  .range(["#6BAED6", "#FDBE85"])
'

links_pred_F <- dados %>%
  filter(interaction == "Predation") %>%
  filter(!is.na(fish), !is.na(crustacean), !is.na(if_predation_who)) %>%
  filter(if_predation_who == "F") %>%
  count(fish, crustacean, name = "value") %>%
  mutate(
    fish = ifelse(fish == "Assemblage", "Fish assemblage", fish),
    crustacean = ifelse(crustacean == "Assemblage", "Crustacean assemblage", crustacean)
  )

n_links_2 <- nrow(links_pred_F)
n_links_2

fish_nodes_F <- links_pred_F %>%
  distinct(name = fish) %>%
  mutate(group = "Fish")

crust_nodes_F <- links_pred_F %>%
  distinct(name = crustacean) %>%
  mutate(group = "Crustacean")

nodes_pred_F <- bind_rows(fish_nodes_F, crust_nodes_F)

links_pred_F_d3 <- links_pred_F %>%
  mutate(
    source = match(fish, nodes_pred_F$name) - 1,
    target = match(crustacean, nodes_pred_F$name) - 1
  ) %>%
  dplyr::select(source, target, value)

sankeyNetwork(
  Links = as.data.frame(links_pred_F_d3),
  Nodes = as.data.frame(nodes_pred_F),
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  colourScale = my_colour,
  fontSize = 16,
  nodeWidth = 30,
  sinksRight = TRUE
)


widget_pred_F <- browsable(
  tagList(
    tags$h2("Predation - fish as the predator",
            style = "text-align: center; margin-bottom: 5px; font-weight: bold;"),
    sankeyNetwork(
      Links = as.data.frame(links_pred_F_d3),
      Nodes = as.data.frame(nodes_pred_F),
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

htmltools::save_html(widget_pred_F, "predation_fish_predator.html")

webshot(
  "predation_fish_predator.html",
  file = "Figure_6.png",
  vwidth = 1200,
  vheight = 900,
  zoom = 3
)

# Figure 7 ---------------------------------------------------
links_pred_C <- dados %>%
  filter(interaction == "Predation") %>%
  filter(!is.na(fish), !is.na(crustacean), !is.na(if_predation_who)) %>%
  filter(if_predation_who == "C") %>%
  count(crustacean, fish, name = "value") %>%
  mutate(
    fish = ifelse(fish == "Assemblage", "Fish assemblage", fish),
    crustacean = ifelse(crustacean == "Assemblage", "Crustacean assemblage", crustacean)
  )

n_links_3 <- nrow(links_pred_C)
n_links_3


crust_nodes_C <- links_pred_C %>%
  distinct(name = crustacean) %>%
  mutate(group = "Crustacean")

fish_nodes_C <- links_pred_C %>%
  distinct(name = fish) %>%
  mutate(group = "Fish")

nodes_pred_C <- bind_rows(crust_nodes_C, fish_nodes_C)

links_pred_C_d3 <- links_pred_C %>%
  mutate(
    source = match(crustacean, nodes_pred_C$name) - 1,
    target = match(fish, nodes_pred_C$name) - 1
  ) %>%
  dplyr::select(source, target, value)


sankeyNetwork(
  Links = as.data.frame(links_pred_C_d3),
  Nodes = as.data.frame(nodes_pred_C),
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  colourScale = my_colour,
  fontSize = 16,
  nodeWidth = 30,
  sinksRight = TRUE
)


widget_pred_C <- browsable(
  tagList(
    tags$h2("Predation - crustacean as the predator",
            style = "text-align: center; margin-bottom: 5px; font-weight: bold;"),
    sankeyNetwork(
      Links = as.data.frame(links_pred_C_d3),
      Nodes = as.data.frame(nodes_pred_C),
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


htmltools::save_html(widget_pred_C, "predation_crustacean_predator.html")

webshot(
  "predation_crustacean_predator.html",
  file = "Figure_7.png",
  vwidth = 1200,
  vheight = 900,
  zoom = 3
)

# Figure 8 -------------------------------------------------
links_par <- dados %>%
  filter(interaction == "Parasitism") %>%
  filter(!is.na(fish), !is.na(crustacean)) %>%
  count(fish, crustacean, name = "value") %>%
  mutate(
    fish = ifelse(fish == "Assemblage", "Fish assemblage", fish),
    crustacean = ifelse(crustacean == "Assemblage", "Crustacean assemblage", crustacean)
  )

n_links_4 <- nrow(links_par)
n_links_4

fish_nodes_par <- links_par %>%
  distinct(name = fish) %>%
  mutate(group = "Fish")

crust_nodes_par <- links_par %>%
  distinct(name = crustacean) %>%
  mutate(group = "Crustacean")

nodes_par <- bind_rows(fish_nodes_par, crust_nodes_par)

links_par_d3 <- links_par %>%
  mutate(
    source = match(fish, nodes_par$name) - 1,
    target = match(crustacean, nodes_par$name) - 1
  ) %>%
  dplyr::select(source, target, value)

my_colour <- '
d3.scaleOrdinal()
  .domain(["Fish", "Crustacean"])
  .range(["#6BAED6", "#FDBE85"])
'

sankeyNetwork(
  Links = links_par_d3,
  Nodes = nodes_par,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  colourScale = my_colour,
  fontSize = 16,
  nodeWidth = 30,
  sinksRight = TRUE
)

widget_par <- browsable(
  tagList(
    tags$h2("Parasitism",
            style = "text-align: center; margin-bottom: 5px; font-weight: bold;"),
    sankeyNetwork(
      Links = as.data.frame(links_par_d3),
      Nodes = as.data.frame(nodes_par),
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

htmltools::save_html(widget_par, "parasitism_sankey.html")

webshot(
  "parasitism_sankey.html",
  file = "Figure_8.png",
  vwidth = 1200,
  vheight = 900,
  zoom = 3
)
