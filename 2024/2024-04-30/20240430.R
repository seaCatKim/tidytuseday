
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(ggforce)
library(forcats)
library(sf)
library(magick)
library(colorspace)

#library(cowplot)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-04-30")

# Worldwide Bureaucracy Indicators
wwbi_data <- tuesdata$wwbi_data
wwbi_series <- tuesdata$wwbi_series
wwbi_country <- tuesdata$wwbi_country

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "#F3F2EE"
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

# join region into data
wwbi_sum <- wwbi_data |> 
  filter(indicator_code == "BI.PWK.PUBS.FE.ZS" | indicator_code == "BI.PWK.PRVS.FE.ZS") |> 
  left_join(y = wwbi_country |> select(country_code, short_name, region, income_group)) |> 
  mutate(region = case_when(country_code == "AIA" | country_code == "MSR" ~ "Latin America & CAribbean",
                            .default = as.character(region)))|> 
  group_by(country_code, indicator_code, region, short_name, income_group) |>
  summarize(
    #average years per country
    ave = mean(value), SE = sd(value)/sqrt(n())
  ) |> 
  ungroup() |> 
 # pivot_wider(names_from = income_group, values_from = c(ave, SE), values_fill = 0) |>
#  pivot_longer(cols = `ave_Low income`:SE_NA, names_to = c(".value", "income_group"), names_sep = "_") |> 
  mutate(income_group = factor(income_group,
                               exclude = NULL,
                               levels = c("High income", "Upper middle income", "Lower middle income", "Low income", NA),
                               labels = c("High", "Upper Middle", "Lower Middle", "Low", "Not assigned")),
         income_group = fct_inorder(income_group),
         income_group = fct_relevel(income_group, "Not assigned", after = 0)) |> 
  group_by(region, indicator_code, income_group) |> 
  summarize(
    # average countries per region
    reg_ave = mean(ave), n = n(), SE = sd(ave)/sqrt(n()),
    perc = paste0(" ", sprintf("%2.0f", reg_ave * 100), "%", " ")) |> 
  ungroup() |> 
  mutate(
    perc = if_else(row_number() == 3, paste(perc, "Private Sector"), perc),
    perc = if_else(row_number() == 6, paste(perc, "Public Sector"), perc)
  )

wwbi_sum$income_group |> levels()


# BI.EMP.PWRK.PB.FE.ZS: Public sector employment, as a share of paid employment by gender: female
# BI.PWK.PUBS.FE.ZS: Females, as a share of public paid employees
# BI.PWK.PRVS.FE.ZS: Females, as a share of private paid employees

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-04-30", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "roboto"
)
title <- ""
st <- ""
cap <- paste0(
  "**Data**: <br>", social
)

# Plot --------------------------------------------------------------------

mywidth = .9

plot <- ggplot(wwbi_sum, aes(x = reg_ave, y = income_group, fill = indicator_code)) +
  geom_vline(xintercept = .5, lty = 2, alpha = 0.8, color = "red3") +
  geom_bar(stat = "identity", position = position_dodge(width = mywidth)) +
  facet_col(facets = vars(region),
            scales = "free_y",
            space = "free") +
  # add percent labels
  geom_text(aes(label = perc),  hjust = 0, 
            position = position_dodge(width = mywidth),
            size = 6, fontface = "italic", family = "sans") +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    strip.text = element_text(size = 26, hjust = 0),
    axis.text.y = element_text(size = 20, 
                               hjust = 0.9, 
                               # padding on right of strip label
                               margin = margin(0, 3, 0, 0)),
    title = element_text(size = 30),
    plot.title = element_textbox_simple(
    margin = margin(b = 8, t = 10, l = 0),
    lineheight = 0.5,
    size = rel(1.5),
    face = "bold"
    ),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col)
  ) +
  scale_x_continuous(limits = c(0,1),
                    breaks = seq(0, 0.7, by = .2), expand = c(0,0)) +
  guides(fill = "none") +
  labs(x = "Percentage of Females in the Workforce",
       y = "Country Income Level",
       title = "Proportion of Females Working in Private and Public Sectors") 

wwbi_sum |> 
#  group_by(region, .add = FALSE) |> 
  group_split(region) |>
  map(nrow)
  map(~ ggsave(filename = paste0(unique(.x$region), ".png"),
                                 plot = ggplot(.x, aes(x = reg_ave, y = indicator_code, fill = indicator_code)) +
        geom_bar(stat = "identity") +
        facet_wrap(region ~ income_group, ncol = 1) +
        theme_void() +
        theme(
          strip.text = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
        ) +
        ggtitle(.x$region),
        device = "png",
        path = "plots/")
      )

# widths all the same
width_multiplier <- 1 # try .5 also  
(num_of_bigger <- 8)
(num_of_smaller <- 2)
(width_fraction <- width_multiplier * num_of_smaller/num_of_bigger)


# map the ggplots
# bars not the same size though bc of different number of bars...
plot_list <- wwbi_sum |> 
 # group_by(region, .add = FALSE) |> 
  group_split(region) |>
  map(~ ggplot(.x, aes(x = reg_ave, y = indicator_code, fill = indicator_code)) +
        geom_bar(stat = "identity", width = 1 * nrow(.x)/15) +
        facet_grid(vars(income_group), 
                  # ncol = 1, 
                   switch = "y",
                   scales = "free_y",
                space = "free_y") +
        theme_void() +
        theme(
           strip.text = element_text(size = 20),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           title = element_text(size = 26)
         ) +
        ggtitle(.x$region) +
      guides(fill = "none")
      )

plot_list[[1]] / plot_list[[2]] / plot_list[[3]] / plot_list[[4]] / plot_list[[5]] / plot_list[[6]] / plot_list[[7]]

# World map ---------------------------------------------------------------
world <- st_read("2024/2024-04-30/shapefile/WB_countries_Admin0_10m.shp") |> 
  select(FORMAL_EN, REGION_WB, NAME_EN, WB_REGION, geometry) |> 
  filter(REGION_WB != "Antarctica")


  
st_crs(world)

world |> group_by(WB_REGION) |> summarize()

plot(world)
map <- 
  ggplot(world) +
  geom_sf(aes(fill = REGION_WB), color = NA) +
  # prevent ggplot from expanding beyond map limits
  coord_sf(expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
      #  panel.background = 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      panel.border = element_blank(),
      plot.margin = margin(0,0,0,0)) +
  scale_fill_brewer(type = "qual", palette = "Set3")+
  guides(fill = "none") +
  annotate(geom = "text", x = 130, y = 10, label = "East Asia & Pacific", size = 7) +
  annotate(geom = "text", x = 80, y = 22, label = "South Asia", size = 7) +
  annotate(geom = "text", x = 65, y = 55, label = "Europe & Central Asia", size = 7) +
  annotate(geom = "text", x = 25, y = 30, label = "Middle East & North Africa", size = 7) +
  annotate(geom = "text", x = 25, y = 0, label = "Sub-Saharan Africa", size = 7) +
  annotate(geom = "text", x = -55, y = -10, label = "South America", size = 7) +
  annotate(geom = "text", x = -100, y = 40, label = "North America", size = 7) +
  labs(x = "", y = "")
ggsave("plots/WB_region_map.png")

#myfile <- 
ggdraw() +
  # distorts the original ggplot formatting?
  draw_plot(plot) +
  draw_image("plots/WB_region_map.png", x = 0.35, vjust = .35, scale = .35)

#map <- image_read("plots/WB_region_map.png")
#image_ggplot(map)

plot + inset_element(map, left = .52, bottom = -.10, right = 1, top = 0.35, align_to = "plot")

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-04-30", paste0("20240430", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
