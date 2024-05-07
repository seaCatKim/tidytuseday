
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

# BI.PWK.PUBS.FE.ZS: Females, as a share of public paid employees
# BI.PWK.PRVS.FE.ZS: Females, as a share of private paid employees

wwbi_sum <- wwbi_data |> 
  filter(indicator_code == "BI.PWK.PUBS.FE.ZS" | indicator_code == "BI.PWK.PRVS.FE.ZS") |> 
  left_join(y = wwbi_country |> select(country_code, short_name, region, income_group)) |> 
  # assign missing regions
  mutate(region = case_when(country_code == "AIA" | country_code == "MSR" ~ "Latin America & CAribbean",
                            .default = as.character(region)))|> 
  group_by(country_code, indicator_code, region, short_name, income_group) |>
  summarize(
    ave = mean(value), SE = sd(value)/sqrt(n())
  ) |> 
  ungroup() |> 
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
    # create labels on the bars
    perc = if_else(row_number() == 3, paste(perc, "Private Sector"), perc),
    perc = if_else(row_number() == 6, paste(perc, "Public Sector"), perc))

my_cols <- RColorBrewer::brewer.pal(length(unique(wwbi_sum$region)), "Set3")
## for easier assignment, name the colors
names(my_cols) <- unique(wwbi_sum$region)

wwbi_sum <- wwbi_sum |> 
  mutate(
    # add col to make each facet by region a different color
    code_ind = as.integer(ordered(indicator_code)),
    new_cols = my_cols[region],
    ## now darken or lighten according to the rank
    code_dark = darken(new_cols, amount = code_ind / 5)
  )

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

plot <- ggplot(wwbi_sum, aes(x = reg_ave, y = income_group, fill = code_dark)) +
  scale_fill_identity() +
  # background 50% gray
  geom_rect(data=NULL,aes(xmin=0,xmax=.5,ymin=-Inf,ymax=Inf),
            fill="gray85")+
 # geom_vline(xintercept = .5, lty = 2, alpha = 0.8, color = "red3") +
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
    margin = margin(b = 4, t = 8, l = 8),
    lineheight = 0.9,
    size = rel(1.5),
    face = "bold",
    halign = 0
    ),
    plot.subtitle = element_textbox_simple( margin = margin(b = 4, t = 0, l = 8)),
    plot.caption = element_text(hjust = 0, 
                                lineheight = 0.3, size = 20,
                                margin = margin(b = 2, t = 4, l = -6)), 
    plot.title.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col)
  ) +
  scale_x_continuous(limits = c(0,1),
                    breaks = seq(0, 0.7, by = .2), expand = c(0,0)) +
  guides(fill = "none") +
  labs(x = "Percentage of Females in the Workforce",
       y = "Country Income Level",
       title = "Proportion of Females Working in Private and Public Sectors",
       subtitle = "Gender Equity in the Workforce Globally from the WorldWide Bureaucracy Indicators",
       caption = "Data: World Bank
Retrieved from: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_data.csv
https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_series.csv
https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_country.csv
Visualization: Catherine Kim")
 # annotate(
    # geom = "text",
    # x = .6,
    # y = .75,
    # label = c("", "", "", "Show some text", "", "", "")


# World map ---------------------------------------------------------------
world <- st_read("2024/2024-04-30/shapefile/WB_countries_Admin0_10m.shp") |> 
  select(FORMAL_EN, REGION_WB, NAME_EN, WB_REGION, geometry) |> 
  filter(REGION_WB != "Antarctica")

st_crs(world)

map <- ggplot(world) +
  geom_sf(aes(fill = REGION_WB), color = NA) +
  # prevent ggplot from expanding beyond map limits
  coord_sf(expand = FALSE) +
  theme_bw() +
  theme(panel.grid = element_blank(),
      #  panel.background = 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(0,0,0,0)) +
  scale_fill_brewer(type = "qual", palette = "Set3")+
  guides(fill = "none") +
  annotate(geom = "text", x = 130, y = 5, label = "East Asia & Pacific", size = 7) +
  annotate(geom = "text", x = 80, y = 20, label = "South Asia", size = 7) +
  annotate(geom = "text", x = 65, y = 60, label = "Europe & Central Asia", size = 7) +
  annotate(geom = "text", x = 15, y = 35, label = "Middle East & North Africa", size = 7) +
  annotate(geom = "text", x = 25, y = 0, label = "Sub-Saharan Africa", size = 7) +
  annotate(geom = "text", x = -65, y = -10, label = "Latin America & Caribbean", size = 7) +
  annotate(geom = "text", x = -100, y = 50, label = "North America", size = 7) +
  labs(x = "", y = "")
#ggsave("plots/WB_region_map.png")

text <- tibble(
  label = "Females have higher employment<br>
rates in the public sector than the<br>
private sector. Few regions have<br>
a majority of women working<br>
in the public sector which is more<br>
likely in higher income countries.",
  x = 0,
  y = 0,
  hjust = .5,
  vjust = 0.5,
  orientation = "upright",
  color = "black",
  fill = "cornsilk"
)

#text_wrap <- cat(stringr::str_wrap(text, width = 60), "\n"
test_text <- ggplot() + 
  geom_richtext(data = text, aes(x,y, label = label, 
                                 hjust = hjust, vjust = vjust), 
           # width = unit(.7, "npc"),
              fill = bg_col, 
            size = 10,
            lineheight = .35,
           label.r = unit(0.5, "lines"),
           label.padding = unit(0.5, "lines")) +
  theme_void()
# put map as inset on plot
plot + 
  inset_element(map, left = .53, bottom = -.05, right = 1, top = 0.35, align_to = "plot") + 
  inset_element(test_text, left = .3, bottom = .9, right = 1.3, top = .25, align_to = "panel")


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-04-30", paste0("20240430", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
