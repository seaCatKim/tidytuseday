
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(ggforce)


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

bg_col <- ""
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
                               labels = c("High", "Upper Middle", "Lower Middle", "Low", "Not assigned"))) |> 
  group_by(region, indicator_code, income_group) |> 
  summarize(
    # average countries per region
    reg_ave = mean(ave), n = n(), SE = sd(ave)/sqrt(n())
            ) |> 
  ungroup()

wwbi_sum


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

# labeller function

ggplot(wwbi_sum, aes(x = reg_ave, y = income_group, fill = indicator_code)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_col(facets = vars(region),
            scales = "free_y",
            space = "free") +
  # facet_grid(region ~ ., 
  #            scale = "free_y",
  #            space = "free_y",
           #  switch = "y",
            # strip.position = "left" #,
             # labeller = function(df) {
             #   list(as.character(df[,2]))
             #   }
         #    ) +
  #theme_void() +
  theme(
    strip.text = element_text(size = 20),
    strip.text.y = element_text(size = 20, angle = 0),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    title = element_text(size = 22)
  ) +
 # ggtitle(wwbi_sum$region) +
  guides(fill = "none")

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
           strip.text = element_text(size = 16),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           title = element_text(size = 22)
         ) +
        ggtitle(.x$region) +
      guides(fill = "none")
      )

plot_list[[1]] / plot_list[[2]] / plot_list[[3]] / plot_list[[4]] / plot_list[[5]] / plot_list[[6]] / plot_list[[7]]

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-04-30", paste0("20240430", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
