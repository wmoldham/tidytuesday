# 2022-11-15.R


# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)


# import data -------------------------------------------------------------

path <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/"

image_alt <- read_csv(str_c(path, "image_alt.csv"))
color_contrast <- read_csv(str_c(path, "color_contrast.csv"))
ally_scores <- read_csv(str_c(path, "ally_scores.csv"))
bytes_total <- read_csv(str_c(path, "bytes_total.csv"), col_types = rep("c", 9))
speed_index <- read_csv(str_c(path, "speed_index.csv"))


# recreate graph ----------------------------------------------------------

bytes_total |>
  mutate(
    date = lubridate::as_date(date),
  ) |>
  ggplot() +
  aes(
    x = date,
    y = p50,
    color = client
  ) +
  geom_line()


# assemble data -----------------------------------------------------------

fmt_pct <- function(df) {
  nm <- str_c(unique(df$measure), "_pct")
  df |>
    select(date, timestamp, client, !!nm := percent)
}

fmt_prob <- function(df) {
  nm <- str_c(unique(df$measure), "_avg")
  df |>
    select(date, timestamp, client, !!nm := p50)
}

df <-
  reduce(
    list(
      fmt_pct(image_alt),
      fmt_pct(color_contrast),
      fmt_prob(ally_scores),
      fmt_prob(bytes_total),
      fmt_prob(speed_index)
    ),
    full_join,
    by = c("date", "timestamp", "client")
  ) |>
  mutate(
    date = as_date(date),
    timestamp = as_datetime(timestamp),
    client = factor(client, levels = c("desktop", "mobile"), labels = c("Desktop", "Mobile"))
  )

# ally scores for mobile go back to June 2017
# ally scores for desktop go back to May 2022
# ally scores only available for 90 records
# speed index only goes to 2016


# plots -------------------------------------------------------------------

p1 <-
  ggplot(df) +
  aes(
    x = date,
    y = bytesTotal_avg,
    color = client
  ) +
  geom_line() +
  labs(
    y = "Page weight (kB)",
    x = NULL,
    color = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_light()

p2 <-
  ggplot(df) +
  aes(
    x = date,
    y = speedIndex_avg,
    color = client
  ) +
  geom_line() +
  labs(
    y = "Display speed (s)",
    x = NULL,
    color = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_light()

p3 <-
  ggplot(df) +
  aes(
    x = date,
    y = a11yImageAlt_pct,
    color = client
  ) +
  geom_line() +
  labs(
    y = "Alt text (%)",
    x = NULL,
    color = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_light()

p4 <-
  ggplot(df) +
  aes(
    x = date,
    y = a11yColorContrast_pct,
    color = client
  ) +
  geom_line() +
  labs(
    y = "Color contrast (%)",
    x = NULL,
    color = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_light()

p5 <-
  ggplot(df) +
  aes(
    x = date,
    y = a11yScores_avg,
    color = client
  ) +
  geom_line() +
  labs(
    y = "Accessibility score",
    x = NULL,
    color = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_light()

design <- "
ab#
cde
"

p1 + p2 + p3 + p4 + p5 +
  plot_annotation(
    title = "Web Page Metrics",
    subtitle = "httparchive.org",
    caption = "William Oldham\n"
  ) +
  plot_layout(
    ncol = 1,
    guides = "collect"
  ) &
  theme(
    text = element_text(family = "Avenir", size = 10),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(face = "italic"),
    legend.position = "bottom"
  )
