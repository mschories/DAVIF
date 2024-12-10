library(ggtext)

# systemfonts::register_font(name="Skia-Regular_Condensed", plain = "Skia-Regular_Condensed")
# systemfonts::register_font("Skia-Regular_Condensed")
typo_mono <- "mono"

# typo_mono <- "Roboto Mono"

# gdtools::register_gfont("Roboto Mono")
# typo_mono <- "Roboto Mono"

theme_ffg_base <- theme(
    plot.title.position = "plot",
    axis.title = element_blank(),
    axis.text = element_text(family = typo_mono, size = 10, hjust = 0),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0,"in"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0, "in"),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.justification = "center",
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(family = typo_mono, size = 12, hjust = 0.5),
    legend.margin = margin(0, .2, 0, .2, "in"),
    legend.key = element_blank(),
    text = element_text(family = typo_mono, size = 12, hjust = 0)
  )
  
ffg_facets <- theme(
    strip.text = element_text(family = typo_mono, size = 11, hjust = 0),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "#f2f2f2")
  )

ffg_facets_polar <- theme(
  strip.text = element_text(family = typo_mono, size = 10, hjust = 0),
  axis.text.y = element_blank(),
  panel.background = element_rect(fill = "#f2f2f2", color = NA),
  panel.grid.major.y = element_line(color = "#c2c2c2", linewidth = .2),
  panel.grid.major.x = element_line(color = "#e2e2e2", linewidth = .1),
  panel.grid.minor.y = element_blank()
)



ffg_facets_occupations <- theme(
  strip.text = element_text(family = typo_mono, size = 11, hjust = 0),
  # axis.text.y = element_blank(),
  panel.grid.major.y = element_line(color = "#c2c2c2", linewidth = .2),
  panel.background = element_rect(fill = "#f2f2f2")
) 

ffg_legend_justification_left <- theme(
  legend.location = "plot"
)
