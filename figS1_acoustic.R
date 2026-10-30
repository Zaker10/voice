## =============================================================================
##  figS1_acoustic.R
##
##  Supplementary Figure S1. Acoustic characterization of the five conditions.
##  Values are the openSMILE eGeMAPSv02 functionals for the ten audio tracks,
##  reproduced from Supplementary Table S2, so the script needs no audio.
##  Output: figures/figS1_acoustic.pdf and figures/figS1_acoustic.png
## =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# Run from the repository root; save generated files outside version control.
FIG_DIR <- Sys.getenv("NPJ_FIG_DIR", unset = "figures")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

## Colour-blind-accessible blue-orange palette
COL_HUMAN <- "#468EB7"
COL_AI    <- "#CC584E"

## Condition order used throughout the manuscript and supplement
ORD       <- c("HNV", "HCV", "AINV", "AICV", "AIENV")

BASE_SIZE <- 7
OUT_W     <- 7.2
OUT_H     <- 3.6

## Rows ordered by lesson and condition; columns are the eight indices in
## panel order (Supplementary Table S2).
acou <- tribble(
  ~voice,  ~lesson,   ~f0_range, ~f0_slope, ~loud_range, ~loud_peaks, ~voiced, ~unvoiced, ~flux, ~hnr,
  
  "HNV",   "Physics",      9.25,    424.46,        1.73,        3.78,    1.64,     0.100,  1.40, 6.98,
  "HCV",   "Physics",      7.07,    302.12,        1.62,        3.84,    0.96,     0.165,  1.21, 9.83,
  "AINV",  "Physics",      7.89,    493.64,        1.20,        4.83,    2.19,     0.084,  1.40, 6.82,
  "AICV",  "Physics",      9.93,    555.47,        1.44,        4.04,    1.80,     0.115,  1.43, 6.34,
  "AIENV", "Physics",      8.75,    460.66,        2.14,        4.65,    2.07,     0.080,  1.84, 4.84,
  
  "HNV",   "Biology",      9.51,    364.48,        1.68,        3.84,    1.39,     0.094,  1.41, 6.89,
  "HCV",   "Biology",      7.20,    262.30,        1.55,        3.95,    0.94,     0.153,  1.21, 9.76,
  "AINV",  "Biology",      7.78,    488.07,        1.20,        4.81,    1.82,     0.085,  1.42, 6.59,
  "AICV",  "Biology",      9.97,    488.66,        1.42,        4.07,    1.52,     0.105,  1.41, 6.28,
  "AIENV", "Biology",     12.08,    386.91,        1.37,        4.74,    1.24,     0.068,  1.88, 3.89
)

panels <- c(
  f0_range   = "a   F0 range\n(P20-P80, semitones)",
  f0_slope   = "b   Mean rising F0 slope\n(semitones per s)",
  loud_range = "c   Loudness range\n(P20-P80)",
  loud_peaks = "d   Loudness peaks\n(n per s)",
  voiced     = "e   Voiced segments\n(n per s)",
  unvoiced   = "f   Mean unvoiced\nsegment length (s)",
  flux       = "g   Spectral flux\n(voiced)",
  hnr        = "h   Harmonics-to-noise\nratio (dB)"
)

dat <- acou %>%
  pivot_longer(
    -c(voice, lesson),
    names_to = "index",
    values_to = "value"
  ) %>%
  mutate(
    voice = factor(voice, levels = ORD),
    lesson = factor(lesson, levels = c("Physics", "Biology")),
    source = if_else(
      as.character(voice) %in% c("HNV", "HCV"),
      "Human-recorded",
      "AI-generated"
    ),
    source = factor(
      source,
      levels = c("Human-recorded", "AI-generated")
    ),
    panel = factor(
      panels[index],
      levels = unname(panels)
    )
  )

p <- ggplot(
  dat,
  aes(x = voice, y = value, fill = source, alpha = lesson)
) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.72,
    color = "grey20",
    linewidth = 0.2
  ) +
  facet_wrap(
    ~ panel,
    nrow = 2,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Human-recorded" = COL_HUMAN,
      "AI-generated" = COL_AI
    ),
    breaks = c("Human-recorded", "AI-generated")
  ) +
  scale_alpha_manual(
    values = c(
      "Physics" = 1,
      "Biology" = 0.58
    ),
    breaks = c("Physics", "Biology")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    alpha = NULL
  ) +
  guides(
    alpha = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  theme_classic(base_size = BASE_SIZE) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      size = BASE_SIZE - 0.5,
      hjust = 0,
      lineheight = 1.15
    ),
    panel.spacing.x = unit(9, "pt"),
    panel.spacing.y = unit(11, "pt"),
    axis.text.x = element_text(size = BASE_SIZE - 1),
    legend.position = "bottom",
    legend.key.size = unit(8, "pt"),
    legend.box = "horizontal",
    legend.margin = margin(t = -4)
  )

print(p)

ggsave(
  file.path(FIG_DIR, "figS1_acoustic.pdf"),
  p,
  width = OUT_W,
  height = OUT_H,
  device = cairo_pdf
)

ggsave(
  file.path(FIG_DIR, "figS1_acoustic.png"),
  p,
  width = OUT_W,
  height = OUT_H,
  dpi = 300
)

message("Wrote figS1_acoustic.pdf and figS1_acoustic.png")