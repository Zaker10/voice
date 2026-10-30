## =============================================================================
##  figures_main.R
##
##  Main-text figures for "Effects of voice source and prosodic style on
##  learner ratings, learning outcomes, and arousal" (npj Science of Learning).
##
##  Produces:
##    Fig. 2  Perceptual validation of the narration conditions
##    Fig. 3  Learner evaluation by voice source and prosodic style
##    Fig. 4  Learning outcomes by voice condition
##    Fig. 5  Voice-condition estimates, model-based and CR2 intervals
##
##  Physiological variables are read as baseline-corrected values already
##  present in the data; this script applies no further normalization.
##  Run from the repository root (or set NPJ_DATA_DIR and NPJ_FIG_DIR).
##
##  Figure 2a and 2b: participant-cluster percentile bootstrap (B = 5000);
##  each sampled listener contributes all 10 excerpts (5 voices x 2 lessons).
##  Figure 3 and 4: KR model-based confidence intervals.
##  Figure 5: KR/CR2 intervals from the same LMM, asymptotic Wald for
##  ordinal/logistic sensitivity models.
##  This script draws figures, not adjusted p-values for planned contrasts.
## =============================================================================

## ---- 0. Packages ------------------------------------------------------------
pkgs <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "patchwork",
  "lme4", "lmerTest", "ordinal", "emmeans", "pbkrtest", "clubSandwich"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Missing packages: ", paste(missing, collapse = ", "),
    "\nInstall with: install.packages(c(",
    paste0('"', missing, '"', collapse = ", "), "))"
  )
}

invisible(lapply(pkgs, library, character.only = TRUE))

emmeans::emm_options(
  lmer.df = "kenward-roger",
  pbkrtest.limit = 6000,
  lmerTest.limit = 6000
)

## ---- 0.1 Paths and constants ------------------------------------------------
# Run from the repository root. Optionally override with environment variables.
DATA_DIR  <- Sys.getenv("NPJ_DATA_DIR", unset = "data")
FIG_DIR   <- Sys.getenv("NPJ_FIG_DIR", unset = "figures")
FILE_MAIN <- file.path(DATA_DIR, "data.xlsx")
FILE_PV   <- file.path(DATA_DIR, "perception.xlsx")

# The perception workbook contains 90 listeners x 10 excerpts.
# Set this explicitly only if automatic identification cannot find the ID column.
PV_LISTENER_COL <- NULL  # e.g., "participant_id"
BOOT_B <- 5000L
BOOT_SEED <- 20261029L

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(FILE_MAIN)) stop("Learner-data file not found: ", FILE_MAIN)
if (!file.exists(FILE_PV))   stop("Perception-data file not found: ", FILE_PV)

VOICE_LEVELS <- c("HNV", "HCV", "AINV", "AICV", "AIENV")
VOICE_RECODE <- c(
  "1" = "HNV", "2" = "HCV", "3" = "AINV",
  "4" = "AICV", "5" = "AIENV"
)

LESSON_RECODE <- c("0" = "physics", "1" = "biology")
LESSON_LEVELS <- c("physics", "biology")

SRC <- c(
  HNV = "Human-recorded", HCV = "Human-recorded",
  AINV = "AI-generated", AICV = "AI-generated", AIENV = "AI-generated"
)

STYLE <- c(
  HNV = "Neutral", HCV = "Calm", AINV = "Neutral",
  AICV = "Calm", AIENV = "Energetic"
)
STYLE_LEVELS <- c("Calm", "Neutral", "Energetic")

COL_HUMAN <- "#468EB7"
COL_AI    <- "#CC584E"

COL_SRC <- c(
  "Human-recorded" = COL_HUMAN,
  "AI-generated"   = COL_AI
)

## Two-source colour system; shapes distinguish voices.
COL_VOICE <- c(
  HNV = COL_HUMAN, HCV = COL_HUMAN,
  AINV = COL_AI, AICV = COL_AI, AIENV = COL_AI
)

# Match the figure captions: neutral = filled triangle, calm = filled
# circle, energetic = open triangle. Source is encoded by colour.
SHAPE_VOICE <- c(
  HNV = 17, HCV = 16,
  AINV = 17, AICV = 16, AIENV = 2
)

## ---- 0.2 Publication theme --------------------------------------------------
theme_npj <- function(base_size = 8.5) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        linewidth = 0.22, colour = "grey89"
      ),
      panel.border = ggplot2::element_rect(
        linewidth = 0.38, colour = "grey35"
      ),
      strip.background = ggplot2::element_rect(
        fill = "grey96", linewidth = 0.35, colour = "grey55"
      ),
      strip.text = ggplot2::element_text(size = base_size - 0.2),
      plot.title = ggplot2::element_text(
        size = base_size, face = "plain", hjust = 0.5,
        margin = ggplot2::margin(b = 4)
      ),
      plot.tag = ggplot2::element_text(
        size = base_size + 3, face = "bold"
      ),
      legend.key.size = grid::unit(3.4, "mm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 1, 0),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 0.3),
      axis.text = ggplot2::element_text(colour = "grey20"),
      axis.title = ggplot2::element_text(colour = "grey15"),
      plot.margin = ggplot2::margin(4, 5, 4, 5)
    )
}

ggplot2::theme_set(theme_npj())

save_fig <- function(plot, name, width_mm, height_mm) {
  ggplot2::ggsave(
    filename = file.path(FIG_DIR, paste0(name, ".pdf")),
    plot = plot,
    width = width_mm, height = height_mm, units = "mm",
    device = "pdf", bg = "white", limitsize = FALSE
  )
  
  ggplot2::ggsave(
    filename = file.path(FIG_DIR, paste0(name, ".png")),
    plot = plot,
    width = width_mm, height = height_mm, units = "mm",
    dpi = 600, bg = "white", limitsize = FALSE
  )
  
  invisible(NULL)
}

recode_col <- function(x, target, map) {
  x <- trimws(as.character(x))
  hit <- x %in% names(map)
  x[hit] <- unname(map[x[hit]])
  factor(x, levels = target)
}

## ---- 1. Learner data --------------------------------------------------------
d <- readxl::read_excel(FILE_MAIN, sheet = 1) |>
  as.data.frame()

d$VoiGrp <- recode_col(d$VoiGrp, VOICE_LEVELS, VOICE_RECODE)
d$lesson <- recode_col(d$lesson, LESSON_LEVELS, LESSON_RECODE)
d$Class  <- factor(trimws(as.character(d$Class)))
# Ensure participant random intercepts cannot accidentally join students
# from different classes if source ID numbers restart within each class.
d$ID <- interaction(d$Class, factor(d$ID), drop = TRUE)

d$Sat_sum  <- rowSums(d[, c("Sat1", "Sat2")])
d$VSP_soc  <- rowSums(d[, c("VSP2", "VSP3", "VSP5", "VSP6", "VSP7")])
d$VSP_int  <- d$VSP1 + d$VSP4  # VSP4 is already reverse-scored in the file
d$CL_intr  <- rowSums(d[, c("CogLoad1", "CogLoad2")])
d$CL_extr  <- rowSums(d[, c("CogLoad3", "CogLoad4", "CogLoad5")])
d$LE_total <- rowSums(d[, paste0("LearnEng", 1:6)])
d$Pre_c    <- as.numeric(scale(d$Pre, scale = FALSE))
d$pre_prop_c <- as.numeric(scale(d$Pre / 8, scale = FALSE))

## dHR, dSCL and daSCR are assumed to contain baseline-
## corrected changes. No within-participant normalization is performed here.

## Post-test items in long form for the item-level model in Fig. 4.
dfi <- d |>
  dplyr::select(
    ID, lesson, Class, VoiGrp, pre_prop_c,
    dplyr::all_of(paste0("Post", 1:8))
  ) |>
  tidyr::pivot_longer(
    dplyr::all_of(paste0("Post", 1:8)),
    names_to = "item", values_to = "correct"
  ) |>
  dplyr::mutate(item = factor(paste(lesson, item, sep = "_"))) |>
  as.data.frame()

sat_long <- d |>
  dplyr::select(ID, Class, VoiGrp, lesson, Sat1, Sat2) |>
  tidyr::pivot_longer(
    c(Sat1, Sat2), names_to = "item", values_to = "sat_resp"
  ) |>
  dplyr::mutate(
    item = factor(item),
    sat_resp = ordered(sat_resp, levels = 1:5)
  ) |>
  as.data.frame()


## =============================================================================
## 2. FIGURE 2 -- perceptual validation
## =============================================================================

PV_CONSTRUCTS <- list(
  Calmness       = c("calm_1", "calm_2"),
  Energy         = c("energy_1", "energy_2"),
  Neutrality     = c("neutral_1", "neutral_2"),
  Expressiveness = c("express_1", "express_2", "express_3"),
  Naturalness    = c("natural_1", "natural_2", "natural_3"),
  Syntheticness  = c("synthetic_1", "synthetic_2", "synthetic_3"),
  Clarity        = c("clarity_1", "clarity_2"),
  Quality        = "voice_quality",
  Pleasantness   = "pleasantness"
)

TRUE_SOURCE <- c(
  HNV = "human", HCV = "human",
  AINV = "ai", AICV = "ai", AIENV = "ai"
)

pv <- readxl::read_excel(FILE_PV) |>
  as.data.frame()

# Accept either condition codes (1-5) or labels (HNV-AIENV), even if mixed.
pv$voice <- recode_col(pv$condition_code, VOICE_LEVELS, VOICE_RECODE)
if (anyNA(pv$voice)) {
  stop("Unrecognized condition_code in perception.xlsx; inspect unique(pv$condition_code).")
}

for (nm in names(PV_CONSTRUCTS)) {
  pv[[nm]] <- rowMeans(pv[, PV_CONSTRUCTS[[nm]], drop = FALSE])
}

norm_src <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("ai-generated", "ai generated", "synthetic", "machine")] <- "ai"
  x[is.na(x) | x %in% c("not sure", "unsure", "na", "")] <- "unsure"
  x
}

pv$src        <- norm_src(pv$source_judgment)
pv$src_commit <- pv$src != "unsure"
pv$src_hit    <- pv$src == TRUE_SOURCE[as.character(pv$voice)]

stopifnot(all(pv$src[pv$src_commit] %in% c("human", "ai")))

## -- 2a + 2b. Listener-cluster bootstrap -------------------------------
## Both panels use 95% percentile intervals from the *same* 5,000
## listener-level bootstrap samples. Each selected listener contributes
## all their evaluations, including both lessons and all five conditions.
## These are marginal descriptives, not CIs for differences between voices.

resolve_listener_col <- function(dat, specified = PV_LISTENER_COL) {
  if (!is.null(specified)) {
    if (!specified %in% names(dat)) {
      stop("PV_LISTENER_COL not found in perception.xlsx: ", specified)
    }
    return(specified)
  }

  candidates <- intersect(
    c("listener_id", "listenerID", "Listener_ID", "listener",
      "participant_id", "ParticipantID", "participant", "Participant",
      "subject_id", "subject", "respondent_id", "respondent",
      "ID", "id", "pid"),
    names(dat)
  )
  candidates <- candidates[vapply(candidates, function(nm) {
    x <- dat[[nm]]
    if (anyNA(x) || length(unique(x)) != 90L) return(FALSE)
    all(as.integer(table(x)) == 10L)
  }, logical(1))]

  if (length(candidates) != 1L) {
    stop(
      "Cannot uniquely determine listener ID column in perception.xlsx. ",
      "Set PV_LISTENER_COL manually (e.g. 'participant_id'). ",
      "Detected candidates: ", paste(candidates, collapse = ", ")
    )
  }
  candidates
}

listener_col <- resolve_listener_col(pv)
pv$listener <- factor(trimws(as.character(pv[[listener_col]])))
listener_ids <- levels(pv$listener)
n_listener <- length(listener_ids)
n_voice <- length(VOICE_LEVELS)
construct_names <- names(PV_CONSTRUCTS)

if (n_listener != 90L || nrow(pv) != 900L) {
  stop("Perception data mismatch: expected 90 listeners and 900 evaluations; found ",
       n_listener, " listeners and ", nrow(pv), " evaluations.")
}

# The planned listening study has 2 excerpts per voice per listener.
listener_voice_n <- xtabs(~ listener + voice, data = pv)
if (any(listener_voice_n != 2L)) {
  stop("Perception design mismatch: each listener should evaluate ",
       "two excerpts for each of the five voice conditions.")
}

pv_long <- pv |>
  dplyr::select(listener, voice, dplyr::all_of(construct_names)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(construct_names),
    names_to = "construct", values_to = "rating"
  ) |>
  dplyr::mutate(
    voice = factor(voice, levels = VOICE_LEVELS),
    construct = factor(construct, levels = construct_names)
  )

# For each listener x voice x construct, precompute the rating sum and
# nonmissing count. A bootstrap sample is then just a set of weights
# on these listener-level cells, so repeated observations are not split.
a_cells <- pv_long |>
  dplyr::group_by(listener, voice, construct) |>
  dplyr::summarise(
    rating_sum = sum(rating, na.rm = TRUE),
    n_rating = sum(!is.na(rating)),
    .groups = "drop"
  )

# Deterministic column order: the 9 constructs inside each of 5 voices.
cell_keys <- unlist(lapply(
  VOICE_LEVELS, function(v) paste(v, construct_names, sep = "__")
), use.names = FALSE)

sum_mat <- matrix(0, nrow = n_listener, ncol = length(cell_keys),
                  dimnames = list(listener_ids, cell_keys))
count_mat <- sum_mat
cell_indices <- cbind(
  match(as.character(a_cells$listener), listener_ids),
  match(paste(a_cells$voice, a_cells$construct, sep = "__"), cell_keys)
)
stopifnot(!anyNA(cell_indices))
sum_mat[cell_indices] <- a_cells$rating_sum
count_mat[cell_indices] <- a_cells$n_rating
if (any(colSums(count_mat) == 0)) {
  stop("At least one voice-by-construct cell has no valid ratings.")
}

# Accuracy must be calculated using only committed human/AI responses.
# 'Unsure' is retained in the raw data but excluded from the denominator.
b_cells <- pv |>
  dplyr::mutate(
    committed = as.integer(src_commit),
    hit = as.integer(src_commit & src_hit)
  ) |>
  dplyr::group_by(listener, voice) |>
  dplyr::summarise(
    n_hit = sum(hit),
    n_committed = sum(committed),
    .groups = "drop"
  )

hit_mat <- matrix(0, nrow = n_listener, ncol = n_voice,
                  dimnames = list(listener_ids, VOICE_LEVELS))
committed_mat <- hit_mat
b_indices <- cbind(
  match(as.character(b_cells$listener), listener_ids),
  match(as.character(b_cells$voice), VOICE_LEVELS)
)
stopifnot(!anyNA(b_indices))
hit_mat[b_indices] <- b_cells$n_hit
committed_mat[b_indices] <- b_cells$n_committed
if (any(colSums(committed_mat) == 0)) {
  stop("At least one voice condition has no committed source judgments.")
}

# One column = one bootstrap replicate; rows are multiplicities of listeners.
set.seed(BOOT_SEED)
bootstrap_weights <- replicate(
  BOOT_B, tabulate(
    sample.int(n_listener, n_listener, replace = TRUE),
    nbins = n_listener
  )
)

boot_mean <- crossprod(bootstrap_weights, sum_mat) /
  crossprod(bootstrap_weights, count_mat)
boot_accuracy <- 100 * crossprod(bootstrap_weights, hit_mat) /
  crossprod(bootstrap_weights, committed_mat)

percentile_ci <- function(bootstrap_matrix, column_names) {
  # Avoid silently reporting intervals based on very few valid draws.
  n_valid <- colSums(is.finite(bootstrap_matrix))
  if (any(n_valid < 0.99 * nrow(bootstrap_matrix))) {
    stop("Over 1% of bootstrap replicates are undefined for one or more cells.")
  }
  interval <- t(apply(
    bootstrap_matrix, 2L, stats::quantile,
    probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE
  ))
  rownames(interval) <- column_names
  colnames(interval) <- c("lower", "upper")
  interval
}

a_intervals <- percentile_ci(boot_mean, cell_keys)
b_intervals <- percentile_ci(boot_accuracy, VOICE_LEVELS)

# 2a data: 5 voices x 9 constructs, with observed means and bootstrap CIs.
pv_plot <- expand.grid(
  construct = construct_names,
  voice = VOICE_LEVELS,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
pv_plot$key <- paste(pv_plot$voice, pv_plot$construct, sep = "__")
pv_plot$m <- (colSums(sum_mat) / colSums(count_mat))[pv_plot$key]
pv_plot$lower <- a_intervals[pv_plot$key, "lower"]
pv_plot$upper <- a_intervals[pv_plot$key, "upper"]
pv_plot$n_rating <- as.integer(colSums(count_mat)[pv_plot$key])
pv_plot$voice <- factor(pv_plot$voice, levels = VOICE_LEVELS)
pv_plot$construct <- factor(pv_plot$construct, levels = construct_names)
pv_plot$source <- SRC[as.character(pv_plot$voice)]

pd_voice <- ggplot2::position_dodge(width = 0.68)
p2a <- ggplot2::ggplot(
  pv_plot,
  ggplot2::aes(
    x = construct, y = m,
    colour = voice, shape = voice, group = voice
  )
) +
  ggplot2::geom_hline(
    yintercept = 4, linewidth = 0.22,
    linetype = "dotted", colour = "grey60"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lower, ymax = upper),
    width = 0, linewidth = 0.45, colour = "grey25", position = pd_voice
  ) +
  ggplot2::geom_point(
    size = 1.65, stroke = 0.55,
    position = pd_voice
  ) +
  ggplot2::scale_colour_manual(
    values = COL_VOICE, breaks = VOICE_LEVELS, name = NULL
  ) +
  ggplot2::scale_shape_manual(
    values = SHAPE_VOICE, breaks = VOICE_LEVELS, name = NULL
  ) +
  ggplot2::scale_y_continuous(
    limits = c(1, 7), breaks = 1:7,
    expand = ggplot2::expansion(mult = c(0.01, 0.03))
  ) +
  ggplot2::labs(x = NULL, y = "Rating (1-7)") +
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      nrow = 1, byrow = TRUE,
      override.aes = list(size = 2.1)
    ),
    shape = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
  ) +
  ggplot2::theme(
    legend.position = "top",
    axis.text.x = ggplot2::element_text(angle = 28, hjust = 1, vjust = 1)
  )

# 2b data: committed-response accuracy with the SAME listener resamples.
acc <- data.frame(
  voice = factor(VOICE_LEVELS, levels = VOICE_LEVELS),
  pct = 100 * colSums(hit_mat) / colSums(committed_mat),
  n = as.integer(colSums(committed_mat)),
  lower = b_intervals[VOICE_LEVELS, "lower"],
  upper = b_intervals[VOICE_LEVELS, "upper"]
)

p2b <- ggplot2::ggplot(
  acc,
  ggplot2::aes(x = voice, y = pct, fill = voice)
) +
  ggplot2::geom_col(width = 0.62, colour = NA) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lower, ymax = upper),
    width = 0.16, linewidth = 0.32, colour = "grey25"
  ) +
  ggplot2::geom_hline(
    yintercept = 50, linetype = "dashed",
    linewidth = 0.35, colour = "grey35"
  ) +
  ggplot2::scale_fill_manual(values = COL_VOICE, guide = "none") +
  ggplot2::scale_y_continuous(
    limits = c(0, 100), breaks = seq(0, 100, 20),
    expand = ggplot2::expansion(mult = c(0, 0.03))
  ) +
  ggplot2::labs(x = NULL, y = "Source judged correctly (%)")

# These tables contain only aggregated results, not individual listener data.
utils::write.csv(
  pv_plot[, c("voice", "construct", "m", "lower", "upper", "n_rating")],
  file.path(FIG_DIR, "fig2a_bootstrap_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  acc,
  file.path(FIG_DIR, "fig2b_bootstrap_summary.csv"),
  row.names = FALSE
)

## -- 2c. Style and source axes ------------------------------------------------
loc <- pv_plot |>
  dplyr::select(construct, voice, m) |>
  tidyr::pivot_wider(names_from = construct, values_from = m) |>
  dplyr::mutate(
    style_axis = Calmness - Energy,
    source_axis = Syntheticness,
    source = SRC[as.character(voice)],
    style = STYLE[as.character(voice)]
  )

pair_calm    <- loc[loc$style == "Calm", ]
pair_neutral <- loc[loc$style == "Neutral", ]

p2c <- ggplot2::ggplot(
  loc,
  ggplot2::aes(x = style_axis, y = source_axis)
) +
  ggplot2::geom_hline(
    yintercept = 4, linewidth = 0.22,
    linetype = "dotted", colour = "grey70"
  ) +
  ggplot2::geom_vline(
    xintercept = 0, linewidth = 0.22,
    linetype = "dotted", colour = "grey70"
  ) +
  ggplot2::geom_line(
    data = pair_calm,
    ggplot2::aes(group = 1),
    linewidth = 0.70, colour = "grey35"
  ) +
  ggplot2::geom_line(
    data = pair_neutral,
    ggplot2::aes(group = 1),
    linewidth = 0.42, colour = "grey60", linetype = "dotted"
  ) +
  ggplot2::geom_point(
    ggplot2::aes(colour = voice, shape = voice),
    size = 2.35, stroke = 0.55, fill = "white"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = voice),
    nudge_x = 0.14, hjust = 0,
    size = 2.25, colour = "grey20"
  ) +
  ggplot2::scale_colour_manual(values = COL_VOICE, guide = "none") +
  ggplot2::scale_shape_manual(values = SHAPE_VOICE, guide = "none") +
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(0.10, 0.22))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(1, 7), breaks = 1:7,
    expand = ggplot2::expansion(mult = c(0.02, 0.03))
  ) +
  ggplot2::labs(
    x = "Calmness - energy rating",
    y = "Syntheticness rating"
  ) +
  ggplot2::coord_cartesian(clip = "off")

fig2 <- (
  p2a /
    (p2b | p2c)
) +
  patchwork::plot_layout(heights = c(1.05, 1)) +
  patchwork::plot_annotation(tag_levels = "a")

save_fig(fig2, "fig2_manipulation_validation", 180, 145)


## =============================================================================
## 3. FIGURE 3 -- descriptive source-by-style profiles (incomplete factorial design)
## =============================================================================

emm_by_condition <- function(outcome) {
  f <- stats::as.formula(
    paste(outcome, "~ VoiGrp + lesson + (1|Class) + (1|ID)")
  )
  
  m <- lmerTest::lmer(f, data = d)
  
  out <- as.data.frame(
    summary(emmeans::emmeans(m, ~ VoiGrp))
  )
  
  out$style <- factor(
    STYLE[as.character(out$VoiGrp)],
    levels = STYLE_LEVELS
  )
  
  out$source <- factor(
    SRC[as.character(out$VoiGrp)],
    levels = names(COL_SRC)
  )
  
  out
}

condition_profile_panel <- function(dat, ylab) {
  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = style, y = emmean,
      colour = source, group = source
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(linetype = source),
      linewidth = 0.55
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower.CL, ymax = upper.CL),
      width = 0.07, linewidth = 0.40
    ) +
    ggplot2::geom_point(size = 1.9) +
    ggplot2::scale_colour_manual(values = COL_SRC) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Human-recorded" = "solid",
        "AI-generated" = "22"
      )
    ) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = "Prosodic style", y = ylab)
}

sat_emm <- emm_by_condition("Sat_sum")
vsp_emm <- emm_by_condition("VSP_soc")

p3a <- condition_profile_panel(
  sat_emm,
  "Satisfaction, summed (2-10)"
)

p3b <- condition_profile_panel(
  vsp_emm,
  "Voice evaluation, social (5-25)"
)

fig3 <- (
  (p3a | p3b) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(tag_levels = "a")
) &
  ggplot2::theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical"
  )

save_fig(fig3, "fig3_interaction", 180, 78)


## =============================================================================
## 4. FIGURE 4 -- learning outcomes
## =============================================================================



## Individual lesson-level trajectories, plus condition means.
traj <- d |>
  dplyr::mutate(obs = dplyr::row_number()) |>
  dplyr::select(obs, VoiGrp, Pre, Post) |>
  tidyr::pivot_longer(
    c(Pre, Post),
    names_to = "time",
    values_to = "score"
  ) |>
  dplyr::mutate(
    x = ifelse(time == "Pre", 1, 2),
    time = factor(time, levels = c("Pre", "Post"))
  )

traj_mean <- traj |>
  dplyr::group_by(VoiGrp, time, x) |>
  dplyr::summarise(
    score = mean(score, na.rm = TRUE),
    .groups = "drop"
  )

p4a <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = traj,
    ggplot2::aes(x = x, y = score, group = obs),
    linewidth = 0.12,
    alpha = 0.14,
    colour = "grey55"
  ) +
  ggplot2::geom_line(
    data = traj_mean,
    ggplot2::aes(
      x = x, y = score,
      group = VoiGrp, colour = VoiGrp
    ),
    linewidth = 0.82
  ) +
  ggplot2::geom_point(
    data = traj_mean,
    ggplot2::aes(
      x = x, y = score,
      colour = VoiGrp, shape = VoiGrp
    ),
    size = 1.75,
    stroke = 0.55,
    fill = "white"
  ) +
  ggplot2::scale_colour_manual(
    values = COL_VOICE,
    breaks = VOICE_LEVELS,
    name = NULL
  ) +
  ggplot2::scale_shape_manual(
    values = SHAPE_VOICE,
    breaks = VOICE_LEVELS,
    name = NULL
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Pre-test", "Post-test"),
    limits = c(0.92, 2.08)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 8),
    breaks = 0:8,
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::labs(x = NULL, y = "Items correct (of 8)") +
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      ncol = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 0.8, size = 2)
    ),
    shape = ggplot2::guide_legend(
      ncol = 1,
      byrow = TRUE
    )
  ) +
  ggplot2::theme(legend.position = "top")

m_post <- lmerTest::lmer(
  Post ~ VoiGrp + Pre_c + lesson + (1 | Class) + (1 | ID),
  data = d
)

adj <- as.data.frame(
  summary(
    emmeans::emmeans(
      m_post, ~ VoiGrp,
      at = list(Pre_c = 0)
    )
  )
)

adj$VoiGrp <- factor(adj$VoiGrp, levels = VOICE_LEVELS)
adj$xpos <- match(adj$VoiGrp, VOICE_LEVELS)
ref <- adj$emmean[adj$VoiGrp == "HNV"]

ylim_b <- c(
  min(adj$lower.CL, na.rm = TRUE) - 0.10,
  max(adj$upper.CL, na.rm = TRUE) + 0.10
)

p4b <- ggplot2::ggplot(
  adj,
  ggplot2::aes(
    x = xpos, y = emmean,
    colour = VoiGrp, shape = VoiGrp
  )
) +
  ggplot2::geom_hline(
    yintercept = ref,
    linetype = "dotted",
    linewidth = 0.32,
    colour = "grey45"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.08,
    linewidth = 0.42
  ) +
  ggplot2::geom_point(
    size = 1.95,
    stroke = 0.60,
    fill = "white"
  ) +
  ggplot2::scale_colour_manual(values = COL_VOICE, guide = "none") +
  ggplot2::scale_shape_manual(values = SHAPE_VOICE, guide = "none") +
  ggplot2::scale_x_continuous(
    breaks = seq_along(VOICE_LEVELS),
    labels = VOICE_LEVELS,
    expand = ggplot2::expansion(mult = c(0.08, 0.08))
  ) +
  ggplot2::scale_y_continuous(
    breaks = pretty(ylim_b, n = 5),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::coord_cartesian(ylim = ylim_b) +
  ggplot2::labs(
    x = NULL,
    y = "Adjusted post-test score (of 8)"
  )

fig4 <- (
  (p4a | p4b) +
    patchwork::plot_layout(
      widths = c(1.08, 1),
      guides = "collect"
    ) +
    patchwork::plot_annotation(tag_levels = "a")
) &
  ggplot2::theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical"
  )

save_fig(fig4, "fig4_learning", 180, 62)

## =========================================================
## Figure 5: consistent LMM-based KR and CR2 intervals
## =========================================================


dual_intervals <- function(outcome, covariates = NULL, data = d) {
  
  rhs <- paste(
    c("VoiGrp", covariates, "lesson"),
    collapse = " + "
  )
  
  f <- stats::as.formula(
    paste(
      outcome, "~", rhs,
      "+ (1|Class) + (1|ID)"
    )
  )
  
  # 1. Fit one LMM
  m <- lmerTest::lmer(
    f,
    data = data,
    REML = TRUE
  )
  
  # 2. Kenward-Roger confidence intervals
  fe <- summary(
    m,
    ddf = "Kenward-Roger"
  )$coefficients
  
  rows <- grep(
    "^VoiGrp",
    rownames(fe),
    value = TRUE
  )
  
  stopifnot(length(rows) == 4)
  
  tcrit <- stats::qt(
    0.975,
    df = fe[rows, "df"]
  )
  
  mod <- data.frame(
    term = sub("^VoiGrp", "", rows),
    est = fe[rows, "Estimate"],
    lo = fe[rows, "Estimate"] -
      tcrit * fe[rows, "Std. Error"],
    hi = fe[rows, "Estimate"] +
      tcrit * fe[rows, "Std. Error"],
    regime = "Model-based",
    row.names = NULL
  )
  
  # 3. CR2 inference on THE SAME LMM
  class_id <- lme4::getME(
    m, "flist"
  )[["Class"]]
  
  stopifnot(
    length(class_id) == nobs(m),
    length(unique(class_id)) == 10
  )
  
  V_cr2 <- clubSandwich::vcovCR(
    m,
    cluster = class_id,
    type = "CR2"
  )
  
  cr2_ci <- as.data.frame(
    clubSandwich::conf_int(
      m,
      vcov = V_cr2,
      coefs = rows,
      test = "Satterthwaite",
      level = 0.95
    )
  )
  
  # Align estimates by coefficient name
  idx <- match(rows, cr2_ci$Coef)
  
  stopifnot(!anyNA(idx))
  
  rob <- data.frame(
    term = sub("^VoiGrp", "", rows),
    est = cr2_ci$beta[idx],
    lo = cr2_ci$CI_L[idx],
    hi = cr2_ci$CI_U[idx],
    regime = "CR2",
    row.names = NULL
  )
  
  # Same LMM: point estimates must match
  stopifnot(isTRUE(all.equal(
    mod$est,
    rob$est,
    tolerance = 1e-8
  )))
  
  rbind(mod, rob)
}


## Ordinal/logistic outcomes: asymptotic model-based interval only.
logodds_intervals <- function(model) {
  cs <- coef(summary(model))
  rows <- grep("^VoiGrp", rownames(cs), value = TRUE)
  
  data.frame(
    term = sub("^VoiGrp", "", rows),
    est = cs[rows, 1],
    lo = cs[rows, 1] - 1.96 * cs[rows, 2],
    hi = cs[rows, 1] + 1.96 * cs[rows, 2],
    regime = "Model-based"
  )
}

m_sat_clmm <- ordinal::clmm(
  sat_resp ~ VoiGrp + lesson + item + (1 | Class) + (1 | ID),
  data = sat_long,
  link = "logit",
  Hess = TRUE
)

m_item <- lme4::glmer(
  correct ~ VoiGrp + lesson + pre_prop_c +
    (1 | ID) + (1 | item) + (1 | Class),
  data = dfi,
  family = stats::binomial,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

panels <- list(
  list(ttl = "Satisfaction",       dat = logodds_intervals(m_sat_clmm)),
  list(ttl = "Voice evaluation",   dat = dual_intervals("VSP_soc")),
  list(ttl = "Post-test total",    dat = dual_intervals("Post", "Pre_c")),
  list(ttl = "Post-test item",     dat = logodds_intervals(m_item)),
  list(ttl = "Extraneous load",    dat = dual_intervals("CL_extr")),
  list(ttl = "Engagement",         dat = dual_intervals("LE_total")),
  list(ttl = "HR change",          dat = dual_intervals("dHR")),
  list(ttl = "SCL change",         dat = dual_intervals("dSCL")),
  list(ttl = "aSCR change",        dat = dual_intervals("daSCR"))
)

forest_panel <- function(dat, ttl) {
  dat$term <- factor(dat$term, levels = rev(VOICE_LEVELS[-1]))
  dat$source <- SRC[as.character(dat$term)]
  dat$regime <- factor(dat$regime, levels = c("CR2", "Model-based"))
  
  ## Draw the thick CR2 interval first, then the thin model-based interval.
  dat <- dat |>
    dplyr::arrange(regime)
  
  point_dat <- dat |>
    dplyr::filter(regime == "Model-based") |>
    dplyr::select(term, source, est)
  
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = est, y = term, colour = source)
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.30,
      colour = "grey45"
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = lo, xmax = hi, linewidth = regime),
      lineend = "butt"
    ) +
    ggplot2::geom_point(
      data = point_dat,
      ggplot2::aes(x = est, y = term, colour = source),
      inherit.aes = FALSE,
      size = 1.35
    ) +
    ggplot2::scale_linewidth_manual(
      values = c(
        "CR2" = 1.20,
        "Model-based" = 0.42
      ),
      breaks = c("Model-based", "CR2"),
      labels = c(
        "Model-based 95% CI",
        "CR2 95% CI"
      ),
      name = "Interval type",
      guide = ggplot2::guide_legend(
        order = 2,
        override.aes = list(colour = "grey35")
      )
    ) +
    ggplot2::scale_colour_manual(
      values = COL_SRC,
      name = "Narration source",
      guide = ggplot2::guide_legend(
        order = 1,
        override.aes = list(linewidth = 1.1)
      )
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = ttl) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 7.6, hjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 7.5),
      axis.text.x = ggplot2::element_text(size = 7.0),
      legend.position = "bottom"
    )
}


forest_plots <- lapply(
  panels,
  function(x) forest_panel(x$dat, x$ttl)
)

for (i in seq_along(forest_plots)) {
  if (i != 2L) {
    forest_plots[[i]] <- forest_plots[[i]] +
      ggplot2::guides(
        linewidth = "none",
        colour = "none"
      )
  }
}

fig5 <- patchwork::wrap_plots(
  plotlist = forest_plots,
  ncol = 3,
  guides = "collect"
) +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


save_fig(fig5, "fig5_dual_intervals", 180, 150)



cat("\nFigures written to configured output directory.\n")
