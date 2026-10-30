## ---- 1.1 Paths -------------------------------------------------------------

# Paths are relative to the repository root by default.
# For authorized data stored elsewhere, set NPJ_DATA_DIR and NPJ_OUT_DIR.
DATA_DIR <- Sys.getenv("NPJ_DATA_DIR", unset = "data")

OUT_DIR <- Sys.getenv("NPJ_OUT_DIR", unset = "outputs")

FILE_MAIN <- file.path(DATA_DIR, "data.xlsx")
FILE_ITEM <- file.path(DATA_DIR, "data.xlsx")
FILE_PV   <- file.path(DATA_DIR, "perception.xlsx")

SHEET_MAIN <- 1
SHEET_ITEM <- NA

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("Looking for the required study files in the configured data directory.\n")
for (f in c(FILE_MAIN, FILE_ITEM, FILE_PV))
  cat(sprintf("  %-25s %s\n", basename(f), if (file.exists(f)) "found" else "NOT FOUND"))
if (!file.exists(FILE_MAIN))
  stop("Required learner workbook not found; set NPJ_DATA_DIR or use data/.",
       call. = FALSE)

## ---- 1.1b Optional logging -------------------------------------------------
## Only the output stream is redirected; messages are left on the console so
## that errors remain visible. For a complete log use the Rscript form above.
LOG_TO_FILE <- FALSE
log_con <- NULL
close_log <- function() {
  while (sink.number() > 0) sink()
  if (!is.null(log_con)) try(close(log_con), silent = TRUE)
  log_con <<- NULL
}
if (LOG_TO_FILE) {
  log_con <- file(file.path(OUT_DIR, "analysis_log.txt"), open = "wt")
  sink(log_con, split = TRUE)
  options(error = function() {
    close_log()
    cat("\n*** analysis.R stopped with an error (see message above). ***\n")
  })
}

cat("\nanalysis.R run at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

## ---- 1.2 Column names ------------------------------------------------------
## Adjust the right-hand sides if your column names differ. Everything else in
## the script refers to these constants, not to literal column names.

COL_ID      <- "ID"
COL_VOICE   <- "VoiGrp"
COL_CLASS   <- "Class"
COL_LESSON  <- "lesson"
COL_GENDER  <- "Gender"

## Satisfaction: TWO raw items on 1-5 scales. The cumulative link model needs
## the ordinal responses, not the 2-10 sum.
SAT_ITEMS   <- c("Sat1", "Sat2")

## Cognitive load: five analysed items on 1-7 scales.
## Intrinsic = CL1, CL2. Extraneous = CL3, CL4, CL5.
## Three germane items were administered but are not analysed (positively
## keyed, not summable with the other five); list them here to be excluded.
CL_INTRINSIC  <- c("CogLoad1", "CogLoad2")
CL_EXTRANEOUS <- c("CogLoad3", "CogLoad4", "CogLoad5")
CL_GERMANE    <- character(0)   # administered but not exported; not analysed

## Voice evaluation: seven items on 1-5 scales.
## Intelligibility = VSP1, VSP4 (VSP4 reverse-keyed).
## Social = VSP2, VSP3, VSP5, VSP6, VSP7.
VSP_INTELLIG  <- c("VSP1", "VSP4")
VSP_SOCIAL    <- c("VSP2", "VSP3", "VSP5", "VSP6", "VSP7")
VSP_REVERSED  <- "VSP4"
VSP_MAX       <- 5   # scale maximum, used for reverse scoring: (max + 1) - x
## Set to TRUE if VSP4 is ALREADY reverse-scored in the file. Section 1.6
## checks this against the stored VSP total and will tell you which is right.
VSP4_ALREADY_REVERSED <- TRUE

## Learning engagement: six items on 1-5 scales.
LE_ITEMS    <- paste0("LearnEng", 1:6)

## Affect: scale means, pre and post, five positive and five negative items.
COL_PA_PRE  <- "PA1"; COL_PA_POST <- "PA2"
COL_NA_PRE  <- "NA1"; COL_NA_POST <- "NA2"

## Knowledge test: totals out of 8, and the eight binary item responses.
COL_PRE     <- "Pre"; COL_POST <- "Post"
N_TEST_ITEMS <- 8
## If the post-test items live in the main sheet as wide columns, name them
## here and set SHEET_ITEM <- NA above. Otherwise they are read from a long
## sheet with columns ID / lesson / item / correct.
POST_ITEM_COLS <- paste0("Post", 1:N_TEST_ITEMS)
PRE_ITEM_COLS  <- paste0("Pre",  1:N_TEST_ITEMS)   # not modelled, checked only

## Stored summed scores. Used as cross-checks when the items are present, and
## as fallbacks when they are not (see the capability report in Section 1.4).
COL_SAT_SUM <- "Sat"        # 2-10
COL_CL_SUM  <- "CogLoad"    # 5-35
COL_VSP_SUM <- "VSP"        # 7-35
COL_LE_SUM  <- "LearnEng"   # 6-30

## Physiology (already baseline-corrected and within-participant normalised).
PHYS_COLS   <- c("dHR", "dSCL", "daSCR")

## Lesson order counterbalancing at class level. Used to derive `session`,
## which is not stored in the file. Classes 1, 2, 3, 4 and 9 received the
## physics lesson first; classes 5, 6, 7, 8 and 10 received the reverse.
PHYSICS_FIRST_CLASSES <- c(1, 2, 3, 4, 9)
## How physics and biology are coded in COL_LESSON. First element = reference.
LESSON_LEVELS <- c("physics", "biology")

## Condition levels, human neutral first because it is the model reference.
VOICE_LEVELS <- c("HNV", "HCV", "AINV", "AICV", "AIENV")

## Maps the codes stored in the file to the canonical condition labels.
## Left-hand side is the stored value; set to NULL if no mapping is needed.
VOICE_RECODE  <- c("1" = "HNV", "2" = "HCV", "3" = "AINV",
                   "4" = "AICV", "5" = "AIENV")
LESSON_RECODE <- c("0" = "physics", "1" = "biology")
GENDER_LABELS <- c("0" = "female", "1" = "male")

## Design constants used for design effects (Supplementary Table S15).
N_STUDENTS   <- 233
N_CLASSES    <- 10
STUDENTS_PER_CLASS <- N_STUDENTS / N_CLASSES   # 23.3

## Simulation settings for Section 13. Use 200 for a dry run, 1000 to
## reproduce the reported figures.
N_SIM <- 1000

## Section 7.4 fits two cumulative link mixed models on 2,330 ordinal
## responses. Expect roughly 10 to 90 s each. Set to FALSE to skip.
RUN_ORDINAL_CHECK <- TRUE

## ---- 1.3 Preflight ---------------------------------------------------------
preflight <- function(dat, required, label) {
  absent <- setdiff(required, names(dat))
  if (length(absent)) {
    stop("[", label, "] missing required columns:\n  ",
         paste(absent, collapse = ", "),
         "\nEdit the column-name constants in Section 1.2 if they are named ",
         "differently in your file.", call. = FALSE)
  }
  invisible(TRUE)
}

## Coerce a coding column to the canonical labels, trimming whitespace and
## applying the optional recode map. On failure it prints what the file
## actually contains, which is the information needed to write the map.
recode_levels <- function(x, target, map, what, map_name) {
  x <- trimws(as.character(x))
  if (!is.null(map)) {
    hit <- x %in% names(map)
    x[hit] <- unname(map[x[hit]])
  }
  bad <- setdiff(unique(x), target)
  if (length(bad)) {
    stop(what, " holds values that are not ",
         paste(target, collapse = " / "), ".\n",
         "  Found in the file: ", paste(shQuote(sort(unique(x))), collapse = ", "),
         "\n  Unmatched:         ", paste(shQuote(sort(bad)), collapse = ", "),
         "\n  Set ", map_name, " in Section 1.2, for example\n",
         "    ", map_name, " <- c(", paste0(shQuote(sort(bad)), ' = "..."',
                                            collapse = ", "), ")",
         call. = FALSE)
  }
  factor(x, levels = target)
}

## ---- 1.4 Read and capability check -----------------------------------------
d <- readxl::read_excel(FILE_MAIN, sheet = SHEET_MAIN) |> as.data.frame()

## Columns without which nothing can be estimated.
required_core <- c(COL_ID, COL_VOICE, COL_CLASS, COL_LESSON, COL_GENDER,
                   VSP_INTELLIG, VSP_SOCIAL, LE_ITEMS,
                   COL_PA_PRE, COL_PA_POST, COL_NA_PRE, COL_NA_POST,
                   COL_PRE, COL_POST, PHYS_COLS)
preflight(d, required_core, "main data")

## Three analyses need item-level responses. Availability is detected here;
## the affected sections are skipped with a warning if the items are absent.
HAVE_SAT_ITEMS <- all(SAT_ITEMS %in% names(d))
HAVE_CL_ITEMS  <- all(c(CL_INTRINSIC, CL_EXTRANEOUS) %in% names(d))
HAVE_POST_ITEMS <- (!is.na(SHEET_ITEM) &&
                      SHEET_ITEM %in% readxl::excel_sheets(FILE_ITEM)) ||
  all(POST_ITEM_COLS %in% names(d))
HAVE_PV <- file.exists(FILE_PV)

cat("\n===== 1.4 Analyses available with this data file =====\n")
cap <- function(ok, what, needs)
  cat(sprintf("  [%s] %-46s %s\n", if (ok) "x" else " ", what,
              if (ok) "" else paste("needs", needs)))
cap(HAVE_SAT_ITEMS,  "Satisfaction CLMM (pre-specified primary)",
    paste(SAT_ITEMS, collapse = ", "))
cap(HAVE_SAT_ITEMS,  "Repeated-exposure CLMM (Table S14)",
    paste(SAT_ITEMS, collapse = ", "))
cap(HAVE_POST_ITEMS, "Item-level learning GLMM (Table S8, left)",
    "an 'items' sheet or Post_i1..Post_i8")
cap(HAVE_CL_ITEMS,   "Cognitive load, intrinsic and extraneous",
    paste(c(CL_INTRINSIC, CL_EXTRANEOUS), collapse = ", "))
cap(RUN_ORDINAL_CHECK, "Voice-evaluation ordinal check (Table S10)",
    "RUN_ORDINAL_CHECK <- TRUE")
cap(HAVE_PV,         "Perceptual validation (Table S4, Fig. 2)",
    basename(FILE_PV))
cat("  [x] Everything else runs on the columns present.\n")
if (!HAVE_SAT_ITEMS)
  cat("\nNOTE: the summed 2-10 satisfaction score cannot substitute for the\n",
      "      ordinal model. Section 6.2 will still run on the sum, but the\n",
      "      value quoted in the abstract comes from the CLMM.\n", sep = "")

## ---- 1.5 Derived variables -------------------------------------------------
d <- d |>
  dplyr::rename(ID = !!COL_ID, VoiGrp = !!COL_VOICE, Class = !!COL_CLASS,
                lesson = !!COL_LESSON, Gender = !!COL_GENDER,
                Pre = !!COL_PRE, Post = !!COL_POST) |>
  dplyr::mutate(
    ID     = factor(ID),
    Class  = factor(trimws(as.character(Class))),
    Gender = factor(GENDER_LABELS[trimws(as.character(Gender))],
                    levels = unname(GENDER_LABELS))
  )

d$VoiGrp <- recode_levels(d$VoiGrp, VOICE_LEVELS, VOICE_RECODE,
                          "Voice condition", "VOICE_RECODE")
d$lesson <- recode_levels(d$lesson, LESSON_LEVELS, LESSON_RECODE,
                          "Lesson", "LESSON_RECODE")
cat("\nVoice condition coding accepted:\n"); print(table(d$VoiGrp))
cat("Lesson coding accepted:\n");            print(table(d$lesson))

## Session: which of the two weekly sessions this row is. Derived from the
## class-level counterbalancing, so session is orthogonal to voice condition
## by construction. Verified in Section 1.7.
class_num <- suppressWarnings(as.numeric(as.character(d$Class)))
if (anyNA(class_num))
  stop("Class labels are not numeric, so `session` cannot be derived from ",
       "PHYSICS_FIRST_CLASSES. Either recode Class or add a `session` column.")
d$session <- factor(ifelse(
  (class_num %in% PHYSICS_FIRST_CLASSES) == (d$lesson == LESSON_LEVELS[1]),
  1L, 2L))

## Reverse-score VSP4 unless the file already carries it reversed.
vsp4_scored <- if (VSP4_ALREADY_REVERSED) d[[VSP_REVERSED]] else (VSP_MAX + 1) - d[[VSP_REVERSED]]

## Scale scores. Recomputed from items wherever the items are present;
## otherwise the stored total is used and the subscale split is unavailable.
d$Sat_sum <- if (HAVE_SAT_ITEMS) rowSums(d[, SAT_ITEMS]) else d[[COL_SAT_SUM]]
if (HAVE_CL_ITEMS) {
  d$CL_intr  <- rowSums(d[, CL_INTRINSIC])                   # 2-14
  d$CL_extr  <- rowSums(d[, CL_EXTRANEOUS])                  # 3-21
  d$CL_total <- d$CL_intr + d$CL_extr                        # 5-35
} else {
  d$CL_total <- d[[COL_CL_SUM]]
}
d$VSP_soc   <- rowSums(d[, VSP_SOCIAL])                      # 5-25
d$VSP_int   <- d[[VSP_INTELLIG[1]]] + vsp4_scored            # 2-10
d$VSP_total <- d$VSP_soc + d$VSP_int                         # 7-35
d$LE_total  <- rowSums(d[, LE_ITEMS])                        # 6-30
d$PA_chg    <- d[[COL_PA_POST]] - d[[COL_PA_PRE]]            # post minus pre
d$NA_chg    <- d[[COL_NA_POST]] - d[[COL_NA_PRE]]

## Prior knowledge: centred total for the total-score model, proportion
## correct (centred) for the item-level model.
d$Pre_c        <- as.numeric(scale(d$Pre, scale = FALSE))
d$pre_prop_c   <- as.numeric(scale(d$Pre / N_TEST_ITEMS, scale = FALSE))
d$Gain         <- d$Post - d$Pre

## ---- 1.6 Recomputed scores vs stored totals --------------------------------
## Stored totals are used only as a cross-check; all models use the
## recomputed scores. Any mismatch is printed rather than silently repaired.
cat("\n===== 1.6 Recomputed scale scores vs stored totals =====\n")
compare_stored <- function(recomputed, stored_name, label) {
  if (!stored_name %in% names(d)) {
    cat(sprintf("  %-22s stored column '%s' absent, skipped\n", label, stored_name))
    return(invisible(NULL))
  }
  diff_idx <- which(abs(recomputed - d[[stored_name]]) > 1e-8 &
                      !is.na(recomputed) & !is.na(d[[stored_name]]))
  cat(sprintf("  %-22s %d mismatching rows\n", label, length(diff_idx)))
  if (length(diff_idx)) {
    print(data.frame(row = diff_idx,
                     ID = d$ID[diff_idx], lesson = d$lesson[diff_idx],
                     recomputed = recomputed[diff_idx],
                     stored = d[[stored_name]][diff_idx]))
  }
  invisible(diff_idx)
}
if (HAVE_SAT_ITEMS) compare_stored(d$Sat_sum,  COL_SAT_SUM, "Satisfaction (2-10)")
if (HAVE_CL_ITEMS)  compare_stored(d$CL_total, COL_CL_SUM,  "Cognitive load (5-35)")
compare_stored(d$VSP_total, COL_VSP_SUM, "Voice evaluation (7-35)")
compare_stored(d$LE_total,  COL_LE_SUM,  "Engagement (6-30)")
compare_stored(d$Gain,      "Gain",     "Gain (Post - Pre)")
compare_stored(d$PA_chg,    "PAchg",    "Positive affect change")
compare_stored(d$NA_chg,    "NAchg",    "Negative affect change")

## Reverse-keying diagnostic for VSP4: whichever direction reproduces the
## stored seven-item total is the one the file uses.
if ("VSP" %in% names(d)) {
  fwd <- d$VSP_soc + d[[VSP_INTELLIG[1]]] + d[[VSP_REVERSED]]
  rev <- d$VSP_soc + d[[VSP_INTELLIG[1]]] + ((VSP_MAX + 1) - d[[VSP_REVERSED]])
  cat(sprintf("\n  VSP4 diagnostic: raw matches stored on %d rows, ",
              sum(abs(fwd - d$VSP) < 1e-8, na.rm = TRUE)))
  cat(sprintf("reversed matches on %d rows (n = %d)\n",
              sum(abs(rev - d$VSP) < 1e-8, na.rm = TRUE), nrow(d)))
  cat("  -> set VSP4_ALREADY_REVERSED in Section 1.2 accordingly.\n")
}

## ---- 1.7 Integrity assertions ----------------------------------------------
cat("\n===== 1.7 Integrity checks =====\n")
stopifnot(
  nrow(d) == 2 * N_STUDENTS,                       # 466 observations
  dplyr::n_distinct(d$ID) == N_STUDENTS,           # 233 students
  dplyr::n_distinct(d$Class) == N_CLASSES,         # 10 classes
  all(table(d$ID) == 2),                           # two lessons per student
  all(d$Pre  >= 0 & d$Pre  <= N_TEST_ITEMS),
  all(d$Post >= 0 & d$Post <= N_TEST_ITEMS)
)
## Each class has exactly one voice condition; each condition has two classes.
stopifnot(all(rowSums(table(d$Class, d$VoiGrp) > 0) == 1),
          all(colSums(table(d$Class, d$VoiGrp) > 0) == 2))
## Session is orthogonal to voice condition: every condition contributes
## equally to both sessions.
cat("\nVoice condition x session (should be balanced):\n")
print(table(d$VoiGrp, d$session))
cat("\nStudents per condition:\n")
print(tapply(d$ID, d$VoiGrp, dplyr::n_distinct))
cat("\nAll integrity checks passed.\n")

## Export raw lesson-specific knowledge-test summaries so that any physics
## grading revision can be traced separately from biology and from pooled means.
grade_audit <- d |>
  dplyr::group_by(lesson, VoiGrp) |>
  dplyr::summarise(n = dplyr::n(), pre_mean = mean(Pre),
                   post_mean = mean(Post), gain_mean = mean(Gain),
                   pre_total = sum(Pre), post_total = sum(Post),
                   .groups = "drop")
write.csv(grade_audit, file.path(OUT_DIR, "learning_grade_audit_by_lesson.csv"), row.names = FALSE)
cat("\n[Raw learning scores by lesson and voice, including physics separately]\n")
print(grade_audit)


## ---- 1.7b Physiological data quality ---------------------------------------
## Reactivity scores are differences from each participant's own resting
## baseline. Heart rate is in bpm; the electrodermal measures inherit the
## units of the recording. Extreme values are reported rather than removed;
## Section 10b refits the electrodermal models without them as a sensitivity
## check, because a handful of large values can carry a null result.
cat("\n===== 1.7b Physiological reactivity, distribution =====\n")
phys_summary <- t(sapply(PHYS_COLS, function(v) {
  x <- d[[v]]; q <- quantile(x, c(.25, .75), na.rm = TRUE); iqr <- diff(q)
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE),
    n_beyond_3IQR = sum(x < q[1] - 3 * iqr | x > q[2] + 3 * iqr, na.rm = TRUE))
}))
print(round(phys_summary, 3))
## Physiological plausibility for heart rate: a change of more than 30 bpm
## from a seated resting baseline during passive video viewing is more likely
## a recording artifact than a response to narration.
HR_IMPLAUSIBLE <- 30
n_hr_bad <- sum(abs(d$dHR) > HR_IMPLAUSIBLE, na.rm = TRUE)
cat(sprintf("\n  |dHR| > %d bpm: %d of %d observations\n",
            HR_IMPLAUSIBLE, n_hr_bad, nrow(d)))
## Flag participants who carry an extreme electrodermal value in either
## session; Section 10b excludes them.
EDA_CUTOFF <- 5
eda_flag <- abs(d$dSCL) > EDA_CUTOFF | abs(d$daSCR) > EDA_CUTOFF
EDA_OUTLIER_IDS <- unique(as.character(d$ID[eda_flag]))
cat(sprintf("  |dSCL| or |daSCR| > %d: %d observations from %d participants\n",
            EDA_CUTOFF, sum(eda_flag, na.rm = TRUE), length(EDA_OUTLIER_IDS)))
if (length(EDA_OUTLIER_IDS))
  print(d[eda_flag, c("ID", "Class", "VoiGrp", "lesson", "dSCL", "daSCR")],
        row.names = FALSE, digits = 3)

## ---- 1.8 Long-form frames --------------------------------------------------
## (a) Satisfaction, item level: 932 ordinal responses.
sat_long <- NULL
if (HAVE_SAT_ITEMS) {
  sat_long <- d |>
    dplyr::select(ID, Class, VoiGrp, lesson, session, dplyr::all_of(SAT_ITEMS)) |>
    tidyr::pivot_longer(dplyr::all_of(SAT_ITEMS),
                        names_to = "item", values_to = "sat_resp") |>
    dplyr::mutate(item = factor(item, levels = SAT_ITEMS),
                  sat_resp = ordered(sat_resp, levels = 1:5)) |>
    as.data.frame()
  stopifnot(nrow(sat_long) == 4 * N_STUDENTS)   # 932
}

## (b) Knowledge post-test, item level: 3,728 binary responses.
build_item_frame <- function() {
  if (!is.na(SHEET_ITEM) && SHEET_ITEM %in% readxl::excel_sheets(FILE_ITEM)) {
    di <- readxl::read_excel(FILE_ITEM, sheet = SHEET_ITEM) |> as.data.frame()
    preflight(di, c(COL_ID, COL_LESSON, "item", "correct"), "item-level data")
    di <- dplyr::rename(di, ID = !!COL_ID, lesson = !!COL_LESSON)
  } else {
    preflight(d, POST_ITEM_COLS, "post-test item columns in main sheet")
    di <- d |>
      dplyr::select(ID, lesson, dplyr::all_of(POST_ITEM_COLS)) |>
      tidyr::pivot_longer(dplyr::all_of(POST_ITEM_COLS),
                          names_to = "item", values_to = "correct") |>
      as.data.frame()
  }
  ## Items are nested in lesson: item 1 of physics is not item 1 of biology.
  di$item   <- factor(paste(di$lesson, di$item, sep = "_"))
  di$ID     <- as.character(di$ID)
  di$lesson <- as.character(di$lesson)
  key <- dplyr::select(d, ID, lesson, Class, VoiGrp, pre_prop_c)
  key$ID <- as.character(key$ID); key$lesson <- as.character(key$lesson)
  out <- dplyr::left_join(di, key, by = c("ID", "lesson"))
  out$ID     <- factor(out$ID)
  out$lesson <- factor(out$lesson, levels = LESSON_LEVELS)
  if (anyNA(out$VoiGrp)) stop("Some item-level rows did not match the main data.")
  out
}
dfi <- NULL
if (HAVE_POST_ITEMS) {
  dfi <- build_item_frame()
  stopifnot(nrow(dfi) == 2 * N_STUDENTS * N_TEST_ITEMS,   # 3,728
            all(dfi$correct %in% c(0, 1)),
            dplyr::n_distinct(dfi$item) == 2 * N_TEST_ITEMS)
  ## The item-level responses must sum to the stored post-test totals.
  tot_check <- dfi |>
    dplyr::group_by(ID, lesson) |>
    dplyr::summarise(recomputed = sum(correct), .groups = "drop") |>
    dplyr::inner_join(dplyr::select(d, ID, lesson, Post), by = c("ID", "lesson"))
  cat(sprintf("\nItem-level post-test totals disagree with stored Post on %d of %d rows.\n",
              sum(tot_check$recomputed != tot_check$Post), nrow(tot_check)))
  if (any(tot_check$recomputed != tot_check$Post))
    stop("Post scores differ from the sum of eight binary items. Check the revised physics scoring.")
  ## Grade revisions may change individual physics items without changing
  ## total scores; retain an item-level audit to examine the score key.
  item_audit <- dfi |>
    dplyr::group_by(lesson, item) |>
    dplyr::summarise(n = dplyr::n(), n_correct = sum(correct),
                     proportion_correct = mean(correct), .groups = "drop")
  write.csv(item_audit, file.path(OUT_DIR, "posttest_item_audit_by_lesson.csv"), row.names = FALSE)
  
}

## (c) Generic item-level long frame for the rating scales. Written generically
## so that the same ordinal check can later be run on the intelligibility pair
## or the three extraneous-load items without new code.
build_item_long <- function(items, scale_max, data = d) {
  out <- data |>
    dplyr::select(ID, Class, VoiGrp, lesson, session, dplyr::all_of(items)) |>
    tidyr::pivot_longer(dplyr::all_of(items),
                        names_to = "item", values_to = "resp") |>
    as.data.frame()
  bad <- !(out$resp %in% seq_len(scale_max)) & !is.na(out$resp)
  if (any(bad))
    stop("Responses outside 1-", scale_max, " in ",
         paste(items, collapse = ", "), ": ",
         paste(sort(unique(out$resp[bad])), collapse = ", "), call. = FALSE)
  out$item <- factor(out$item, levels = items)
  out$resp <- ordered(out$resp, levels = seq_len(scale_max))
  out
}

## Voice evaluation, social subscale: 5 items x 466 observations = 2,330.
## None of the five is reverse-keyed (VSP4 is, and it sits in the
## intelligibility pair), so the raw responses are used as recorded.
vsp_soc_long <- build_item_long(VSP_SOCIAL, scale_max = 5)
stopifnot(nrow(vsp_soc_long) == length(VSP_SOCIAL) * 2 * N_STUDENTS)
cat(sprintf("\nVoice evaluation, item-level frame: %d responses, %d items\n",
            nrow(vsp_soc_long), dplyr::n_distinct(vsp_soc_long$item)))


## =============================================================================
## 2. RELIABILITY (reported in Methods)
## =============================================================================

cat("\n\n===== 2. Cronbach's alpha by lesson =====\n")
alpha_by_lesson <- function(items, label) {
  for (ls in levels(d$lesson)) {
    sub <- d[d$lesson == ls, items, drop = FALSE]
    a <- suppressWarnings(psych::alpha(sub, warnings = FALSE, check.keys = FALSE))
    cat(sprintf("  %-34s %-10s alpha = %.2f (k = %d)\n",
                label, ls, a$total$raw_alpha, length(items)))
  }
}
if (HAVE_SAT_ITEMS) alpha_by_lesson(SAT_ITEMS, "Satisfaction (2 items)")
if (HAVE_CL_ITEMS) {
  alpha_by_lesson(c(CL_INTRINSIC, CL_EXTRANEOUS), "Cognitive load, total (5)")
  alpha_by_lesson(CL_INTRINSIC,                   "Cognitive load, intrinsic (2)")
  alpha_by_lesson(CL_EXTRANEOUS,                  "Cognitive load, extraneous (3)")
}
alpha_by_lesson(c(VSP_INTELLIG, VSP_SOCIAL),  "Voice evaluation, total (7)")
alpha_by_lesson(VSP_SOCIAL,                   "Voice evaluation, social (5)")
alpha_by_lesson(LE_ITEMS,                     "Learning engagement (6)")

## Item-total correlations justify splitting the voice-evaluation scale:
## about 0.43 and 0.14 for the two intelligibility items against 0.66 to 0.78
## for the five social items.
cat("\nVoice evaluation, corrected item-total correlations (both lessons pooled):\n")
vsp_all <- d[, c(VSP_INTELLIG, VSP_SOCIAL)]
vsp_all[[VSP_REVERSED]] <- vsp4_scored
print(round(suppressWarnings(
  psych::alpha(vsp_all, warnings = FALSE, check.keys = FALSE))$item.stats[, "r.drop", drop = FALSE], 2))


## =============================================================================
## 3. DESCRIPTIVE STATISTICS (Supplementary Table S6)
## =============================================================================

cat("\n\n===== 3. Means by voice condition, pooled across lessons (Table S6) =====\n")
desc_vars <- c(Satisfaction = "Sat_sum", VoiceEval_total = "VSP_total",
               VoiceEval_social = "VSP_soc", VoiceEval_intel = "VSP_int",
               CogLoad_total = "CL_total",
               if (HAVE_CL_ITEMS) c(CogLoad_intrinsic = "CL_intr",
                                    CogLoad_extraneous = "CL_extr"),
               Engagement = "LE_total",
               PreTest = "Pre", PostTest = "Post",
               dHR = "dHR", dSCL = "dSCL", daSCR = "daSCR")
tab_s6 <- sapply(desc_vars, function(v) tapply(d[[v]], d$VoiGrp, mean, na.rm = TRUE))
print(round(t(tab_s6), 2))
write.csv(round(t(tab_s6), 3), file.path(OUT_DIR, "table_s6_descriptives.csv"))


## =============================================================================
## 4. RANDOMISATION CHECKS
## =============================================================================

cat("\n\n===== 4. Baseline balance across conditions =====\n")
## Continuous baselines: omnibus test of the five-level voice factor in a
## mixed model that respects the class structure.
balance_test <- function(var, label) {
  f <- as.formula(paste(var, "~ VoiGrp + lesson + (1|Class) + (1|ID)"))
  m <- lmerTest::lmer(f, data = d)
  a <- anova(m, ddf = "Kenward-Roger")
  cat(sprintf("  %-28s F(%.0f, %.1f) = %.3f, p = %.3f\n", label,
              a["VoiGrp", "NumDF"], a["VoiGrp", "DenDF"],
              a["VoiGrp", "F value"], a["VoiGrp", "Pr(>F)"]))
}
balance_test("Pre",        "Prior knowledge")
balance_test(COL_PA_PRE,   "Pre-lesson positive affect")
balance_test(COL_NA_PRE,   "Pre-lesson negative affect")

## Gender at the student level, one row per student.
stu <- d[!duplicated(d$ID), c("ID", "VoiGrp", "Gender")]
gender_tab <- table(stu$VoiGrp, stu$Gender)
cat("\n  Gender composition by condition:\n"); print(gender_tab)
gx <- chisq.test(gender_tab)
cat(sprintf("  chi-square(%d) = %.3f, p = %.3f\n", gx$parameter, gx$statistic, gx$p.value))


## =============================================================================
## 5. PLANNED CONTRASTS AND THE THREE-REGIME HELPER
## =============================================================================

## Six planned contrasts, Holm-adjusted within outcome.
## Weights follow VOICE_LEVELS = HNV, HCV, AINV, AICV, AIENV.
six <- list(
  "AINV - HNV"   = c(-1,  0,  1,  0,  0),
  "AICV - HCV"   = c( 0, -1,  0,  1,  0),
  "HCV - HNV"    = c(-1,  1,  0,  0,  0),
  "AICV - AINV"  = c( 0,  0, -1,  1,  0),
  "AIENV - AINV" = c( 0,  0, -1,  0,  1),
  "AIENV - AICV" = c( 0,  0,  0, -1,  1)
)

## Build the same six contrasts as linear combinations of model coefficients.
## Used where emmeans is not the cleanest route (the cumulative link models).
build_L <- function(coef_names, prefix = "VoiGrp") {
  dv <- matrix(0, nrow = length(VOICE_LEVELS), ncol = length(coef_names),
               dimnames = list(VOICE_LEVELS, coef_names))
  for (lv in VOICE_LEVELS[-1]) {
    nm <- paste0(prefix, lv)
    if (!nm %in% coef_names) stop("coefficient not found: ", nm)
    dv[lv, nm] <- 1
  }
  L <- t(sapply(six, function(w) as.vector(w %*% dv)))
  colnames(L) <- coef_names
  L
}

## Wald tests for a set of linear combinations, with Holm adjustment.
wald_contrasts <- function(b, V, L, df = Inf) {
  est  <- as.vector(L %*% b)
  se   <- sqrt(diag(L %*% V %*% t(L)))
  stat <- est / se
  p    <- if (is.finite(df)) 2 * pt(-abs(stat), df) else 2 * pnorm(-abs(stat))
  data.frame(contrast = rownames(L), estimate = est, SE = se,
             statistic = stat, p = p, p_holm = p.adjust(p, "holm"),
             row.names = NULL)
}

## Six planned contrasts from a fitted cumulative link mixed model.
## clmm keeps the regression coefficients in $beta while vcov() also returns
## the threshold parameters, so the covariance matrix is subset by name first.
clmm_six <- function(m) {
  b <- m$beta
  V <- vcov(m)
  stopifnot(all(names(b) %in% rownames(V)))
  wald_contrasts(b, V[names(b), names(b), drop = FALSE],
                 build_L(names(b)), df = Inf)
}

## Fit a clmm, reporting failure rather than halting the script. A model that
## does not converge is a result to report, not a reason to lose the rest of
## the run.
fit_clmm_safe <- function(f, data, label) {
  t0 <- Sys.time()
  m <- tryCatch(ordinal::clmm(f, data = data, link = "logit", Hess = TRUE),
                error = function(e) {
                  cat("  ", label, ": FAILED -- ", conditionMessage(e),
                      "\n", sep = "")
                  NULL })
  if (!is.null(m))
    cat(sprintf("  %-24s converged in %5.1f s\n", paste0(label, ":"),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  m
}

## Intraclass correlation and design effect for a fitted lmer model.
icc_design <- function(m) {
  vc  <- as.data.frame(lme4::VarCorr(m))
  tot <- sum(vc$vcov)
  cls <- sum(vc$vcov[vc$grp == "Class"])
  icc <- cls / tot
  de  <- 1 + (STUDENTS_PER_CLASS - 1) * icc
  c(class_var = cls, total_var = tot, ICC = icc,
    design_effect = de, effective_N = N_STUDENTS / de)
}

## Reports every continuous outcome under three regimes, in this order:
##   (1) Kenward-Roger degrees of freedom on the mixed model,
##   (2) the six planned contrasts, Holm-adjusted, under Kenward-Roger,
##   (3) a separate OLS--CR2 sensitivity fit clustered by class (Table S9b).
## The six CR2 contrasts from the SAME LMM are exported separately in Section 7.3 (S9a).
three_regimes <- function(outcome, label, data = d, covariates = NULL,
                          store = TRUE) {
  cat("\n-------------------------------------------------------------------\n")
  cat("OUTCOME: ", label, "   (", outcome, ")\n", sep = "")
  cat("-------------------------------------------------------------------\n")
  
  rhs <- paste(c("VoiGrp", covariates, "lesson"), collapse = " + ")
  f <- as.formula(paste(outcome, "~", rhs, "+ (1|Class) + (1|ID)"))
  cat("Model: ", deparse(f), "\n", sep = "")
  m <- lmerTest::lmer(f, data = data)
  if (lme4::isSingular(m))
    cat("NOTE: singular fit, a variance component is estimated at the boundary.\n",
        "      Reported as such and not removed; the CR2 analysis below\n",
        "      provides the corresponding class-level inference.\n", sep = "")
  
  cat("\n[1] Fixed effects, Kenward-Roger df\n")
  print(round(summary(m, ddf = "Kenward-Roger")$coefficients, 3))
  
  cat("\n[2] Six planned contrasts, Holm-adjusted, Kenward-Roger df\n")
  emm <- emmeans::emmeans(m, ~ VoiGrp)
  ct  <- emmeans::contrast(emm, method = six, adjust = "holm")
  print(as.data.frame(summary(ct)), digits = 3)
  
  cat("\n[3] Independent OLS--CR2 sensitivity, class-clustered (Table S9b)\n")
  f_ols <- as.formula(paste(outcome, "~", rhs))
  mr <- estimatr::lm_robust(f_ols, data = data, clusters = Class, se_type = "CR2")
  print(round(cbind(Estimate = mr$coefficients, SE = mr$std.error,
                    t = mr$statistic, p = mr$p.value, df = mr$df), 3))
  
  cat("\n[4] Variance components, ICC and design effect\n")
  print(round(icc_design(m), 4))
  
  if (store) {
    fe <- summary(m, ddf = "Kenward-Roger")$coefficients
    rows <- grep("^VoiGrp", rownames(fe), value = TRUE)
    ESTIMATES <<- rbind(ESTIMATES, data.frame(
      outcome  = label,
      term     = sub("^VoiGrp", "", rows),
      estimate = fe[rows, "Estimate"],
      se       = fe[rows, "Std. Error"],
      df       = fe[rows, "df"],
      p        = fe[rows, "Pr(>|t|)"],
      p_ols_cr2 = mr$p.value[match(rows, names(mr$coefficients))],
      ci_low   = fe[rows, "Estimate"] - stats::qt(0.975, fe[rows, "df"]) * fe[rows, "Std. Error"],
      ci_high  = fe[rows, "Estimate"] + stats::qt(0.975, fe[rows, "df"]) * fe[rows, "Std. Error"],
      row.names = NULL))
  }
  invisible(list(lmm = m, contrasts = ct, robust = mr, icc = icc_design(m)))
}

## Accumulator for Section 15.
ESTIMATES <- NULL


## =============================================================================
## 6. SATISFACTION (Table 1; Supplementary Tables S7, S8, S9)
## =============================================================================

cat("\n\n===== 6. Satisfaction =====\n")

## ---- 6.1 Pre-specified primary analysis: cumulative link mixed model -------
## Pre-specified primary analysis. Two items are too few to support an item
## random effect, so item enters as a fixed factor.
## Expected: logLik = -1124.05, AIC = 2272.10 on 932 responses.
m_sat_clmm <- NULL
if (!HAVE_SAT_ITEMS) {
  cat("\nSKIPPED: the pre-specified ordinal analysis needs the two raw 1-5\n",
      "satisfaction items (", paste(SAT_ITEMS, collapse = ", "), ").\n",
      "Section 6.2 below still runs on the summed score.\n", sep = "")
} else {
  m_sat_clmm <- ordinal::clmm(
    sat_resp ~ VoiGrp + lesson + item + (1 | Class) + (1 | ID),
    data = sat_long, link = "logit", Hess = TRUE)
  cat("\n[Table S7] Cumulative link mixed model\n")
  print(summary(m_sat_clmm))
  cat(sprintf("\nN = %d responses, logLik = %.2f, AIC = %.2f\n",
              nrow(sat_long), as.numeric(logLik(m_sat_clmm)), AIC(m_sat_clmm)))
  
  ## Six planned contrasts on the log-odds scale, asymptotic inference. This is
  ## the regime the ordinal implementation offers; it does not adjust for the
  ## number of clusters, which is why the other two regimes follow.
  b_sat <- m_sat_clmm$beta
  V_all <- vcov(m_sat_clmm)
  stopifnot(all(names(b_sat) %in% rownames(V_all)))
  V_sat <- V_all[names(b_sat), names(b_sat), drop = FALSE]
  L_sat <- build_L(names(b_sat))
  sat_ct_clmm <- wald_contrasts(b_sat, V_sat, L_sat, df = Inf)
  cat("\n[Table 1, satisfaction] Six planned contrasts, CLMM, Holm-adjusted\n")
  print(sat_ct_clmm, digits = 3)
  
  ## Cross-check against emmeans. Latent mode puts the contrasts on the same
  ## scale and in the same direction as the model coefficients, so the two
  ## routes must agree to numerical precision.
  cat("\nCross-check with emmeans (latent scale):\n")
  print(tryCatch(
    summary(emmeans::contrast(
      emmeans::emmeans(m_sat_clmm, ~ VoiGrp, mode = "latent"),
      method = six, adjust = "holm")),
    error = function(e) paste("emmeans cross-check unavailable:",
                              conditionMessage(e))), digits = 3)
}   # end if (HAVE_SAT_ITEMS)

## ---- 6.2 Summed score on the linear scale, Kenward-Roger -------------------
## Places all six contrasts on one interpretable scale and supplies the
## small-sample regime. Expected Holm-adjusted values 0.215 to 0.221 for the
## three contrasts that isolate the AI calm voice, and CR2 p = 0.087 for the
## AI calm coefficient.
res_sat_lmm <- three_regimes("Sat_sum", "Satisfaction, summed score")


## =============================================================================
## 7. VOICE EVALUATION (Table 1; Supplementary Tables S8, S9, S10)
## =============================================================================

cat("\n\n===== 7. Voice evaluation =====\n")
## The two intelligibility items and the five social items behave differently
## (Section 2) and are reported separately. The social subscale is primary.
res_vsp_soc <- three_regimes("VSP_soc",   "Voice evaluation, social (5 items)")
res_vsp_int <- three_regimes("VSP_int",   "Voice evaluation, intelligibility (2 items)")
res_vsp_tot <- three_regimes("VSP_total", "Voice evaluation, total (7 items)", store = FALSE)


## ---- 7.3 Exact CR2 planned contrasts on the SAME fitted LMMs (Table S9a) ----
## This is different from the independent OLS--CR2 fits in three_regimes()
## (Table S9b). The six comparisons and their order are taken from `six`.
## clubSandwich uses the class as the clustering unit and Satterthwaite df.
cat("\n\n===== 7.3 Same-LMM CR2 planned contrasts (Table S9a) =====\n")
if (!requireNamespace("clubSandwich", quietly = TRUE))
  stop("Install required package: clubSandwich")

extract_s9a_lmm_cr2 <- function(m, outcome) {
  b_names <- names(lme4::fixef(m))
  required <- paste0("VoiGrp", VOICE_LEVELS[-1])
  stopifnot(all(required %in% b_names))
  L <- build_L(b_names)  # Reuses the SIX original pre-specified comparisons.
  stopifnot(identical(rownames(L), names(six)))
  classes <- lme4::getME(m, "flist")[["Class"]]
  stopifnot(length(classes) == stats::nobs(m),
            length(unique(classes)) == N_CLASSES)
  V_cr2 <- clubSandwich::vcovCR(m, cluster = classes, type = "CR2")
  tab <- as.data.frame(clubSandwich::linear_contrast(
    m, vcov = V_cr2, contrasts = L, test = "Satterthwaite",
    level = 0.95, p_values = TRUE, adjustment_method = "none"
  ))
  cols <- c("Coef", "Est", "SE", "df", "CI_L", "CI_U", "p_val")
  if (!all(cols %in% names(tab))) {
    stop("clubSandwich::linear_contrast output changed: found ",
         paste(names(tab), collapse = ", "))
  }
  tab <- tab[match(names(six), as.character(tab$Coef)), , drop = FALSE]
  stopifnot(nrow(tab) == length(six), !anyNA(tab$Coef),
            all(is.finite(tab$df)), all(tab$df > 0))
  data.frame(
    Outcome = outcome,
    Contrast = as.character(tab$Coef),
    Estimate = tab$Est,
    CR2_SE = tab$SE,
    Satterthwaite_df = tab$df,
    CI_L = tab$CI_L,
    CI_U = tab$CI_U,
    p_CR2_raw = tab$p_val,
    p_CR2_Holm = stats::p.adjust(tab$p_val, method = "holm"),
    row.names = NULL
  )
}

## Reuse the SAME LMM objects fitted in Sections 6.2 and 7.1.
## No additional refit, outcome recoding, or contrast redefinition.
s9a_exact <- rbind(
  extract_s9a_lmm_cr2(res_sat_lmm$lmm, "Sat_sum"),
  extract_s9a_lmm_cr2(res_vsp_soc$lmm, "VSP_soc")
)
print(s9a_exact, row.names = FALSE, digits = 6)

## Cross-check published values. Mismatches flag an audit; do not force them.
s9a_check <- function(outcome, contrast, target, tolerance = 0.002) {
  got <- s9a_exact$p_CR2_Holm[
    s9a_exact$Outcome == outcome & s9a_exact$Contrast == contrast]
  if (length(got) != 1L || !is.finite(got) || abs(got - target) > tolerance)
    warning(sprintf("Table S9a mismatch: %s / %s: got %s, expected %.3f",
                    outcome, contrast, paste(got, collapse = ","), target))
}
s9a_check("Sat_sum", "AICV - HCV", 0.515)
s9a_check("VSP_soc", "AICV - HCV", 0.261)
s9a_file <- file.path(OUT_DIR, "table_s9a_exact_lmm_cr2.csv")
utils::write.csv(s9a_exact, s9a_file, row.names = FALSE)
cat("Saved Table S9a: ", basename(s9a_file), "\n", sep = "")

## ---- 7.4 Ordinal sensitivity analysis on the five social items -------------
## Item-level refit of the subscale analysed above as a 5-25 sum, as a check
## on the link function. Two parameterisations are fitted, item random and
## item fixed, because a variance component estimated from five levels is
## itself imprecise; they should return the same voice coefficients.
##
## clmm offers asymptotic inference only, so these p values carry no
## small-sample correction and will look stronger than the Kenward-Roger and
## CR2 values. The small-sample regimes remain the basis for the conclusions.

m_vsp_ord <- NULL; m_vsp_ord_fx <- NULL; vsp_ct_ord <- NULL
if (!RUN_ORDINAL_CHECK) {
  cat("\n----- 7.4 Ordinal sensitivity analysis SKIPPED",
      " (RUN_ORDINAL_CHECK is FALSE) -----\n", sep = "")
} else {
  cat("\n\n----- 7.4 Ordinal sensitivity analysis, five social items -----\n")
  cat(sprintf("N = %d item responses from %d students in %d classes\n",
              nrow(vsp_soc_long), dplyr::n_distinct(vsp_soc_long$ID),
              dplyr::n_distinct(vsp_soc_long$Class)))
  
  ## (i) Participants, classes and items all random.
  m_vsp_ord <- fit_clmm_safe(
    resp ~ VoiGrp + lesson + (1 | Class) + (1 | ID) + (1 | item),
    vsp_soc_long, "item random")
  
  ## (ii) Item as a fixed factor, everything else unchanged.
  m_vsp_ord_fx <- fit_clmm_safe(
    resp ~ VoiGrp + lesson + item + (1 | Class) + (1 | ID),
    vsp_soc_long, "item fixed")
  
  if (!is.null(m_vsp_ord)) {
    cat("\n[Table S10a] Cumulative link mixed model, item random\n")
    print(summary(m_vsp_ord))
    
    ## Random-effect standard deviations on the log-odds scale. Class variance
    ## at the boundary is expected and matches the satisfaction model; item
    ## variance at the boundary would mean five levels were too few, in which
    ## case report model (ii) as the primary version of this check.
    vc <- tryCatch(VarCorr(m_vsp_ord), error = function(e) NULL)
    if (!is.null(vc)) {
      sds <- vapply(vc, function(x) sqrt(as.numeric(x)[1]), numeric(1))
      cat("\nRandom-effect SDs (log-odds): ",
          paste(sprintf("%s = %.3f", names(sds), sds), collapse = ", "), "\n",
          sep = "")
      if (any(sds < 1e-4))
        cat("NOTE: a variance component is at the boundary; see model (ii).\n")
    }
    
    vsp_ct_ord <- clmm_six(m_vsp_ord)
    cat("\n[Table S10b] Six planned contrasts, CLMM, Holm-adjusted\n")
    print(vsp_ct_ord, digits = 3)
    
    ## Cross-check by a second route, as Section 6.1 does for satisfaction.
    cat("\nCross-check with emmeans (latent scale):\n")
    print(tryCatch(
      summary(emmeans::contrast(
        emmeans::emmeans(m_vsp_ord, ~ VoiGrp, mode = "latent"),
        method = six, adjust = "holm")),
      error = function(e) paste("emmeans cross-check unavailable:",
                                conditionMessage(e))), digits = 3)
  }
  
  if (!is.null(m_vsp_ord_fx)) {
    vsp_ct_ord_fx <- clmm_six(m_vsp_ord_fx)
    cat("\nItem as a fixed factor, six planned contrasts\n")
    print(vsp_ct_ord_fx, digits = 3)
    if (!is.null(vsp_ct_ord))
      cat(sprintf("Largest absolute difference in the six estimates: %.4f log-odds\n",
                  max(abs(vsp_ct_ord$estimate - vsp_ct_ord_fx$estimate))))
  }
  
  ## ---- 7.5 Do the ordinal and interval treatments agree? ---------------------
  ## Compares the six contrasts under the ordinal and interval treatments. The
  ## two are on different scales (log-odds against scale points), so they are
  ## compared by sign and rank, not magnitude.
  if (!is.null(vsp_ct_ord)) {
    lin <- as.data.frame(summary(res_vsp_soc$contrasts))
    k   <- match(vsp_ct_ord$contrast, lin$contrast)
    if (anyNA(k))
      warning("Contrast labels differ between the two models; check `six`.")
    cmp <- data.frame(
      contrast  = vsp_ct_ord$contrast,
      ord_est   = vsp_ct_ord$estimate,
      ord_p     = vsp_ct_ord$p_holm,
      lin_est   = lin$estimate[k],
      lin_p     = lin$p.value[k])
    cmp$same_sign <- sign(cmp$ord_est) == sign(cmp$lin_est)
    
    cat("\nOrdinal CLMM (log-odds, asymptotic) vs summed score (points, KR)\n")
    print(cmp, digits = 3, row.names = FALSE)
    cat(sprintf("\nSigns agree on %d of %d contrasts; Spearman rho = %.3f\n",
                sum(cmp$same_sign), nrow(cmp),
                cor(cmp$ord_est, cmp$lin_est, method = "spearman")))
    
    key <- cmp$contrast == "AICV - HCV"
    if (any(key))
      cat(sprintf("Rank of 'AICV - HCV' by |estimate|: ordinal %d, linear %d (of %d)\n",
                  rank(-abs(cmp$ord_est))[key], rank(-abs(cmp$lin_est))[key],
                  nrow(cmp)))
    
    write.csv(cmp, file.path(OUT_DIR, "vsp_ordinal_vs_linear_sensitivity.csv"),
              row.names = FALSE)
    cat("Wrote ", "vsp_ordinal_vs_linear_sensitivity.csv", "\n",
        sep = "")
  }
}   # end if (RUN_ORDINAL_CHECK)


## =============================================================================
## 8. LEARNING (Fig. 4b; Supplementary Tables S11, S12)
## =============================================================================

cat("\n\n===== 8. Learning =====\n")

## ---- 8.1 Item-level logistic mixed model -----------------------------------
## Each post-test item is a binary outcome, with crossed random effects for
## participant, item and class. N = 3,728; see actual fitted model output.
m_learn_item <- NULL
if (!HAVE_POST_ITEMS) {
  cat("\nSKIPPED: the item-level model needs the eight binary post-test\n",
      "responses per lesson, either as an 'items' sheet or as columns\n",
      paste(POST_ITEM_COLS, collapse = ", "), ".\n",
      "The total-score model in Section 8.2 still runs.\n", sep = "")
} else {
  m_learn_item <- lme4::glmer(
    correct ~ VoiGrp + lesson + pre_prop_c + (1 | ID) + (1 | item) + (1 | Class),
    data = dfi, family = binomial,
    control = lme4::glmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 2e5)))
  cat("\n[Table S8, left] Item-level logistic mixed model\n")
  print(summary(m_learn_item))
  cat(sprintf("\nN = %d responses, AIC = %.1f\n", nrow(dfi), AIC(m_learn_item)))
  
  b_li <- lme4::fixef(m_learn_item)
  V_li <- as.matrix(vcov(m_learn_item))
  L_li <- build_L(names(b_li))
  cat("\nSix planned contrasts, item-level model, asymptotic\n")
  print(wald_contrasts(b_li, V_li, L_li, df = Inf), digits = 3)
}   # end if (HAVE_POST_ITEMS)

## ---- 8.2 Total-score model -------------------------------------------------
## Post ~ VoiGrp + Pre_c + lesson + (1|Class) + (1|ID).
## Expected: N = 466, residual variance 1.388, no condition differing from
## the reference (all p > 0.16), prior knowledge beta = 0.562, and CR2
## intervals of [-0.21, 0.68] items for AICV and [-0.86, 1.26] for AIENV.
res_post <- three_regimes("Post", "Post-test total (items of 8)",
                          covariates = "Pre_c")
m_post <- res_post$lmm
cat("\n[Table S8, right] CR2 confidence intervals on the total score\n")
print(round(cbind(Estimate = res_post$robust$coefficients,
                  ci_low   = res_post$robust$conf.low,
                  ci_high  = res_post$robust$conf.high,
                  df       = res_post$robust$df), 3))

## ---- 8.3 Adjusted post-test means (Fig. 4b) --------------------------------
## Model-implied means at the mean pre-test score, averaged over lessons.
## Expected range 5.37 to 5.69 items correct out of eight.
emm_post <- emmeans::emmeans(m_post, ~ VoiGrp, at = list(Pre_c = 0),
                             weights = "equal")
cat("\n[Fig. 4b] Adjusted post-test means\n")
print(summary(emm_post), digits = 4)

cat("\nSix planned contrasts on adjusted means, Holm-adjusted\n")
print(summary(emmeans::contrast(emm_post, method = six, adjust = "holm")), digits = 3)

## ---- 8.4 Moderation by prior knowledge (Table S12) -------------------------
## The likelihood-ratio test is computed from current data; report the logged output.
m_post_ml  <- lme4::lmer(Post ~ VoiGrp + Pre_c + lesson + (1 | Class) + (1 | ID),
                         data = d, REML = FALSE)
m_post_int <- lme4::lmer(Post ~ VoiGrp * Pre_c + lesson + (1 | Class) + (1 | ID),
                         data = d, REML = FALSE)
cat("\n[Table S12] Likelihood-ratio test for voice condition x prior knowledge\n")
print(anova(m_post_ml, m_post_int))
cat("\nInteraction terms\n")
ci <- summary(lmerTest::lmer(Post ~ VoiGrp * Pre_c + lesson + (1 | Class) + (1 | ID),
                             data = d), ddf = "Kenward-Roger")$coefficients
print(round(ci[grep(":", rownames(ci)), , drop = FALSE], 3))


## =============================================================================
## 9. COGNITIVE LOAD, ENGAGEMENT AND AFFECT (Supplementary Tables S8, S9, S10)
## =============================================================================

cat("\n\n===== 9. Cognitive load, engagement and affect =====\n")
## The three extraneous items name the manipulation directly, which makes
## that subscale the sharpest test the design affords.
res_cl_tot  <- three_regimes("CL_total", "Cognitive load, total (5 items)", store = FALSE)
res_cl_intr <- NULL; res_cl_extr <- NULL
if (HAVE_CL_ITEMS) {
  res_cl_intr <- three_regimes("CL_intr", "Cognitive load, intrinsic (2 items)")
  res_cl_extr <- three_regimes("CL_extr", "Cognitive load, extraneous (3 items)")
} else {
  cat("\nSKIPPED: the intrinsic and extraneous subscales need ",
      paste(c(CL_INTRINSIC, CL_EXTRANEOUS), collapse = ", "), ".\n",
      "The extraneous subscale is the sharpest test the design affords,\n",
      "because those three items name the manipulation directly.\n", sep = "")
}
res_le      <- three_regimes("LE_total", "Learning engagement (6 items)")
res_pa      <- three_regimes("PA_chg",   "Positive affect change")
res_na      <- three_regimes("NA_chg",   "Negative affect change")

## ---- 9b Optional ordinal check on the three extraneous-load items ----------
## Item-level refit of the three extraneous-load items (seven-point scale).
## Three levels are too few for an item random effect, so item is fixed.
m_cl_ord <- NULL
if (RUN_ORDINAL_CHECK && HAVE_CL_ITEMS) {
  cat("\n\n----- 9b Ordinal check, three extraneous-load items -----\n")
  cl_extr_long <- build_item_long(CL_EXTRANEOUS, scale_max = 7)
  cat(sprintf("N = %d item responses\n", nrow(cl_extr_long)))
  m_cl_ord <- fit_clmm_safe(
    resp ~ VoiGrp + lesson + item + (1 | Class) + (1 | ID),
    cl_extr_long, "extraneous load")
  if (!is.null(m_cl_ord)) {
    print(summary(m_cl_ord))
    cat("\nSix planned contrasts, Holm-adjusted\n")
    print(clmm_six(m_cl_ord), digits = 3)
  }
}


## =============================================================================
## 10. AUTONOMIC MEASURES (Supplementary Tables S8, S9, S13)
## =============================================================================

cat("\n\n===== 10. Physiological reactivity =====\n")
res_hr   <- three_regimes("dHR",   "Heart-rate change (bpm)")
res_scl  <- three_regimes("dSCL",  "Skin conductance level change")
res_ascr <- three_regimes("daSCR", "SCR amplitude change")

## ---- 10b Sensitivity: electrodermal outliers -------------------------------
## The electrodermal reactivity scores are heavy-tailed; both models are
## refitted with the flagged participants excluded.
if (length(EDA_OUTLIER_IDS)) {
  cat("\n\n----- 10b Refit excluding ", length(EDA_OUTLIER_IDS),
      " participants with |dSCL| or |daSCR| > ", EDA_CUTOFF, " -----\n", sep = "")
  d_eda <- d[!(as.character(d$ID) %in% EDA_OUTLIER_IDS), ]
  cat("N = ", nrow(d_eda), " observations from ",
      dplyr::n_distinct(d_eda$ID), " participants\n", sep = "")
  invisible(three_regimes("dSCL",  "dSCL, outliers excluded",
                          data = d_eda, store = FALSE))
  invisible(three_regimes("daSCR", "daSCR, outliers excluded",
                          data = d_eda, store = FALSE))
} else {
  cat("\nNo electrodermal outliers beyond the cutoff; 10b not required.\n")
}


## =============================================================================
## 11. REPEATED EXPOSURE (Supplementary Table S14)
## =============================================================================

cat("\n\n===== 11. Exploratory model of repeated exposure (Table S14) =====\n")
## Session is orthogonal to voice condition because lesson order was
## counterbalanced at the class level.
## Exploratory session coefficients are computed from the current fitted model.
if (!HAVE_SAT_ITEMS) {
  cat("\nSKIPPED: needs the two raw satisfaction items.\n")
} else {
  m_expo <- ordinal::clmm(
    sat_resp ~ VoiGrp * session + lesson + (1 | Class) + (1 | ID),
    data = sat_long, link = "logit", Hess = TRUE)
  print(summary(m_expo))
  cat(sprintf("\nlogLik = %.2f, AIC = %.2f\n",
              as.numeric(logLik(m_expo)), AIC(m_expo)))
}

cat("\nMean summed satisfaction by condition and session\n")
print(round(tapply(d$Sat_sum, list(d$VoiGrp, d$session), mean), 2))


## =============================================================================
## 12. INTRACLASS CORRELATIONS AND DESIGN EFFECTS (Supplementary Table S15)
## =============================================================================

cat("\n\n===== 12. ICC and design effects (Table S15) =====\n")
## Precision and degrees of freedom are separate matters: near-zero ICCs mean
## little loss of precision, but a condition contrast still carries about five
## degrees of freedom under Kenward-Roger and about two under CR2.
icc_tab <- rbind(
  Satisfaction      = icc_design(res_sat_lmm$lmm),
  Learning_total    = icc_design(res_post$lmm),
  Cognitive_load    = icc_design(res_cl_tot$lmm),
  Voice_evaluation  = icc_design(res_vsp_soc$lmm),
  dSCR              = icc_design(res_ascr$lmm),
  dSCL              = icc_design(res_scl$lmm),
  dHR               = icc_design(res_hr$lmm)
)
## Item-level learning model, computed separately because it is a glmer.
if (HAVE_POST_ITEMS) {
  ## VarCorr() returns a data frame whose first columns are character, so
  ## round() cannot be applied to the whole frame.
  vc_li <- as.data.frame(lme4::VarCorr(m_learn_item))
  vc_li <- data.frame(grp = vc_li$grp,
                      vcov  = round(vc_li$vcov, 4),
                      sdcor = round(vc_li$sdcor, 4))
  cat("\nItem-level learning model, variance components:\n")
  print(vc_li, row.names = FALSE)
}
print(round(icc_tab, 4))
write.csv(round(icc_tab, 4), file.path(OUT_DIR, "table_s15_icc_design_effects.csv"))


## =============================================================================
## 13. SIMULATION-BASED SENSITIVITY ANALYSIS (Methods)
## =============================================================================

cat("\n\n===== 13. Sensitivity analysis with simr =====\n")
## The a priori analysis (G*Power, one-way omnibus F, five groups, f = 0.25,
## alpha = 0.05, 80% power) required N = 200 and assumed independent students.
## This takes the realised clustered design as given instead, and asks what
## difference it could have detected.
## Latest logged run: 0.267 at 0.3 items, 0.804 at 0.6 items, 0.958 at 0.8 items (1,000 simulations).
## Slow: N_SIM x 3 model fits. Set N_SIM <- 200 for a dry run.

library(lme4)
library(simr)

m_sens_base <- lme4::lmer(Post ~ VoiGrp + Pre_c + lesson + (1 | Class) + (1 | ID),
                          data = d)
simr::simrOptions(progress = FALSE)
sens_grid <- c(0.3, 0.6, 0.8)
sens_out  <- data.frame(effect_items = sens_grid, power = NA_real_,
                        ci_low = NA_real_, ci_high = NA_real_)
for (i in seq_along(sens_grid)) {
  ## Substitute a known effect size so powerSim() simulates under it.
  m_alt <- m_sens_base
  fixef(m_alt)["VoiGrpAICV"] <- sens_grid[i]
  ps <- tryCatch(
    simr::powerSim(m_alt, test = simr::fixed("VoiGrpAICV", "t"),
                   nsim = N_SIM, seed = 20260101),
    error = function(e) {cat("  simr failed:", conditionMessage(e), "\n"); NULL})
  if (!is.null(ps)) {
    s <- summary(ps)
    sens_out[i, 2:4] <- c(s$mean, s$lower, s$upper)
    cat(sprintf("  effect = %.1f items: power = %.3f, 95%% CI [%.3f, %.3f]\n",
                sens_grid[i], s$mean, s$lower, s$upper))
  }
}
write.csv(sens_out, file.path(OUT_DIR, "sensitivity_power.csv"), row.names = FALSE)


## =============================================================================
## 14. PERCEPTUAL VALIDATION STUDY (Fig. 2; Supplementary Tables S3, S4)
## =============================================================================

cat("\n\n===== 14. Perceptual validation =====\n")
## Ninety university students who took no part in the learning experiment
## each rated ten recordings, one for each voice-by-lesson combination,
## on 19 seven-point items and two forced-choice items.
## N = 900 listener-by-recording observations.

## Expected column names in perception.xlsx. Adjust to match your file.
PV_LISTENER <- "participant_id"
PV_VOICE    <- "condition_code"
PV_LESSON   <- "lesson"
PV_EXCERPT  <- "excerpt_id"   # Unique physical audio clip ID (30 distinct excerpts)
PV_STYLE    <- "prosody_choice"  # forced choice: calm / neutral / energetic / unsure
PV_SOURCE   <- "source_judgment" # forced choice: human / AI / unsure
PV_CONF     <- "source_confidence" # confidence, 1-7
PV_CONSTRUCTS <- list(
  Calmness       = c("calm_1", "calm_2"),
  Energy         = c("energy_1", "energy_2"),
  Neutrality     = c("neutral_1", "neutral_2"),
  Expressiveness = c("express_1", "express_2", "express_3"),
  Naturalness    = c("natural_1", "natural_2", "natural_3"),
  Syntheticness  = c("synthetic_1", "synthetic_2", "synthetic_3"),
  Clarity        = c("clarity_1", "clarity_2"),
  Quality        = c("voice_quality"),
  Pleasantness   = c("pleasantness")
)
## Intended style and true source for each voice condition.
INTENDED_STYLE <- c(HNV = "neutral", HCV = "calm", AINV = "neutral",
                    AICV = "calm",   AIENV = "energetic")
TRUE_SOURCE    <- c(HNV = "human", HCV = "human", AINV = "AI",
                    AICV = "AI",   AIENV = "AI")

if (!file.exists(FILE_PV)) {
  cat("  ", FILE_PV, " not found; Section 14 skipped.\n", sep = "")
} else {
  pv <- readxl::read_excel(FILE_PV) |> as.data.frame()
  preflight(pv, c(PV_LISTENER, PV_VOICE, PV_LESSON, PV_EXCERPT, PV_STYLE, PV_SOURCE,
                  PV_CONF, unlist(PV_CONSTRUCTS)), "perception data")
  pv$listener <- factor(trimws(as.character(pv[[PV_LISTENER]])))
  pv$voice <- recode_levels(pv[[PV_VOICE]], VOICE_LEVELS, VOICE_RECODE,
                            "Perception voice", "VOICE_RECODE")
  
  ## Each listener evaluates ten clips: one per voice-by-lesson cell.
  ## The 3 stimulus versions contain distinct physical excerpts for each
  ## voice-by-lesson cell, so the excerpt random effect needs 30 clip IDs,
  ## not merely the 10 voice-by-lesson cells.
  lesson_key <- trimws(as.character(pv[[PV_LESSON]]))
  excerpt_key <- trimws(as.character(pv[[PV_EXCERPT]]))
  if (anyNA(lesson_key) || any(!nzchar(lesson_key)))
    stop("Perception data contain missing or blank lesson values.")
  if (anyNA(excerpt_key) || any(!nzchar(excerpt_key)))
    stop("perception.xlsx excerpt_id contains missing or blank values.")
  pv$excerpt <- factor(excerpt_key)
  pv$voice_lesson <- interaction(pv$voice, factor(lesson_key), drop = TRUE)
  if (nrow(pv) != 900L || nlevels(pv$listener) != 90L ||
      nlevels(pv$voice_lesson) != 10L ||
      !all(as.integer(table(pv$listener, pv$voice_lesson)) == 1L)) {
    stop("Expected 90 listeners, 900 rows, and exactly one clip per listener per voice-by-lesson cell.")
  }
  ## A global excerpt_id must map to exactly one voice-by-lesson cell.
  excerpt_mapping <- unique(data.frame(excerpt = excerpt_key,
                                       voice_lesson = as.character(pv$voice_lesson)))
  if (any(table(excerpt_mapping$excerpt) != 1L))
    stop("excerpt_id was reused for different voice-by-lesson clips. Supply a globally unique clip ID.")
  clips_per_cell <- table(excerpt_mapping$voice_lesson)
  clip_counts <- table(pv$excerpt)
  if (nlevels(pv$excerpt) != 30L ||
      !all(clips_per_cell == 3L) ||
      !all(as.integer(clip_counts) == 30L)) {
    stop("The described three-version design expects 30 distinct clips: 3 per voice-by-lesson cell, each rated by 30 listeners. Found ",
         nlevels(pv$excerpt), " unique excerpt IDs; inspect the workbook's clip mapping.")
  }
  cat("Perceptual data verified: 90 listeners x 10 ratings; 30 distinct physical excerpts (3 per voice-by-lesson cell).\n")
  print(data.frame(excerpt = names(clip_counts), n_ratings = as.integer(clip_counts)), row.names = FALSE)
  write.csv(excerpt_mapping, file.path(OUT_DIR, "perception_excerpt_mapping.csv"), row.names = FALSE)
  
  ## ---- 14.1 Construct scores and reliability -------------------------------
  cat("\nConstruct reliabilities (Cronbach's alpha, multi-item scales only)\n")
  for (nm in names(PV_CONSTRUCTS)) {
    items <- PV_CONSTRUCTS[[nm]]
    pv[[nm]] <- rowMeans(pv[, items, drop = FALSE])
    if (length(items) > 1) {
      a <- suppressWarnings(psych::alpha(pv[, items], warnings = FALSE,
                                         check.keys = FALSE))
      cat(sprintf("  %-16s k = %d, alpha = %.2f\n", nm, length(items),
                  a$total$raw_alpha))
    }
  }
  
  ## ---- 14.2 Mean ratings by condition (Table S4, upper block) --------------
  cat("\n[Table S4] Mean rating by condition\n")
  pv_means <- sapply(names(PV_CONSTRUCTS),
                     function(nm) tapply(pv[[nm]], pv$voice, mean, na.rm = TRUE))
  print(round(t(pv_means), 2))
  write.csv(round(t(pv_means), 3), file.path(OUT_DIR, "table_s4_means.csv"))
  
  ## ---- 14.3 Linear mixed models per construct ------------------------------
  ## Use the physical audio excerpt as a random effect (30 distinct clips)
  ## and listener as the other random intercept. The two matched-style
  ## source contrasts retain the original S4 asymptotic-Wald p-value method
  ## so we can isolate the effect of restoring physical excerpt IDs.
  ## Finite-Satterthwaite p-values are exported for sensitivity comparison.
  ## S4 p-values are unadjusted; the 6 planned learner contrasts elsewhere
  ## are Holm-adjusted. Do not confuse these distinct families of tests.
  pv_rows <- NULL
  pv_coefs <- NULL
  s4_contrasts <- list(
    "AINV - HNV" = c(-1, 0, 1, 0, 0),
    "AICV - HCV" = c(0, -1, 0, 1, 0)
  )
  for (nm in names(PV_CONSTRUCTS)) {
    f <- as.formula(paste(nm, "~ voice + (1|listener) + (1|excerpt)"))
    m <- lmerTest::lmer(f, data = pv)
    b <- lme4::fixef(m)
    em <- emmeans::emmeans(m, ~ voice, lmer.df = "satterthwaite")
    pw <- as.data.frame(summary(emmeans::contrast(
      em, method = s4_contrasts, adjust = "none")))
    stopifnot(all(c("contrast", "estimate", "SE", "df", "t.ratio", "p.value") %in% names(pw)))
    pw$p_finite <- pw$p.value
    pw$p_asymptotic <- 2 * stats::pnorm(-abs(pw$t.ratio))
    pw$p.value <- pw$p_asymptotic  # preserve historical S4 Wald inference
    pw$construct <- nm
    cat(sprintf("\n%s (random effects: listener + physical excerpt)\n", nm))
    print(round(summary(m)$coefficients, 3))
    cat("  source contrasts (S4: asymptotic Wald; finite-df sensitivity alongside):\n")
    print(pw[, c("contrast", "estimate", "SE", "df", "t.ratio", "p.value", "p_finite")],
          digits = 4, row.names = FALSE)
    pv_rows <- rbind(pv_rows, pw[, c("construct", "contrast", "estimate", "SE", "df", "t.ratio", "p.value", "p_finite")])
    vr <- grep("^voice", names(b), value = TRUE)
    pv_coefs <- rbind(pv_coefs, data.frame(
      construct = nm, term = sub("^voice", "", vr),
      estimate = as.numeric(b[vr]), row.names = NULL))
  }
  write.csv(pv_rows, file.path(OUT_DIR, "table_s4_source_contrasts.csv"), row.names = FALSE)
  write.csv(pv_coefs, file.path(OUT_DIR, "table_s4_condition_coefficients.csv"), row.names = FALSE)
  
  ## Audit against the CURRENT values printed in Supplementary Table S4.
  ## This is a consistency check, not a constraint on the model result:
  ## update the manuscript when differences appear; NEVER overwrite outputs
  ## with the published values to make the audit pass.
  s4_reference <- data.frame(
    construct = rep(names(PV_CONSTRUCTS), each = 2),
    contrast = rep(c("AINV - HNV", "AICV - HCV"), times = length(PV_CONSTRUCTS)),
    beta_S4 = c(-0.26, -0.22,  0.05, 0.19, -0.31, 0.34,
                -0.15, -0.11, -1.02, -1.15, 2.82, 3.01,
                -0.09, -0.09, -0.51, -0.99, -0.66, -1.08),
    p_S4 = c("0.009", "0.032", "0.624", "0.044",
             "<0.001", "<0.001", "0.175", "0.329",
             "<0.001", "<0.001", "<0.001", "<0.001",
             "0.315", "0.271", "<0.001", "<0.001",
             "<0.001", "<0.001"),
    stringsAsFactors = FALSE)
  audit <- merge(s4_reference, pv_rows, by = c("construct", "contrast"),
                 all.x = TRUE, sort = FALSE)
  audit$beta_matches_2dp <- round(audit$estimate, 2) == audit$beta_S4
  audit$p_computed_3dp <- formatC(audit$p.value, digits = 3, format = "f")
  audit$p_finite_3dp <- formatC(audit$p_finite, digits = 3, format = "f")
  audit$p_matches_report <- ifelse(
    audit$p_S4 == "<0.001", audit$p.value < 0.001,
    audit$p_computed_3dp == audit$p_S4)
  audit <- audit[match(paste(s4_reference$construct, s4_reference$contrast),
                       paste(audit$construct, audit$contrast)), ]
  write.csv(audit, file.path(OUT_DIR, "table_s4_audit_vs_supplement.csv"), row.names = FALSE)
  cat("\n[Table S4 consistency check] (TRUE = matches current supplement)\n")
  print(audit[, c("construct", "contrast", "beta_S4", "estimate", "beta_matches_2dp",
                  "p_S4", "p_computed_3dp", "p_finite_3dp", "df", "p_matches_report")],
        row.names = FALSE)
  cat("  Matched beta rows: ", sum(audit$beta_matches_2dp, na.rm = TRUE), "/18; matched p rows: ",
      sum(audit$p_matches_report, na.rm = TRUE), "/18.\n", sep = "")
  cat("  If p-values differ, update Supplementary Table S4 only after checking ",
      "excerpt mapping and the chosen p-value method.\n", sep = "")
  
  cat("\nCondition coefficients relative to HNV, range by construct\n")
  print(round(tapply(pv_coefs$estimate, pv_coefs$construct,
                     function(x) c(min = min(x), max = max(x))) |>
                do.call(what = rbind), 3))
  
  ## ---- 14.4 Forced-choice manipulation checks ------------------------------
  ## Aggregated within listener and tested against chance: one third for the
  ## three-way style categorisation, one half for the source judgement after
  ## excluding the trials answered "unsure".
  style_response <- tolower(trimws(as.character(pv[[PV_STYLE]])))
  source_response <- tolower(trimws(as.character(pv[[PV_SOURCE]])))
  source_response[is.na(source_response) | source_response == ""] <- "unsure"
  source_response[source_response %in% c("ai-generated", "ai generated", "synthetic", "machine")] <- "ai"
  pv$style_hit <- style_response == INTENDED_STYLE[as.character(pv$voice)]
  pv$src_commit <- !(source_response %in% c("unsure", "not sure", "na"))
  if (any(!source_response[pv$src_commit] %in% c("human", "ai")))
    stop("Unexpected committed source label; inspect PV_SOURCE coding.")
  pv$src_hit <- source_response == tolower(TRUE_SOURCE[as.character(pv$voice)])
  
  cat(sprintf("\nStyle categorisation: %.1f%% of trials matched the intended category\n",
              100 * mean(pv$style_hit, na.rm = TRUE)))
  style_by_listener <- tapply(pv$style_hit, pv$listener, mean, na.rm = TRUE)
  print(t.test(style_by_listener, mu = 1/3))
  
  cat(sprintf("\nSource: %.1f%% of trials answered 'unsure'; accuracy computed on %d committed trials\n",
              100 * mean(!pv$src_commit), sum(pv$src_commit)))
  src_by_listener <- tapply(pv$src_hit[pv$src_commit],
                            pv$listener[pv$src_commit], mean, na.rm = TRUE)
  cat(sprintf("Overall source accuracy on committed trials: %.1f%%\n",
              100 * mean(pv$src_hit[pv$src_commit], na.rm = TRUE)))
  print(t.test(src_by_listener, mu = 0.5))
  
  cat("\nAccuracy by voice condition (committed trials, Fig. 2b)\n")
  print(round(100 * tapply(pv$src_hit[pv$src_commit],
                           pv$voice[pv$src_commit], mean, na.rm = TRUE), 1))
  cat("\nPercentage judged AI-generated, by condition\n")
  print(round(100 * tapply(source_response == "ai", pv$voice,
                           mean, na.rm = TRUE), 1))
  
  ## Confidence: human voices against synthetic ones, paired within listener.
  pv$is_human <- TRUE_SOURCE[as.character(pv$voice)] == "human"
  conf_h <- tapply(pv[[PV_CONF]][pv$is_human],  pv$listener[pv$is_human],  mean)
  conf_a <- tapply(pv[[PV_CONF]][!pv$is_human], pv$listener[!pv$is_human], mean)
  cat(sprintf("\nSource confidence: human M = %.2f, AI M = %.2f\n",
              mean(conf_h), mean(conf_a)))
  print(t.test(conf_h, conf_a, paired = TRUE))
}


## =============================================================================
## 15. EXPORT FOR THE FIGURE SCRIPTS
## =============================================================================

cat("\n\n===== 15. Export =====\n")
## Statistics and plotting are kept separate so that they can be verified
## independently. This CSV is a supporting export and is not required by the current main-figure script.

## Add the models that three_regimes() does not cover: the ordinal
## satisfaction model, the item-level learning model and the ordinal
## voice-evaluation check, all on log-odds.
add_logodds <- function(label, coefs, ses, ps) {
  data.frame(outcome = label, term = sub("^VoiGrp", "", names(coefs)),
             estimate = as.numeric(coefs), se = as.numeric(ses),
             df = Inf, p = as.numeric(ps), p_ols_cr2 = NA_real_,
             ci_low  = as.numeric(coefs) - 1.96 * as.numeric(ses),
             ci_high = as.numeric(coefs) + 1.96 * as.numeric(ses),
             row.names = NULL)
}
if (HAVE_SAT_ITEMS) {
  cs <- coef(summary(m_sat_clmm))
  vr <- grep("^VoiGrp", rownames(cs), value = TRUE)
  ESTIMATES <- rbind(ESTIMATES,
                     add_logodds("Satisfaction (CLMM, log-odds)",
                                 setNames(cs[vr, "Estimate"], vr), cs[vr, "Std. Error"],
                                 cs[vr, "Pr(>|z|)"]))
}
if (HAVE_POST_ITEMS) {
  cl <- coef(summary(m_learn_item))
  vr <- grep("^VoiGrp", rownames(cl), value = TRUE)
  ESTIMATES <- rbind(ESTIMATES,
                     add_logodds("Post-test item level (GLMM, log-odds)",
                                 setNames(cl[vr, "Estimate"], vr), cl[vr, "Std. Error"],
                                 cl[vr, "Pr(>|z|)"]))
}
if (!is.null(m_vsp_ord)) {
  cv <- coef(summary(m_vsp_ord))
  vr <- grep("^VoiGrp", rownames(cv), value = TRUE)
  ESTIMATES <- rbind(ESTIMATES,
                     add_logodds("Voice evaluation, social (CLMM, log-odds)",
                                 setNames(cv[vr, "Estimate"], vr), cv[vr, "Std. Error"],
                                 cv[vr, "Pr(>|z|)"]))
}

ESTIMATES$term <- factor(ESTIMATES$term, levels = VOICE_LEVELS[-1])
ESTIMATES <- ESTIMATES[order(ESTIMATES$outcome, ESTIMATES$term), ]
write.csv(ESTIMATES, file.path(OUT_DIR, "model_estimates.csv"), row.names = FALSE)
cat("Wrote ", "model_estimates.csv", " (",
    nrow(ESTIMATES), " rows)\n", sep = "")
print(ESTIMATES, digits = 3, row.names = FALSE)


## =============================================================================
## 16. SESSION INFO
## =============================================================================

cat("\n\n===== 16. Session info =====\n")
print(sessionInfo())
cat("\nDone.\n")

close_log()
