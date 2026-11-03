# Data sets of the FruitTrait package from the validation tables in data-raw/validation.
#
# Run from the package root: Rscript data-raw/make_datasets.R
# fr_validation and fr_reference are written first. fr_benchmark and fr_catalogue also need the invariance,
# augmentation and agreement tables written by data-raw/validation/code/invariance_agreement.R, which uses
# fr_validation; run make_datasets.R, then invariance_agreement.R, then make_datasets.R again.
V <- file.path("data-raw", "validation")
rd <- function(f, ...) utils::read.csv(file.path(V, f), stringsAsFactors = FALSE, na.strings = "", ...)
lgl <- function(v) toupper(as.character(v)) %in% c("TRUE", "1")

# ---- fr_validation: two photos of the same plant on the same occasion
fr_validation <- rd("validation_pairs.csv", colClasses = c(occurrenceKey = "character", photo_id = "character"))
fr_validation$measurable <- lgl(fr_validation$measurable)
fr_validation$both_measurable <- lgl(fr_validation$both_measurable)
save(fr_validation, file = "data/fr_validation.rda", compress = "xz")

# ---- fr_reference: colour of the reference species in photos and in reflectance spectra
fr_reference <- rd("reference_species.csv")
save(fr_reference, file = "data/fr_reference.rda", compress = "xz")

pkgload::load_all(".", quiet = TRUE)

# ---- fr_benchmark
outlines <- rd("benchmark_outlines.csv", colClasses = c(photo_id = "character"))
photos <- rd("benchmark_photos.csv", colClasses = c(photo_id = "character", inat_obs_id = "character"))
photos$cut_by_frame <- lgl(photos$cut_by_frame)
met <- jsonlite::fromJSON(file.path(V, "fruit_heads_metrics.json"), simplifyVector = FALSE)
heads <- do.call(rbind, lapply(c("valid", "ripe", "dry"), function(h) {
  m <- met[[h]]
  g <- function(k) if (is.null(m[[k]])) NA else m[[k]]
  data.frame(head = h, fruits = g("fruits"), positive = g("positive"), negative = g("negative"),
             species_positive = g("species_positive"), species_negative = g("species_negative"),
             feature_set = g("feature_set"), cv_auc = g("cv_auc"), oof_balanced_accuracy = g("oof_balanced_accuracy"),
             nested_cv_auc = g("nested_cv_auc"), nested_oof_balanced_accuracy = g("nested_oof_balanced_accuracy"),
             usable = isTRUE(g("usable")), stringsAsFactors = FALSE)
}))
feature_sets <- rd("fruit_heads_eval.csv")
single <- outlines$cls %in% c("F", "Y")
rules <- do.call(rbind, lapply(c(0.3, 0.5, 0.7, 0.8, 0.9), function(t) {
  kv <- !is.na(outlines$p_valid) & outlines$p_valid >= t
  rp <- single & !is.na(outlines$stage) & !is.na(outlines$p_ripe)
  kr <- rp & kv & outlines$p_ripe >= t
  data.frame(threshold = t,
             valid_kept = sum(kv), valid_share_kept = mean(kv),
             valid_precision = mean(single[kv]), valid_recall = sum(kv & single) / sum(single),
             ripe_rated = sum(rp & kv), ripe_kept = sum(kr),
             ripe_precision = mean(outlines$stage[kr] == "R"),
             ripe_recall = sum(kr & outlines$stage == "R") / sum(rp & kv & outlines$stage == "R"))
}))
rr <- outlines[single & !is.na(outlines$stage) & !is.na(outlines$p_ripe), ]
ripe_colour <- do.call(rbind, lapply(split(rr, list(rr$stage == "R", rr$name), drop = TRUE), function(d) {
  data.frame(rated = if (d$stage[1] == "R") "ripe" else "unripe or damaged", name = d$name[1],
             fruits = nrow(d), share_classified_ripe = mean(d$p_ripe >= 0.5), stringsAsFactors = FALSE)
}))
ripe_colour <- ripe_colour[order(ripe_colour$rated, -ripe_colour$fruits), ]
rownames(ripe_colour) <- NULL
opt <- function(f) if (file.exists(file.path(V, f))) rd(f, colClasses = c(photo_id = "character")) else NULL
invariance <- if (file.exists(file.path(V, "invariance_icc.csv"))) rd("invariance_icc.csv", check.names = FALSE) else NULL
invariance_n <- NULL
if (!is.null(invariance)) {
  il <- rd("invariance_long.csv", colClasses = c(photo_id = "character"))
  org <- il[il$condition == "original", ]
  invariance_n <- do.call(rbind, lapply(setdiff(names(invariance), "trait"), function(cd) {
    ids <- intersect(org$id, il$id[il$condition == cd])
    data.frame(condition = cd, fruits = length(ids), crops = length(unique(org$photo_id[org$id %in% ids])))
  }))
}
fr_benchmark <- list(photos = photos, outlines = outlines, heads = heads, feature_sets = feature_sets,
                     rules = rules, ripe_colour = ripe_colour, invariance = invariance, invariance_n = invariance_n,
                     augmentation = opt("augmentation.csv"), r_python = opt("r_python.csv"))
save(fr_benchmark, file = "data/fr_benchmark.rda", compress = "xz")

# ---- fr_catalogue
def <- utils::read.csv(text = '
trait|level|family|type|unit|computed_by|description
m_L|photo|colour|numeric|L* (0-100)|R and back end|Mean CIELAB lightness L* (D65) of the measurable fruits of the photo; each fruit without its brightest 2% of pixels
m_a|photo|colour|numeric|a*|R and back end|Mean CIELAB a* (green to red) of the measurable fruits
m_b|photo|colour|numeric|b*|R and back end|Mean CIELAB b* (blue to yellow) of the measurable fruits
m_C|photo|colour|numeric|C*|R and back end|Chroma, from the mean a* and b* of the measurable fruits
m_hue|photo|colour|circular|degrees|R and back end|Hue angle, from the mean a* and b* of the measurable fruits
m_name|photo|colour|categorical||R and back end|Colour name of the mean colour of the measurable fruits (fixed rule on L*, C* and hue)
m_wb_L|photo|white-balanced colour|numeric|L* (0-100)|R and back end|L* of the measurable fruits after a grey-world white balance of the crop
m_wb_a|photo|white-balanced colour|numeric|a*|R and back end|a* of the measurable fruits after a grey-world white balance of the crop
m_wb_b|photo|white-balanced colour|numeric|b*|R and back end|b* of the measurable fruits after a grey-world white balance of the crop
v_L|photo|colour of valid fruits|numeric|L* (0-100)|R and back end|Mean L* of all valid fruits, ripe or not
v_a|photo|colour of valid fruits|numeric|a*|R and back end|Mean a* of all valid fruits, ripe or not
v_b|photo|colour of valid fruits|numeric|b*|R and back end|Mean b* of all valid fruits, ripe or not
v_C|photo|colour of valid fruits|numeric|C*|R and back end|Chroma of all valid fruits, ripe or not
v_hue|photo|colour of valid fruits|circular|degrees|R and back end|Hue angle of all valid fruits, ripe or not
v_name|photo|colour of valid fruits|categorical||R and back end|Colour name of the mean colour of all valid fruits
ring_dE00|photo|surroundings|numeric|CIEDE2000|back end|Colour difference between the fruits that passed the outline checks and a ring around them
ring_dL|photo|surroundings|numeric|L* units|back end|Lightness of the fruits minus that of the ring around them
gloss_frac|photo|surface|numeric|share|back end|Share of specular (brightest, unsaturated) pixels in the fruits that passed the outline checks
m_diam_px|photo|size and shape|numeric|pixels|back end|Median equivalent diameter of the measurable fruits, in pixels of the photo
fruit_elongation|photo|size and shape|numeric|ratio|back end|Median ratio of the longer to the shorter side of the minimum-area rectangle of the fruits
m_n|photo|counts|numeric|fruits|back end|Number of measurable fruits
n_fruits_ok|photo|counts|numeric|fruits|back end|Number of fruits whose outline passed the checks
sharp|photo|quality|numeric|variance|back end|Sharpness of the crop (variance of the Laplacian)
L|fruit|colour|numeric|L* (0-100)|R and back end|CIELAB L* of the fruit without its brightest 2% of pixels
a|fruit|colour|numeric|a*|R and back end|CIELAB a* of the fruit
b|fruit|colour|numeric|b*|R and back end|CIELAB b* of the fruit
C|fruit|colour|numeric|C*|R and back end|Chroma of the fruit
hue_deg|fruit|colour|circular|degrees|R and back end|Hue angle of the fruit
name|fruit|colour|categorical||R and back end|Colour name of the fruit
p_valid|fruit|classifier|probability|0-1|back end|Probability that the outline shows a single fruit with a correct outline
p_ripe|fruit|classifier|probability|0-1|back end|Probability that the fruit is ripe
p_dry|fruit|classifier|probability|0-1|back end|Probability that the fruit is dry (classifier not usable; reported for information)
', sep = "|", stringsAsFactors = FALSE, strip.white = TRUE)
v <- fr_validation
num <- def$trait[def$level == "photo" & def$type %in% c("numeric", "circular")]
rep_all <- fr_repeatability(v, traits = num, boot = 0)
rep_m <- fr_repeatability(v[v$both_measurable, ], traits = num, boot = 300, seed = 1)
agree_name <- function(col, rows) {
  d <- v[rows & !is.na(v[[col]]), c("occurrenceKey", col)]
  w <- split(d[[col]], d$occurrenceKey)
  w <- w[lengths(w) == 2L]
  c(share = mean(vapply(w, function(z) z[1] == z[2], logical(1))), n = length(w))
}
def$icc_all <- rep_all$icc[match(def$trait, rep_all$trait)]
def$icc_measurable <- rep_m$icc[match(def$trait, rep_m$trait)]
def$icc_lower <- rep_m$lower[match(def$trait, rep_m$trait)]
def$icc_upper <- rep_m$upper[match(def$trait, rep_m$trait)]
def$n_pairs <- rep_m$n_groups[match(def$trait, rep_m$trait)]
def$agreement <- NA_real_
for (k in c("m_name", "v_name")) {
  ag <- agree_name(k, v$both_measurable)
  def$agreement[def$trait == k] <- ag[["share"]]
  def$n_pairs[def$trait == k] <- ag[["n"]]
}
base <- c(m_L = "L", m_a = "a", m_b = "b", m_C = "C", m_hue = "hue_deg", v_L = "L", v_a = "a", v_b = "b",
          v_C = "C", v_hue = "hue_deg", L = "L", a = "a", b = "b", C = "C", hue_deg = "hue_deg")
photometric <- c("white_balance", "exposure", "jpeg", "blur")
geometric <- c("rotate", "perspective")
def$inv_photometric_min <- def$inv_geometric_min <- def$min_D_px <- def$icc_D32 <- NA_real_
if (!is.null(invariance)) {
  iv <- invariance
  rownames(iv) <- iv$trait
  for (i in seq_len(nrow(def))) {
    b <- base[def$trait[i]]
    if (is.na(b) || !b %in% rownames(iv)) next
    def$inv_photometric_min[i] <- min(unlist(iv[b, photometric]))
    def$inv_geometric_min[i] <- min(unlist(iv[b, geometric]))
    dcols <- grep("^D[0-9]+$", names(iv), value = TRUE)
    ok <- as.numeric(sub("D", "", dcols))[unlist(iv[b, dcols]) >= 0.8]
    def$min_D_px[i] <- if (length(ok)) min(ok) else NA_real_
    if ("D32" %in% dcols) def$icc_D32[i] <- iv[b, "D32"]
  }
}
# fruit-level colour traits take the repeatability of the photo-level mean of the measurable fruits
for (k in c("L", "a", "b", "C", "hue_deg")) {
  p <- c(L = "m_L", a = "m_a", b = "m_b", C = "m_C", hue_deg = "m_hue")[[k]]
  for (col in c("icc_all", "icc_measurable", "icc_lower", "icc_upper", "n_pairs")) {
    def[def$trait == k, col] <- def[def$trait == p, col]
  }
}
def$agreement[def$trait == "name"] <- def$agreement[def$trait == "m_name"]
def$n_pairs[def$trait == "name"] <- def$n_pairs[def$trait == "m_name"]
verdict <- function(r) {
  if (r$type == "probability") {
    h <- heads[heads$head == sub("p_", "", r$trait), ]
    if (isTRUE(h$usable)) return(c("reliable", sprintf("classifier: cross-validated AUC %.2f, balanced accuracy %.2f",
                                                        h$cv_auc, h$oof_balanced_accuracy)))
    return(c("not reliable", sprintf("classifier below the criteria: AUC %.2f, balanced accuracy %.2f",
                                     h$cv_auc, h$oof_balanced_accuracy)))
  }
  if (r$type == "categorical") {
    a <- r$agreement
    if (is.na(a)) return(c("not tested", "no paired photos"))
    if (a >= 0.8) return(c("reliable", sprintf("same name in %.0f%% of pairs", 100 * a)))
    if (a >= 0.6) return(c("use with care", sprintf("same name in %.0f%% of pairs", 100 * a)))
    return(c("not reliable", sprintf("same name in %.0f%% of pairs", 100 * a)))
  }
  icc <- r$icc_measurable
  inv <- r$inv_photometric_min
  d32 <- r$icc_D32
  if (is.na(icc)) return(c("not tested", "no paired photos"))
  txt <- sprintf("repeatability %.2f", icc)
  if (!is.na(inv)) txt <- sprintf("%s, photometric invariance %.2f, ICC at 32 px %.2f", txt, inv, d32)
  if (!is.na(inv) && icc >= 0.6 && inv >= 0.8 && !is.na(d32) && d32 >= 0.8) return(c("reliable", txt))
  if (icc >= 0.4 && (is.na(inv) || inv >= 0.7)) {
    return(c("use with care", if (is.na(inv)) paste0(txt, "; invariance not tested") else txt))
  }
  c("not reliable", txt)
}
vv <- t(vapply(seq_len(nrow(def)), function(i) verdict(def[i, ]), character(2)))
def$verdict <- factor(vv[, 1], levels = c("reliable", "use with care", "not reliable", "not tested"))
def$reason <- vv[, 2]
fr_catalogue <- def[, c("trait", "level", "family", "type", "unit", "verdict", "reason", "icc_measurable",
                        "icc_lower", "icc_upper", "icc_all", "n_pairs", "agreement", "inv_photometric_min",
                        "inv_geometric_min", "icc_D32", "min_D_px", "computed_by", "description")]
save(fr_catalogue, file = "data/fr_catalogue.rda", compress = "xz")
cat("fr_validation", nrow(fr_validation), "| fr_reference", nrow(fr_reference), "| outlines", nrow(outlines),
    "| catalogue", nrow(fr_catalogue), "| invariance", !is.null(invariance), "\n")
print(table(fr_catalogue$verdict))
