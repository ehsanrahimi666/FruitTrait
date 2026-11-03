# Invariance, augmentation check and agreement between R and the back end on the benchmark crops.
#
# Rscript invariance_agreement.R <package dir> <out dir> <benchmark run dir> [<benchmark run dir> ...]
#
# Benchmark run dir: output folder of fruittrait.run_photos for the benchmark photos, with crops/<id>.jpg,
# crops/<id>_fruits.png (label map of the fruits that passed the outline checks) and fruits_*.csv.
# Uses the rated outlines (benchmark_outlines.csv in <out dir>). Writes invariance_icc.csv,
# invariance_long.csv, augmentation.csv and r_python.csv to <out dir>.
args <- commandArgs(trailingOnly = TRUE)
pkg <- args[1]
out <- args[2]
runs <- args[-(1:2)]
pkgload::load_all(pkg, quiet = TRUE)

ol <- utils::read.csv(file.path(out, "benchmark_outlines.csv"), colClasses = c(photo_id = "character"))
crop_of <- function(id) {
  for (r in runs) {
    f <- file.path(r, "crops", paste0(id, ".jpg"))
    if (file.exists(f)) return(c(f, file.path(r, "crops", paste0(id, "_fruits.png"))))
  }
  c(NA_character_, NA_character_)
}
ids <- unique(ol$photo_id)
paths <- t(vapply(ids, crop_of, character(2)))
have <- !is.na(paths[, 1]) & file.exists(paths[, 2])
ids <- ids[have]
paths <- paths[have, , drop = FALSE]

# R and back end on the same crops and label maps (all outlines that passed the checks)
fr <- do.call(rbind, lapply(unlist(lapply(runs, function(r) Sys.glob(file.path(r, "fruits_*.csv")))),
                            utils::read.csv, colClasses = c(photo_id = "character")))
fr <- fr[!duplicated(fr[, c("photo_id", "fruit")], fromLast = TRUE) & fr$ok %in% c(TRUE, "True"), ]
agree <- do.call(rbind, lapply(seq_along(ids), function(i) {
  r <- fr_fruit_colour(paths[i, 1], paths[i, 2], min_px = 1)
  w <- fr_fruit_colour(paths[i, 1], paths[i, 2], white_balance = "gray_world", min_px = 1)
  if (!nrow(r)) return(NULL)
  p <- fr[fr$photo_id == ids[i], ]
  m <- match(r$label, p$label)
  data.frame(photo_id = ids[i], label = r$label, px = r$px, L_r = r$L, a_r = r$a, b_r = r$b, name_r = r$name,
             wb_L_r = w$L[match(r$label, w$label)], wb_a_r = w$a[match(r$label, w$label)],
             wb_b_r = w$b[match(r$label, w$label)], L_py = p$L[m], a_py = p$a[m], b_py = p$b[m],
             name_py = p$name[m], wb_L_py = p$wb_L[m], wb_a_py = p$wb_a[m], wb_b_py = p$wb_b[m],
             stringsAsFactors = FALSE)
}))
utils::write.csv(agree, file.path(out, "r_python.csv"), row.names = FALSE)

# Invariance on crops with at least one fruit rated a single ripe fleshy fruit; other labels removed
fr_ripe <- ol[ol$cls == "F" & ol$stage %in% "R" & !is.na(ol$label), ]
sel <- intersect(ids, unique(fr_ripe$photo_id))
labs <- lapply(sel, function(id) {
  lab <- fr_read_labels(paths[match(id, ids), 2])
  keep <- fr_ripe$label[fr_ripe$photo_id == id]
  lab[!lab %in% keep] <- 0L
  lab
})
imgs <- lapply(sel, function(id) fr_read_image(paths[match(id, ids), 1]))
inv <- fr_invariance(imgs, labs, rungs = c(8, 16, 32, 64), seed = 1, min_n = 10)
icc <- data.frame(trait = rownames(inv), inv, row.names = NULL, check.names = FALSE)
utils::write.csv(icc, file.path(out, "invariance_icc.csv"), row.names = FALSE)
long <- attr(inv, "data")
long$photo_id <- sel[as.integer(sub(":.*", "", long$id))]
utils::write.csv(long, file.path(out, "invariance_long.csv"), row.names = FALSE)

# Augmentation check (one draw per augmentation) on the same crops
aug <- do.call(rbind, lapply(seq_along(sel), function(i) {
  a <- fr_augment_check(imgs[[i]], labs[[i]], seed = i)
  cbind(photo_id = sel[i], a, stringsAsFactors = FALSE)
}))
utils::write.csv(aug, file.path(out, "augmentation.csv"), row.names = FALSE)
cat("crops:", length(ids), "| agreement fruits:", nrow(agree), "| invariance crops:", length(sel),
    "| fruits:", length(unique(long$id)), "\n")
