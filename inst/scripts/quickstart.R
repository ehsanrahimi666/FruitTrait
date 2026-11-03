# Quick start: runs as-is with the files shipped in the package (no Python).
library(FruitTrait)

# Example photos (CC0 / CC BY 4.0), with the crops and fruit label maps written by the back end
ex <- fr_example_photos()
ex[, c("photo_id", "species", "family", "rights_holder", "license")]
fr_plot_fruits(ex$crop[1], ex$labels[1], main = ex$species[1])

# Colour of every outlined fruit, recomputed in R, and colour checks of the crops
fr_fruit_colour(ex$crop[1], ex$labels[1])
fr_colour_check(ex$crop, ex$labels)

# Back-end results: measurable fruits (single fruit, correct outline, ripe) and photo colour
res <- fr_example_results()
summary(res)
res[, c("photo_id", "species", "m_n", "m_L", "m_C", "m_hue", "m_name", "v_name")]
fr_fruits(res, "measurable")[, c("photo_id", "label", "p_valid", "p_ripe", "L", "a", "b", "name")]
fr_summarise(res, which = "outlined")
fr_contact_sheet(res, ncol = 4)

# Trait reliability
table(fr_catalogue$family, fr_catalogue$verdict)
fr_trait_info(verdict = c("use with care", "not reliable"))
fr_reliable_traits()

# Repeatability on the validation pairs (two photos of the same plant)
v <- fr_validation[fr_validation$both_measurable, ]
r <- fr_repeatability(v, traits = c("m_L", "m_a", "m_b", "m_C", "m_hue"), boot = 100)
fr_plot_repeatability(r)

# Invariance to camera changes and augmentation check on the example crops
fr_invariance(ex$crop, ex$labels, types = c("white_balance", "exposure", "jpeg", "blur"), rungs = 32, min_n = 5)
fr_augment_check(ex$crop[1], ex$labels[1])

# Classifier performance and decision thresholds on the rated benchmark fruits
fr_benchmark$heads
fr_benchmark$rules

# Photo colour against reflectance spectra of the same species
head(fr_reference[, c("species", "photos", "L", "C", "photo_name", "spectrum_colour", "spectral_saturation")])
