# Internal data: CIE 1931 2-degree colour-matching functions and the D65 illuminant at 1 nm, 360-830 nm (CIE 15:2018
# tables as distributed with the 'colour-science' Python package; see cie1931_d65.csv), used by fr_spectra_to_lab().
cie <- utils::read.csv("data-raw/cie1931_d65.csv")
stopifnot(identical(names(cie), c("wavelength", "xbar", "ybar", "zbar", "D65")), nrow(cie) == 471L)
.fr_cie <- cie
save(.fr_cie, file = "R/sysdata.rda", compress = "xz")
