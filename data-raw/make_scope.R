# fr_scope: fleshy-fruited woody species (trees, shrubs and woody climbers), from the scope list of the FruitTrait
# data set (scope_species.csv, made by make_scope_lists.py from the World Checklist of Vascular Plants, the global
# woodiness database of Zanne et al. 2014, Sinnott-Armstrong et al. 2018 and 2021 and PalmTraits 1.0).
# Run from the package folder with the path of scope_species.csv.
args <- commandArgs(trailingOnly = TRUE)
s <- utils::read.csv(args[1], stringsAsFactors = FALSE, encoding = "UTF-8")
fr_scope <- data.frame(species = s$wcvp_name, family = s$family, woody_source = s$woody_source,
                       fleshy_source = s$fleshy_source, stringsAsFactors = FALSE)
fr_scope <- fr_scope[order(fr_scope$species), ]
rownames(fr_scope) <- NULL
stopifnot(!anyDuplicated(fr_scope$species))
save(fr_scope, file = "data/fr_scope.rda", compress = "xz")
