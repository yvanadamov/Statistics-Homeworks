filename = "pokemon.csv"
pokemonData = read.csv(filename, header=TRUE)

f_nr = 71534
set.seed(f_nr)

indexes = sample(1:705, 600, replace=F)
subsample = subset(pokemonData, pokemonData$Number %in% indexes)