recipes <- list.files('data-raw/recipes', full.names = TRUE, pattern = '\\.[Rr]$')
for (f in recipes) source(f, local = TRUE)
