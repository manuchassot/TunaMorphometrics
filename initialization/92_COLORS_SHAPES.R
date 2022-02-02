
# SPECIES COLORS AND SHAPES
SPECIES_COL_SHAPE    = data.table(species_code_fao = c("BET", "YFT", "SKJ"), species = c("Bigeye tuna", "Yellowfin tuna", "Skipjack tuna"), FILL = pal_jco(alpha = 0.6)(3), OUTLINE = darken(pal_jco(alpha = 0.6)(3), 0.2), SHAPE = c(1, 2, 0)) # c(19, 17, 15))

# OCEAN COLORS AND SHAPES
OCEAN_COL_SHAPE    = data.table(ocean_code = c("AO", "IO"), ocean = c("Atlantic ocean", "Indian Ocean"), FILL = pal_simpsons(alpha = 0.6)(2), OUTLINE = darken(pal_simpsons(alpha = 0.6)(2), 0.2), SHAPE = c(19, 17))


# # COLOR TESTS
# n     = 10000
# xobs  = seq(20, 120, length.out = n)
# 
# # simulate response data
# a      = 1e-5
# b      = 3
# sdy    = 1
# errory = rnorm(n, 0, sdy)
# yobs = (a * xobs ^ b) +  errory
# 
# OBS = data.frame(FL = xobs, TW = yobs)
# 
# par(mar = c(4, 4, 0.5, 0.5), mfrow = c(2, 1))
# plot(OBS$FL, OBS$TW, xlab = "Fork length (cm)", ylab = "Total weight (kg)", pch = 19, cex = .8, las = 1, col = "blue")
# plot(OBS$FL, OBS$TW, xlab = "Fork length (cm)", ylab = "Total weight (kg)", pch = 19, cex = .8, las = 1, col = SPECIES_COL[species_code_fao == "BET", FILL])
# 
# ggplot(data = OBS, aes(x = FL, y = TW)) +
#   geom_point(color = SPECIES_COL[species_code_fao == "YFT", FILL]) +
#   theme_bw()
