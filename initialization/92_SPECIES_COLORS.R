
# SPECIES COLORS
SPECIES_COL    = data.table(species_code_fao = c("BET", "SKJ", "YFT"), FILL = pal_jco(alpha = 0.6)(3), OUTLINE = darken(pal_jco(alpha = 0.6)(3), 0.2))
#SPECIES_COL    = data.table(species_code_fao = c("BET", "SKJ", "YFT"), FILL = pal_startrek(alpha = 0.6)(3), OUTLINE = darken(pal_startrek(alpha = 0.6)(3), 0.2))



# COLOR TESTS
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
#   geom_point(color = SPECIES_COL[species_code_fao == "BET", FILL]) +
#   theme_bw()
#   