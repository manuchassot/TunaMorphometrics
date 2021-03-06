
# Combine review data set of parameters with new estimates

FL_RD_PARAMS = rbindlist(list(FL_RD_PARAMS_REVIEW, FL_RD_PARAMS_ESTIMATED))
FL_RD_PARAMS[Origin != "THIS STUDY" , CombinedSource := "Literature review"]
FL_RD_PARAMS[Origin == "THIS STUDY",  CombinedSource := "This Study"]

# Factorize and order the species
FL_RD_PARAMS[, SpeciesName := factor(SpeciesName, levels = c("Bigeye tuna", "Yellowfin tuna", "Skipjack tuna"), ordered = TRUE)]

FL_RD_PARAMS_TABLE_STUDY = FL_RD_PARAMS[CombinedSource != "Literature review", .(Ocean, SpeciesCode, SpeciesName, Sex, SampleSize, ForkLengthMin, ForkLengthMax, a, b)][order(Ocean, -SpeciesName)]

FL_RD_PARAMS_TABLE_STUDY_FT =
  FL_RD_PARAMS_TABLE_STUDY %>%
  flextable() %>%
  set_header_labels(SpeciesCode = "Species code", SpeciesName = "Species name", SampleSize = "Sample size") %>%
  align(j = c("a", "b"), part = "header", align = "center") %>%
  hline(i = 3) %>%
  autofit()

# Statistics
#FL_RD_PARAMS[, as.list(summary(a*1e5)), keyby = .(SpeciesName, SpeciesCode)]
#FL_RD_PARAMS[, as.list(summary(b)), keyby = .(SpeciesName, SpeciesCode)]

FL_RD_PARAMS_ab_BIPLOT =
  ggplot(FL_RD_PARAMS, aes(x = a*1e5, y = b, color = SpeciesName, shape = CombinedSource)) +
  geom_point(size = 2.5) +
  scale_shape_manual(values = SPECIES_COL_SHAPE$SHAPE, guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = SPECIES_COL_SHAPE$FILL, guide = guide_legend(reverse = TRUE)) +
  labs(x = expression(paste("a x 1", e^{-5})), y = "b") +
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank())

ggsave(filename = "../outputs/charts/REVIEW/FL_RD_PARAMS_ab_BIPLOT.png", plot = FL_RD_PARAMS_ab_BIPLOT, width = 8, height = 6)

# Marginal distributions

FL_RD_PARAMS_a_PLOT =
  ggplot(FL_RD_PARAMS, aes(x = a*1e5, y = SpeciesName, fill = SpeciesName)) +
  scale_fill_manual(values = SPECIES_COL_SHAPE$FILL) +
  geom_boxplot() +
  theme_bw() +
  labs(x = expression(paste("a x 1", e^{-5})), y = "") +
  theme(legend.position = "none")

FL_RD_PARAMS_b_PLOT =
  ggplot(FL_RD_PARAMS, aes(x = b, y = SpeciesName, fill = SpeciesName)) +
  scale_fill_manual(values = SPECIES_COL_SHAPE$FILL) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "b", y = "") +
  theme(legend.position = "none")

FL_RD_PARAMS_ab_PLOT  = FL_RD_PARAMS_a_PLOT + FL_RD_PARAMS_b_PLOT

FL_RD_PARAMS_ab_PLOT[[2]] = FL_RD_PARAMS_ab_PLOT[[2]] + theme(axis.text.y = element_blank(),
                                                              axis.ticks.y = element_blank(),
                                                              axis.title.y = element_blank() )

FL_RD_PARAMS_ab_PLOTS = FL_RD_PARAMS_ab_BIPLOT / FL_RD_PARAMS_ab_PLOT

ggsave(filename = "../outputs/charts/REVIEW/FL_RD_PARAMS_ab_MARGINAL_PLOTS.png", plot = FL_RD_PARAMS_ab_PLOT, width = 8, height = 4.5)
ggsave(filename = "../outputs/charts/REVIEW/FL_RD_PARAMS_ab_PLOTS.png", plot = FL_RD_PARAMS_ab_PLOTS, width = 8, height = 9)
