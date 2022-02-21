
# ANOVA TABLE FUNCTION

anova_table = function(SpeciesName, LinearModel, MeanSquaresDigits = 3){

ANOVA_TABLE = data.table(anova(LinearModel), keep.rownames = TRUE)
ANOVA_TABLE[, `Mean Sq` := round(`Mean Sq`, MeanSquaresDigits)]
ANOVA_TABLE[, `F value` := round(`F value`, 1)]
ANOVA_TABLE[, "%var" :=  round(`Sum Sq`/sum(`Sum Sq`)*100, 1)]
ANOVA_TABLE[, `Pr(>F) STD` := fifelse(`Pr(>F)`<0.001, "<0.001", fifelse(`Pr(>F)`<0.05, "<0.05", as.character(`Pr(>F)`)))]
ANOVA_TABLE = ANOVA_TABLE[, -c("Sum Sq", "Pr(>F)")]
setnames(ANOVA_TABLE, new = c("Source of variation", "Df", "Mean squares", "F statistic", "Percentage of variation", "p-value"))
ANOVA_TABLE[, "Species" := SpeciesName]

setcolorder(ANOVA_TABLE, neworder = c("Species", "Source of variation", "Df", "Mean squares", "Percentage of variation", "F statistic", "p-value"))

return(ANOVA_TABLE)
}

