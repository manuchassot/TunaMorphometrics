
# ANOVA TABLE FUNCTION

anova_table = function(SpeciesName, LinearModel){

ANOVA_TABLE = data.table(anova(LinearModel), keep.rownames = TRUE)
ANOVA_TABLE[, "%var" :=  round(`Sum Sq`/sum(`Sum Sq`)*100, 3)]
ANOVA_TABLE[`Pr(>F)`<0.001, `Pr(>F) STD` := "<0.001"]
ANOVA_TABLE = ANOVA_TABLE[, -c("Sum Sq", "F value", "Pr(>F)")]
setnames(ANOVA_TABLE, new = c("Source of variation", "Df", "Mean squares", "Percentage of variation", "p-value"))
ANOVA_TABLE[, "Species" := SpeciesName]
setcolorder(ANOVA_TABLE, neworder = "Species")
return(ANOVA_TABLE)
}

