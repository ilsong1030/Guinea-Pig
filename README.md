-Correlation of social hierachy & stress reaction-

The endocrine stress response is a homeostatic system—often termed the “fight-or-flight” response—driven primarily by the HPA axis. 
When a vertebrate encounters a stressor, the HPA axis secretes corticosterone (or cortisol in many mammals), and circulating levels are subsequently regulated by negative feedback.
Ambient temperature can itself act as a stressor; higher outside temperatures are associated with elevated blood cortisol levels (Michael Vogeser & Josef Biregel, 2007). 
Guinea pigs used in this study exhibit a pronounced, stable dominance hierarchy, and hierarchy stability can influence rises in cortisol (Olivia Furtado et al., 2014). 
In rodents such as guinea pigs, both cortisol and body temperature increase acutely after stress exposure; however, by ~120 minutes post-exposure, body temperature can fall to ~2 °C below that of non-exposed controls (Jan G. Veening et al., 2004).
Steroid hormones and glucocorticoids are key determinants of dominance rank (M. Mutwill, 2021; Hau & Goymann, 2015). 
In particular, testosterone is linked to courtship, aggression, territoriality, and mating behaviors (M. Mutwill, 2021; Hirschenhauser & Oliveira, 2006). 
Stable guinea-pig societies combine complex social bonding with dominance structures (Sachser, 1986). Glucocorticoids principally modulate energy mobilization to tissues and dominance-related behavior (Koolhaas et al., 2011). 
In stable social systems, low-ranking individuals do not necessarily mount a strong endocrine stress response; with age, social competencies that enable consensual conflict resolution can develop through family-like social contact (Sachser, 1998). 
Social buffering—stress attenuation via conspecific presence—typically operates through bonded partners rather than any group member (Sachser, 1998).
In the present experiment, males are separated from females and thus lack social support. 
Under conditions where social buffering is unavailable, low-ranking individuals are expected to show more stress-related events (e.g., aggressive bouts), higher plasma cortisol, and, additionally, elevated body temperature compared with higher-ranking conspecifics.

Aim and data
	•	Aim: Examine how hierarchy index (Index), temperature, and cortisol relate within each week, and quantify consistency and level shifts across Week 1 vs Week 2.
	•	Tables:
	◦	data_1 (W1), data_2 (W2) with Index/Temperature/Cortisol
	◦	data_3 paired (Index_1/2, Cortisol_1/2)
	◦	data_R1, data_R2 rank-organized tables
Checks and preprocessing
	•	Prefer explicit data references over attach/detach to avoid name collisions.
	•	Summaries (summary, means/SDs) for sanity checks.
	•	Normality with shapiro.test(): applied to variables themselves or paired differences (W1−W2).
	◦	If normal ⇒ t-tests, Pearson correlation, linear regression.
	◦	If non-normal ⇒ Wilcoxon, Spearman/Kendall.
Analysis 1: Temperature–cortisol within each week
	•	Tests
	◦	After normality checks, run Pearson correlation and simple linear regression (Temperatur ~ Cortisol).
	◦	If a grouping factor (Alter) exists, compare temperature by groups via t-test.
	•	Plot: scatter with lm smoother (trend line).
	•	Why: Two continuous variables; Pearson/regression is standard under approximate linearity/normality. Use Spearman if normality is doubtful.
Analysis 2: Week-to-week consistency of hierarchy index
	•	Subset to individuals observed in both weeks.
	•	Tests
	◦	Spearman/Kendall to assess rank/monotone agreement.
	◦	Pairwise inversion rate: share of pairwise order reversals—an intuitive stability metric.
	◦	Level shift: normality of (Index_1 − Index_2) →
	▪	normal: paired t-test,
	▪	non-normal: Wilcoxon signed-rank.
	•	Plot: bars overlaid by week plus connecting lines/points per subject.
	•	Why: separates relative order (correlation/inversions) from mean-level change (paired test).
Analysis 3: Week-to-week consistency of cortisol
	•	Same strategy as for Index: Spearman/Kendall, inversion rate, paired test.
	•	Plot: bar overlays + connecting lines/points per subject.
Analysis 4: Week-wise links of rank/index with temperature/cortisol
	•	Week 1
	◦	Temperatur: normal ⇒ t.test(Temperatur ~ Rang); Index_1 vs Temperatur: Pearson.
	◦	Cortisol: non-normal ⇒ Wilcoxon by Dominanz; Index_1 vs Cortisol: Spearman.
	•	Week 2
	◦	Temperatur: normal ⇒ t.test(Temperatur ~ Rang);
	◦	Cortisol: normal ⇒ t.test(Cortisol ~ Dominanz); Index_2 vs Cortisol: Pearson.
	•	Plots:
	◦	Boxplots + points with stat_compare_means() to annotate p-values.
	◦	Scatter + lm smoother for continuous–index relationships.
Analysis 5: Correlation and paired comparison of Index (W1 vs W2)
	•	Plot: Index_1 vs Index_2 scatter, lm smoother, and y=x reference line.
	•	Tests: Pearson correlation (plus paired nonparametric alternative on levels).
Assumptions and extensions
	•	Even for t-tests/regression, inspect residual normality (Shapiro, QQ) and homoscedasticity (e.g., Levene).
	•	Robust alternatives: Spearman/Wilcoxon under violations.
	•	Effect sizes: report r, τ for associations; standardized effect for paired tests; Cohen’s d or Cliff’s delta for group differences.
Interpretation
	•	Spearman/Kendall capture relative order stability;
	•	Paired tests capture level shifts;
	•	Line plots emphasize within-subject trajectories across weeks.
