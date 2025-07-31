getwd()

library(readr)
songs_normalize <- read_csv("songs_normalize.csv")
head(songs_normalize)
str(songs_normalize)

#HYPOTHESIS 1
songs_normalize$valence_category <- cut(songs_normalize$valence, breaks = c(0, 0.33, 0.66, 1), labels = c("Low", "Medium", "High"))

#Query 2 from HW 1 : Find the number of songs in each valence category
valence_category_counts <- table(songs_normalize$valence_category)
valence_category_counts

#Finding means for all valence categories
mean_low = mean(subset(songs_normalize, valence_category == "Low")$valence)
mean_medium = mean(subset(songs_normalize, valence_category == "Medium")$valence)
mean_high = mean(subset(songs_normalize, valence_category == "High")$valence)

#observed differences in means
observed_diff_low_medium <- mean_low - mean_medium
observed_diff_medium_high <- mean_medium - mean_high
observed_diff_low_high <- mean_low - mean_high

perm_test_low_medium <- permutation_test(songs_normalize, 'valence_category', 'valence', 10000, 'Low', 'Medium')
perm_test_medium_high <- permutation_test(songs_normalize, 'valence_category', 'valence', 10000, 'Medium', 'High')
perm_test_low_high <- permutation_test(songs_normalize, 'valence_category', 'valence', 10000, 'Low', 'High')

cat("P-value (Low vs Medium):", perm_test_low_medium)
cat("P-value (Medium vs High):", perm_test_medium_high)
cat("P-value (Low vs High):", perm_test_low_high)

#HYPOTHESIS 2

#conducting z-test and getting the p-value
result <- z_test_from_data(songs_normalize, "genre", "popularity", "R&B", "metal")

#HYPOTHESIS 3
songs_normalize$popularity_category <- cut(songs_normalize$popularity, breaks = c(0, 50, 100), labels = c("Lower", "Higher"))
mean_energy_by_category <- tapply(songs_normalize$energy, songs_normalize$popularity_category, mean)

observed_diff <- mean_energy_by_category["Higher"] - mean_energy_by_category["Lower"]

#conducting permutation test based on the popularity categories
perm_test_hyp2_result <- permutation_test(songs_normalize, 'popularity_category', 'energy', 10000, 'Higher', 'Lower')

cat("P-value from permutation test:", perm_test_hyp2_result)


# NARROW QUERY

#M
mean(songs_normalize$popularity)

#M0
mean(songs_normalize[songs_normalize$explicit == 'TRUE' & songs_normalize$artist == 'Britney Spears', ]$popularity)




