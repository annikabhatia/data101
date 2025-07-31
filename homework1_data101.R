getwd()

library(readr)

songs_normalize <- read_csv("songs_normalize.csv")
head(songs_normalize)
str(songs_normalize)

# Query 1: Categorize the valence of a songs into 3 categories: Low, Medium, High
songs_normalize$valence_category <- cut(songs_normalize$valence, breaks = c(0, 0.33, 0.66, 1), labels = c("Low", "Medium", "High"))

#Query 2: Find the number of songs in each valence category
valence_category_counts <- table(songs_normalize$valence_category)
valence_category_counts

#Plot 1: Number of Songs in Each Valence Category
colors <- c("orange red", "cyan", "spring green")
barplot(valence_category_counts, col = colors, main = "Number of Songs by Valence Category", xlab = "Valence", ylab = "Number of Songs")

# Query 3: Finding the average duration of songs (in minutes) with a popularity (a value of 60 and higher makes the song more popular) 
avg_song_duration <- songs_normalize$duration_ms[songs_normalize$popularity > 60] / 60000
mean(avg_song_duration)

# Query 4: Finding average song duration each year
avg_duration_per_year <- tapply(songs_normalize$duration_min, songs_normalize$year, mean)
avg_duration_per_year

# Plot 2: Box plot showing the average song duration each year (Query 4)
boxplot(duration_min ~ year, data = songs_normalize, main = "Boxplot of Song Durations by Year", xlab = "Year", ylab = "Duration (min)", col = "hot pink")


# Query 5: number of popular songs that are between 2 to 3 minutes
min_duration_1 <- 2
max_duration_1 <- 3
popular_songs_1 <- subset(songs_normalize, popularity & duration_min > min_duration_1 & duration_min < max_duration_1)
num_popular_songs_1 <- nrow(popular_songs_1)
num_popular_songs_1

# Query 6: number of popular songs that are between 3 to 4 minutes
min_duration_2 <- 3
max_duration_2 <- 4
popular_songs_2 <- subset(songs_normalize, popularity & duration_min > min_duration_2 & duration_min < max_duration_2)
num_popular_songs_2 <- nrow(popular_songs_2)
num_popular_songs_2

# Query 7: number of popular songs that are between 5 to 6 minutes
min_duration_3 <- 5
max_duration_3 <- 6
popular_songs_3 <- subset(songs_normalize, popularity & duration_min > min_duration_3 & duration_min < max_duration_3)
num_popular_songs_3 <- nrow(popular_songs_3)
num_popular_songs_3

# Plot #3: Count of Popular Songs by Duration range
duration_min_range <- c(num_popular_songs_1, num_popular_songs_2, num_popular_songs_3)
labels <- c("2-3 min", "3-4 min", "4-5 min")
barplot(duration_min_range, names.arg = NA, col = "red", main = "Count of Popular Songs by Duration Range", xlab = "Duration Range", ylab = "Number of Songs")
axis(1, at = 1:length(duration_min_range), labels = labels)


# Plot 4: Is high energy = high danceability?
plot(songs_normalize$energy, songs_normalize$danceability, ylab = "energy", xlab = "danceability", main = "Energy vs Danceability", col = "light slate blue")


# Query 8: What is the popularity of an explicit song vs a non-explicit song?
avg_pop_by_explicit <- tapply(songs_normalize$popularity, songs_normalize$explicit, mean, na.rm = TRUE)
avg_pop_by_explicit


# Query 9: How many popular songs were there per year?
popular_songs_per_year <- table(songs_normalize$year)
popular_songs_per_year

# Plot 5: Histogram of Query 9
hist(songs_normalize$year, breaks = 20, col = "dark green", border = "black", main = "Number of Popular Songs per Year", xlab = "Year", ylab = "Number of Songs", xlim = c(1998,2020), ylim = c(1, 120), freq = TRUE)


# Query 10: What are the popularity levels per genre?
avg_popularity_genre <- tapply(songs_normalize$popularity, songs_normalize$genre, mean, na.rm = TRUE)
avg_popularity_genre

# Plot 6: Box plot to show the most popular genre of music
boxplot(popularity ~ genre, data = songs_normalize, main = "Popularity by Genre", xlab = "Genre", ylab = "Popularity", col = "turquoise", border = "black")

#Query 11: What is the mean popularity?
mean_popularity_artists <- mean(songs_normalize$popularity, na.rm = TRUE)

# Query 12: What is the popularity for each of the artists in the datset?
avg_popularity_for_artists <- tapply(songs_normalize$popularity, songs_normalize$artist, mean, na.rm = TRUE)
avg_popularity_for_artists

# Query 13: Filtering out artists to make it easier to plot
most_popular_artists <- avg_popularity_for_artists[avg_popularity_for_artists > mean_popularity_artists]

#Plot 7: Creating a bar plot to find the most popular artist
par(mar = c(10, 4, 4, 2)) # to adjust margins to fit all the names on the x-axis, that way I do not need an x-lab
barplot(most_popular_artists, main = "Artists with Average Popularity Above the Mean", las = 2, ylab = "average popularity", col = "dark blue", border = "black")  


        


