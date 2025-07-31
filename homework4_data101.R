library(readr)
songs_normalize <- read.csv("songs_normalize.csv")

#from HW#1: categorizing valence categories into low, medium, and high
songs_normalize$valence_category <- cut(songs_normalize$valence, breaks = c(0, 0.33, 0.66, 1), labels = c("Low", "Medium", "High"))
valence_category_counts <- table(songs_normalize$valence_category)
valence_category_counts

#question: what are the odds that a song is explicit given its in the medium valence category? 

#observation (what we know) : there are songs that fall within the medium valence category 
#belief (what we do not know) : the song is explicit

#prior odds of a song being explicit (not taking valence category into account)
total_explicit <- sum(songs_normalize$explicit)
total_songs <- nrow(songs_normalize)
prior_odds <- round(total_explicit / (total_songs - total_explicit),2)
cat("Prior Odds:", prior_odds)
  

#true positive : song that is explicit in the "medium" valence category
true_positive <- round(nrow(songs_normalize[songs_normalize$explicit == TRUE & songs_normalize$valence_category == 'Medium',])/nrow(songs_normalize[songs_normalize$valence_category == 'Medium',]),2)
cat("The True Positive is equal to:",true_positive)
  
#false positive : song that is not explicit but is in the "medium" valence category
false_positive <- round(nrow(songs_normalize[songs_normalize$explicit == FALSE & songs_normalize$valence_category == 'Medium',])/nrow(songs_normalize[songs_normalize$valence_category == 'Medium',]),2)
cat("The False Positive is equal to:",false_positive)

#likelihood ratio
likelihood_ratio <- round((true_positive / false_positive),2)
cat("The Likelihood Ratio is equal to:",likelihood_ratio)
                          
#posterior odds
posterior_odds <- likelihood_ratio * prior_odds
cat("The Posterior Odds are equal to:",posterior_odds)

#finding k
k <- posterior_odds / prior_odds
k

#Part 2: Building a Contingency Table
contingency_table <- table(songs_normalize$explicit, songs_normalize$valence_category)
print(contingency_table)

#Getting the prior odds based on contingency table
cat("Observation is that the songs fall within the medium valence category , Belief is that the songs in this valence category are explicit:", "\n")
PriorProb<-sum(contingency_table["TRUE",])/sum(contingency_table[,])
PriorOdds <- PriorProb/(1-PriorProb)
cat("Prior Odds of an explicit song:", PriorOdds, "\n")

#T[Explicit, Medium] and T[Not Explicit, Low] = T[i,j] and T[k, l]
TruePositive <- contingency_table["TRUE", "Medium"] / sum(contingency_table["TRUE", ])
FalsePositive <- contingency_table["FALSE", "Low"] / sum(contingency_table["FALSE", ])
LikelihoodRatio<-TruePositive/FalsePositive
cat("How odds of the song being Explicit change if it's Medium valence:", LikelihoodRatio, "\n")

#This is how your odds of there being an explicit song change if it is within the medium valence category
PosteriorOdds <- LikelihoodRatio * PriorOdds
cat("Posterior Odds of the song being Explicit when it's Medium valence:", PosteriorOdds, "\n")
Posterior <- PosteriorOdds / (1 + PosteriorOdds)
cat("Posterior Probability:", Posterior, "\n")


