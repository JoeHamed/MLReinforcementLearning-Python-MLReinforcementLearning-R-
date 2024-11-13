# Upper Confidence Bound

# Import the dataset
dataset = read.csv('Ads_CTR_Optimisation.csv')

dim(dataset)
d = ncol(dataset) # Number of Column 'Ads'
N = nrow(dataset) # Number of Rows 'users'
# "L" suffix is for integer - R stores double by default

ads_selected <- vector() # Preallocate a vector for selected ads
# ads_selected <- integer(0)
number_of_selections <- rep(0, d) # Track the number of selections per ad
sum_of_rewards <- rep(0, d) # Track the total reward for each ad

total_reward = 0 # Total reward initialized to zero

for (n in 1:N){ # Loop through each round
  ad = 0
  max_upper_bound = 0
  for (i in 1:d){ # Loop through each ad
    if(number_of_selections[i] > 0){
    average_reward = sum_of_rewards[i] / number_of_selections[i]
    delta_i = sqrt(3/2 * log(n) / number_of_selections[i])
    upper_bound = average_reward + delta_i
    } else {
      upper_bound = 10^400 #1e400
    }
    # Find the ad with the highest upper bound
    if (upper_bound > max_upper_bound){
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected[n] <- ad # Record the selected ad
  number_of_selections[ad] = number_of_selections[ad] + 1 # Update the number of selections for the selected ad
  reward = dataset[n, ad] # Get the reward for the selected ad
  # Update the total reward and sum of rewards for the selected ad
  sum_of_rewards[ad] = sum_of_rewards[ad] + reward
  total_reward = total_reward + reward
}

x_lim <- 0:10
# Visualising the Results
hist(ads_selected,
     main = paste('Histogram of ads selections'),
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected',
     col = 'orange',
     xlim = range(x_lim))
axis(1, at = seq(1, 10, 1)) # xticks


