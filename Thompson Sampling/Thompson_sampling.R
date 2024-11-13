# Thompson Sampling

# Importing the dataset
dataset = read.csv('Ads_CTR_Optimisation.csv')

dim(dataset)
d = ncol(dataset) # Number of Column 'Ads'
N = nrow(dataset) # Number of Rows 'users'
# "L" suffix is for integer - R stores double by default

ad_selected <- integer(0)
numbers_of_rewards_1 <- integer(d)
numbers_of_rewards_0 <- integer(d)

total_reward = 0

for(n in 1:N){
  ad = 0
  max_random_beta = 0
  for(i in 1:d){
    random_beta = rbeta(n = 1, shape1 = numbers_of_rewards_1[i] + 1, shape2 = numbers_of_rewards_0[i] + 1) 
    # n --> number of random values to generate , shape1/shape2 --> a/b
    if(random_beta > max_random_beta){
      max_random_beta = random_beta
      ad = i
    }
  }
  ad_selected[n] <- ad # Record the selected ads
  # ad_selected = append(ad_selected, ad)
  reward = dataset[n, ad] # Get the reward for the selected ad
  if(reward == 1){
    numbers_of_rewards_1[ad] = numbers_of_rewards_1[ad] + 1
  }
  else{
    numbers_of_rewards_0[ad] = numbers_of_rewards_0[ad] + 1
  }
  total_reward = total_reward + reward
}

x_lim <- 0:10
# Visualising the Results
hist(ad_selected,
     main = paste('Histogram of ads selections'),
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected',
     col = 'green',
     xlim = range(x_lim))
axis(1, at = seq(1, 10, 1)) # xticks
