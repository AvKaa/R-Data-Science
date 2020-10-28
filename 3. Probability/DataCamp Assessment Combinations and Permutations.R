# Exercise 5. Monte Carlo simulation for Celtics winning a game

# Create a Monte Carlo simulation to confirm your answer to the previous problem by estimating how frequently the Celtics win at least 1 of 4 games. Use B <- 10000 simulations.
# The provided sample code simulates a single series of four random games, simulated_games.

# Use the replicate function for B <- 10000 simulations of a four game series. The results of replicate should be stored to a variable named celtic_wins.
# Within each simulation, replicate the sample code to simulate a four-game series named simulated_games. Then, use the any function to indicate whether the four-game series contains at least one win for the Celtics. Perform these operations in two separate steps.
# Use the mean function on celtic_wins to find the proportion of simulations that contain at least one win for the Celtics out of four games.


# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)
# Create an object called `celtic_wins` that replicates two steps for B iterations: (1) generating a random four-game series `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for the Celtics.
celtic_wins <- replicate(B,{
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4));
  any(simulated_games == c("win"))
})
# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.
mean(celtic_wins)




