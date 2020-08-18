# monopoly

This project is an analysis of Monopoly by Monte Carlo simulation, Markov chain model, and as a network.

* monopoly_probs_final.R -- Probabilities of rolling two six-sided dice. Doubles are re-rolled, but if doubles are rolled three times the player goes to jail. This does not take into account Chance and Community Chest cards.
* monopoly_data_final.xlsx -- Contains tab with a transition matrix based on the dice rolls for calculations, and a tab with coordinates for a game board for plots. 
* monopoly monte carlo_final.R -- Monte Carlo simulation.
* monopoly_markov_final.R -- Uses the transition matrix in the XLSX file to calculate a Markov chain model.
* monopoly_network_final.R -- Uses the transition matrix in the XLSX file to build a network and calculate centrality.
* combined.png -- An image of the different analyses.
