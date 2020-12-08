### Benefits of regression estimates: efficiency

Regression results in much more efficient estimates. Playing statistics god, we can see how the observed outcomes and estimates change if we relabel the treatment assignment n amount of times. This is a randomization distribution. We then estimate SATE and the regression estimate for each of these n simulations. 

You can think of this simulation process as repetitively taking samples of the same dataset, but each time randomly assigning treatment to a new group of observations and estimating the treatment effect from the resulting groups.

The resulting plot shows how the regression estimate is unbiased for SATE and the variance of the distribution is much smaller (i.e. more efficient).

This dynamic holds with even as few as 50 simulations but it differs as slope and error changes. Play with data generation process settings to see how it affects the simulation results.
