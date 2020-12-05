### Propensity scores are the probability that an observation is assigned to treatment based on the observed covariates
The scores can reduce selection bias and allow us to constrain inference to areas of common support (i.e. where the two groups overlap).

In order to illustrate the mechanism, let's first set treatment as either random or a function of the covariates (god view). Then, from the researcher's perspective, select which model family and independent variables to include in the calculation of the propensity scores.

The resulting distribution of should be approximately equal if treatment was assigned randomly and unequal if it is a function of the covariates. Unequal distributions indicate that the observations one group may have higher probability of being selected into treatment.

Lastly, we can use the propensity scores to match observations in the treatment group to observations in the control group. There are many ways to do this. Cycle through the available matching types to visualize how each each treatment observation is matched to a control observations.