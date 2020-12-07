### How do we estimate the effect of the treatment?

Estimating individual level causal effects is difficult so we often focus on estimating the average causal effects between the treatment group and the control group. Average treatment effects (ATE) is just the mean difference in outcomes (E[Y(1)-Y(0)]). Randomization makes this possible as the groups should be balanced on observed and unobserved attributes. 

The most popular ATE estimand is the population average treatment effect (PATE) where all units in the population are included. In practice, its more realistic to estimate the sample average treatment effect (SATE) as this just includes the observations in our sample and that are observable.

Treatment effects can also be estimated using regression and is often used to achieve greater precision.