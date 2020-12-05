### Regression discontinuity
Regression discontinuity design is a special case of an observational study where treatment is assigned solely based on an X variable (in this example `Age`). The observations are split into groups based on a cutoff value, and treatment is assigned to one of these groups.

In this hypothetical example we're going to envision a drug study where participants are given the experimental drug **only** if they are over a certain age cutoff. The outcome variable is a general measure of `health`.

First, attempt to estimate the treatment effect between the two groups using the models available in the dropdown below. Adjust the bandwidth to include/exclude which data to include in your model. Use the play button on the right side of the bandwidth slider to visualize how models change with increasing bandwidths.

The 'Observable data' tab visualizes the data available to the researcher, and the 'All data' tab visualizes the data that a theoretical omniscient being who could see if the drug was simultaneously given to everyone and not given (i.e. seeing the observed data and the counterfactual).

Second, explore the data generating process to see how each model performs with a given set of assumptions.