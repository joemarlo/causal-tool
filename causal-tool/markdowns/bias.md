### Bias....

Assume that a randomized control trial is designed to measure the effect of a drug on cholesterol levels. Smokers and non-smokers are included in the trial but their status is not accounted for in the analysis. How would this affect the results?

This is a randomized block design where the randomization occurs separately within the blocks. Often, there is an imbalance of the treatment to control observations and across the blocks and differing treatment effects.

The the regression method accounts for for the blocking variable but the SATE ignores it. Ignoring the imbalance of the smokers and imbalance of treatment assignment per smoker causes the SATE estimator to be biased. Try adjusting the conditional probability to see how the bias affects the distribution of the SATE estimates.

...
