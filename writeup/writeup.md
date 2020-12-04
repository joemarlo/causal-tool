# Interactive Shiny tool for learning causal inference
#### Joe Marlo, jpm770

## Introduction
The goal of the tool is to help users build intuition for key causal inference concepts via visualization. The tool attempts to follow a 'first principles' path by first describing the fundamental problem of causal inference and why randomization is often necessary. From there, visualizations of treatment effects, propensity scores, and regression discontinuity are included. All include interactive components and most invite the user to experiement with the data generating process to futher understand the concepts.

The tool is currently hosted on my Shinyapps.io server and can be accessed [here](https://jmarlo.shinyapps.io/causal-tool/). All code can be found on [Github](https://github.com/joemarlo/causal-tool) and is provided with a GNU General Public License v3.0 guaranteeing free public and private use and distribution.

<!-- insert homepage screenshot -->

## Fundamental problem of causal inference

The first page sets the foundation for why causal inference is necessary. It's illustrates a simple example of an experiment involving heart disease. Directed acyclic graphs (DAGs) help formalize the concepts visually. The conclusion of the page includes a mathematically formalization to baseline users in the notation.

## Randomization

The rationale for randomization can be easily overlooked in a high-level causal inference course so it is important to remind users why it is referred to as the gold standard. This page invites users to 'observe' two variables via plotting and then manually assign the observations to either control or treatment group. Univariate density plots of both observed and unobserved variables then populate and the resulting balance of the two groups can be visually compared. Users should then randomize the treatment groups using the button and then see how the two groups compare.

The underlying data is generated to include correlations so it is difficult to manually balance the groups. n is kept small so the manual assignment process does not become tedious. The correlations of the variables are:

|                    | Cholesterol LDL| Cholesterol HDL|   Age| Genetic disposition| Packs per day| Exercise per week|
|:-------------------|---------------:|---------------:|-----:|-------------------:|-------------:|-----------------:|
|Cholesterol LDL     |            1.00|            0.79|  0.44|                0.19|          0.52|             -0.45|
|Cholesterol HDL     |            0.79|            1.00|  0.46|                0.26|          0.56|             -0.42|
|Age                 |            0.44|            0.46|  1.00|               -0.03|          0.16|             -0.09|
|Genetic disposition |            0.19|            0.26| -0.03|                1.00|          0.01|             -0.06|
|Packs per day       |            0.52|            0.56|  0.16|                0.01|          1.00|             -0.46|
|Exercise per week   |           -0.45|           -0.42| -0.09|               -0.06|         -0.46|              1.00|


## Treatment effects

Treatment effects is broken into three pages.

### Estimating treatment effects

### Efficiency

### Bias

## Propensity scores and matching

Propensity scores page focuses on illustrating matching concepts. First, treatment can be assigned by the user either randomly or dependent on various variables. Second, the scores can be formulated using logit, probit, or GAM models. The density plots update to show how the propensity scores per the treatment and control groups compare. Most importantly, the illustration under the 'Matching' tab visualizes nearest neighbor (NN) and radius matching. The user can select with or without replacement for NN and caliper width for radius. The plot updates in real time so it is easy to see observations are being matched across the methods.

The underlying data is the same as the Randomization page.

![matching.png]

## Regression discontinuity



![screenshot.png]


## Conclusion

The original goal was to illustrate the basic concepts of causal inference. This was achieved but with some minor caveats. During the process, it became evident that the most important function learning many of the concepts is having the ability to tweak the underlying data generation process (DGP), similar to the simulation homework assignments. Combining the ability to control the DGP with animation is computationally expensive and programmatically challenging. As such, the DGP process was privileged over sleeker animations where necessary.

Many of the concepts still do illustrate well. The matching methods and the regression discontinuity design are particularly insightful and are standouts for pedagogical tasks.

### Next steps

- Assumptions
- Treatment effects
  - animate simulation so observations 'fall down'
- Observational study
  - new tab
  - overlap and balance
  - IPTW
- Regression discontinuity:
  - differing functional forms on left and right side of cutoff
  - differing bandwidth on left and right side of cutoff
- BART. Integrating George's simulation work


<!--
1-2 pages
explain why I chose to display what I displayed and why it makes sense
should add a DGP to regression discontinuity that is difficult to model (some crazy polynomial)
should add mean lines to the SATE animations
-->