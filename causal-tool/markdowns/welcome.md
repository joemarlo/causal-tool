# Learning causal inference

Causal inference is a topic that lends itself well to visualizing many of the core concepts. The goal of this webtool is to help build your intuition for key causal inference concepts via visualization. The tool attempts to follow a 'first principles' path by first describing the fundamental problem of causal inference and why randomization is often necessary. From there, visualizations of treatment effects, propensity scores, and regression discontinuity are included. All rely heavily on interaction, simulation, and visualization. The tool's components are independent of each other but they generally progress in complexity from left-to-right on the navigation bar.

It is not comprehensive. For more information on learning causal inference, try the following resources:
- [Regression and Other Stories](https://avehtari.github.io/ROS-Examples/)
- [Data Analysis Using Regression and Multilevel/Hierarchical Modesl](http://www.stat.columbia.edu/~gelman/arm/)
- [Nick Huntington-Klein's blog post illustrating causal inference](http://nickchk.com/causalgraphs.html)

The webtool is built for a desktop or laptop experience and works best on Safari or Chrome.

### Why is causal inference important?

"Correlation != causation" is a common refrain in statistics. We answer most research questions using advanced correlation measures but we *really* want to understand is the underlying causal mechanism. Does the vaccine reduce infection rates? Do higher taxes reduce inequality? Does an ad increase the number of customers? Update the plot on the right to see highly correlated relationships from real-life data.

***

The source code for this tool can be found on [Github](https://github.com/joemarlo/causal-tool).

<details><summary>Software thanks</summary>
<br>
The tool is made possible by the following software:
- **R**: R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
- **Shiny**:  Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny
- **tidyverse**: Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
- **arm**: Andrew Gelman and Yu-Sung Su (2018). arm: Data Analysis Using   Regression and Multilevel/Hierarchical Models. R package version 1.10-1. https://CRAN.R-project.org/package=arm
- **viridis**: Simon Garnier (2018). viridis: Default Color Maps from 'matplotlib'. R package version 0.5.1. https://CRAN.R-project.org/package=viridis
- **shinyjs**: Dean Attali (2020). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 1.1. https://CRAN.R-project.org/package=shinyjs
- **shinyWidgets**: Victor Perrier, Fanny Meyer and David Granjon (2020). shinyWidgets: Custom Inputs Widgets for Shiny. R package version 0.5.3. https://CRAN.R-project.org/package=shinyWidgets
- **kableExtra**: Hao Zhu (2019). kableExtra: Construct Complex Table with 'kable' and Pipe Syntax. R package version 1.1.0. https://CRAN.R-project.org/package=kableExtra
- **Archive Team**: http://textfiles.com/underconstruction/. An ode to the early web

</details><br>
