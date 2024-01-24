# Shiny_bc_paper
This repository that contains all the code for the [interactive Shiny app](https://v9cawl-aristeidis0sionakidis.shinyapps.io/shiny_bc_paper/) of our paper on predicting response to neoadjuvant treatment.

## App features 🪄
### Exploratory plot generation 📊
The app offers functionality for generating exploratory plots: **histograms** and **bar charts**. In both cases, all data from the project are included by default.

- ☑️ The user can use the checkbox input to include/exclude studies.
- 🖌️ Click *Draw!* to generate plot.
- ℹ️ Click *Info* for useful tips and instructions.
- 🔄 Click *Reset to default parameters* to bring all options back to default.
- ⬇️ The plots generated can be downloaded by hovering to the plot tools on the top right.

**Histograms:** the user can select a variable of interest and choose the type of the histogram (classic/histogram of counts, probability, percentage).
- 🌈 The user can also choose the bin fill and bin color, as well as
- #️⃣ the number of bins (**for continuous variables only**).

**Bar charts:** the user can select **up to two variables** for the bar charts. If a second variable is selected, then the user can also select the type of bar chart (grouped or stacked).
- 🌈 Options for bin fill and outline color are also provided.
---
### Sunburst plots 🌞
Dynamic and interactive plots to illustrate the distribution of **up to four** categorical variables in our data. Colors are preselected. Choose a root variable and add up to three more variables to produce an interactive sunburst plot.
- 🖌️ Click *Draw!* to generate plot.
- ℹ️ Click *Info* for useful tips and instructions.
- 🔄 Click *Reset to default parameters* to bring all options back to default.
- 🎚️ Choose color opacity using the slider.

**Analytical sunbursts**

These plots have access to all the data in our study (training, validation, test, external validation). You can use these plots to examine the nested distributions of subtypes, risk scores, response to treatment and more, within the different studies. 

**Consensus sunbursts**

These plots can be used to illustrate the distribution of different variables within the clusters (Neoadjuvant treatment - NAT response subtypes) we derived.
- 🟦 *Cluster 1*: NAT-responsive subtype; associated with more favorable profiles.
- 🟧 *Cluster 2*: NAT-neutral subtype; associated with higher risk scores and subtypes with poor prognosis.
---
### Volcano plot 🌋
Use the results from our differential expression analysis, to create customized volcano plots.
<details>
  <summary>❓ What is a volcano plot?</summary>
  A volcano plot is typically used to illustrate results of differential expression analysis. It is a scatter plot, where each point represents a <em>gene</em>. The $y$-axis contains the negative base-10 logarithm of the gene's differential expression $p$-value, i.e. the <em>higher</em> a point is in the plot, the <em>lower</em> its $p$-value. The $x$-axis shows differential expression. It's usually centered at 0 and points right from 0, represent <em>up</em>-regulated genes and points left from 0, represent <em>down</em>-regulated genes.
</details>
