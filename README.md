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
  A volcano plot is typically used to illustrate results of differential expression analysis. It is a scatter plot, where each point represents a <em>gene</em>. 
  
  The $y$-axis contains the negative base-10 logarithm of the gene's differential expression $p$-value ($-\log_{10}p$), i.e. the <em>higher</em> a point is in the plot, the <em>lower</em> its $p$-value. 
  
  The $x$-axis shows differential expression (usually log<sub>2</sub>FoldChange). It's usually centered at 0; points right from 0 represent <em>up</em>-regulated genes; points left from 0, represent <em>down</em>-regulated genes.
</details>

**Note:** In our case, differential expression analysis was performed on standardized data. Instead of $\log_{}FoldChange$, differential expression here is defined as the numerical difference between the model coefficient responders and the model coefficient for non-responders. The (full) model for each gene used in our case is:

$$
\begin{align*}
\hat{y_{g}} &= a_1 \cdot 1_{\{Response = responder\}} + a_2 \cdot 1_{\{Response = non-responder\}} + \\
&\quad b_1 \cdot 1_{\{pam50 = Luminal A\}} + b_2 \cdot 1_{\{pam50 = Luminal B\}} + \\
&\quad b_3 \cdot 1_{\{pam50 = HER2+\}} + b_4 \cdot 1_{\{pam50 = Normal-like\}} + \\
&\quad c_1 \cdot 1_{\{timepoint = T2\}} + d_1 \cdot 1_{\{study = study_2\}} + \dots + \\
&\quad d_9 \cdot 1_{\{study = study_{10}\}}
\end{align*}
$$

The user can pick:
- 🗒️ which type of $p$-value to plot (adjusted/unadjusted; we suggest the *adjusted* $p$-value)
- 🛑 a differential expression threshold (DET) which will draw two vertical dashed lines at the positive and negative coordinates of the selected value on the $x$-axis
- 🛑 a $p$-value threshold (PVT) lower than which a gene's differential expression is considered statistically significant (draws a single horizontal line)
- 🌈 colors for:
    - ❌ a) non-significant genes,
    - 🟠 b) significant genes that don't pass the DET,
    - 🔵 c) down-regulated genes (pass both DET - left side - and PVT) and
    - 🔴 d) up-regulated genes (pass both DET - right side - and PVT)
- 🎚️ opacity for the color of the points

<details>
  <summary>
    <b>Short demo</b>
  </summary>
  
  ![Short demo](https://github.com/sionaris/Shiny_bc_paper/blob/main/GIFs/volcano_gif.gif)
</details>
