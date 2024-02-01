# Shiny app for my PhD's breast cancer project
This repository that contains all the code for the [interactive Shiny app](https://v9cawl-aristeidis0sionakidis.shinyapps.io/shiny_bc_paper/) of our paper on predicting response to neoadjuvant treatment.

## App features 💡
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
<!--><a name="volcano_anchor"></a>
### Volcano plot 🌋
Use the results from our differential expression analysis, to create customized volcano plots.
<details>
  <summary>❓ What is a volcano plot?</summary>
  A volcano plot is typically used to illustrate results of differential expression analysis. It is a scatter plot, where each point represents a <em>gene</em>. 
  
  The $y$-axis contains the negative base-10 logarithm of the gene's differential expression $p$-value ($-\log_{10}p$), i.e. the <em>higher</em> a point is in the plot, the <em>lower</em> its $p$-value. 
  
  The $x$-axis shows differential expression (usually log<sub>2</sub>FoldChange). It's usually centered at 0; points right from 0 represent <em>up</em>-regulated genes; points left from 0, represent <em>down</em>-regulated genes.
</details>

> **_Note on x-axis:_** In our case, differential expression analysis was performed on standardized data. Instead of $\log_{}FoldChange$, differential expression here is defined as the numerical difference between the model coefficient for responders and the model coefficient for non-responders. The (full) model for each gene used in our case is:
> 
> $$
> \begin{align*}
> \hat{y_{g}} &= a_1 \cdot 1_{\{Response = responder\}} + a_2 \cdot 1_{\{Response = non-responder\}} + \\
> &\quad b_1 \cdot 1_{\{pam50 = Luminal A\}} + b_2 \cdot 1_{\{pam50 = Luminal B\}} + b_3 \cdot 1_{\{pam50 = HER2+\}} + b_4 \cdot 1_{\{pam50 = Normal-like\}} + \\
> &\quad c_1 \cdot 1_{\{timepoint = T2\}} + \\
> &\quad d_1 \cdot 1_{\{study = study_2\}} + \dots + d_9 \cdot 1_{\{study = study_{10}\}}
> \end{align*}
> $$
> 
> and the differential expression value for each gene is:
> 
> $$DE = a_1 - a_2$$

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

---

### Customized Differential Gene Expression Analysis 📈
Use the training and validation samples as input to perform customized DGEA. There are three tabs available:
- 🗒️ **Data Selection**: select the studies you want to keep for your analysis
- 🧮 **Filtering and Adjustments**:
   - Filter for clinical, demographic and risk score variable of interest
   - Select covariates for the linear models that will be fit for each gene
   - Pick the contrast variable of interest (the variable whose levels you want to compare)
   - Select the levels of interest (e.g. responder and non-responder). The kdifferential expression results represent the numerical difference between the model coefficients for level 1 and level 2:
     $$DE = coef_{level_{1}} - coef_{level_{2}}$$
- 🌋 **Plot Settings**: choose colors and text for the volcano plot that will be produced (see the [Volcano plot](#volcano_anchor) section for more details)

🔍 Click *Analyse* to perform the analysis and produce the table of results and the volcano plot. Both are downloadable.

🔄 Click *Default settings* to set everything back to default (applies to all tabs).

ℹ️ Click *Info* for useful tips and instructions

<details>
  <summary>
    <b>Short demo</b>
  </summary>
  ![Short demo](insert_gif)
</details>

---

### Machine Learning 🔮
A tab that allows the user to compare the performance of **up to 3** pre-trained models on the full data and subets selected interactively.

There are two boxes for each model:

🧰 **Box 1**

Select a model from the following categories:

| 📉 Logistic regression | 🌳 Decision Trees | 🤖 Support Vector Machines |
|     :---:      |     :---:      |     :---:      |
| Backward Logistic Regression | C5.0 (optimized with Cohen's *kappa*) | Linear kernel |
| Regularised Logistic Regression | C5.0 (optimized with ROC) | L2-regularised linear kernel |
|     | Random Forest (optimized with Cohen's *kappa*) | Radial Basis Function kernel |
|     | Random Forest (optimized with Cohen's ROC) |     |
|     | Boosting |     |
|     | Bagging (x100) |     |

and a subset of studies of interest (or keep data from all studies - default option).

🧰 **Box 2**

- 🧮 Filter the subset for clinical, demographic and risk score variables of interest
- 📛 Pick a name for your model (e.g. C5.0 model or C5.0 model for ER- subset, etc.)
- ➕ **Click *Add comparison* if you want to add another model for comparison**:
    - An additional set of boxes will appear
    - Choose model and studies of interest
    - Filter subset if desirable
    - Name the model and **click *Apply* to enable the comparison with the first model**
    - If you want a third model, **click *Add comparison* and follow the same steps. Don't forget to click *Apply* in the end!**
- ❌ Click *Remove* if you want to remove a comparison you added
- 🔮 Click *Predict!* (after applying additional comparisons - if any) to generate predictions of response to treatment and produce a ROC curve
- 🔄 Click *Default filters* to set all filters back to default in Box 2
- ℹ️ Click *Info* to see useful tips and instructions

**Output**
- ROC curve(s) for the selected model(s) with info on the record AUC values. Downloadable.
- Table of error metrics for each model used.
