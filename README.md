# Shiny_bc_paper
This repository that contains all the code for the [interactive Shiny app](https://v9cawl-aristeidis0sionakidis.shinyapps.io/shiny_bc_paper/) of our paper on predicting response to neoadjuvant treatment.

## App features 🪄
### Exploratory plot generation 📊
The app offers functionality for generating exploratory plots: **histograms** and **bar charts**. 

In both cases, all data from the project are included by default.
- ☑️ The user can use the checkbox input to include/exclude studies.
- 🖌️ Click *Draw!* to generate plot.
- ℹ️ Click *Info* for useful tips and instructions.
- 🔄 Click *Reset to default parameters* to bring all options back to default.
- ⬇️ The plots generated can be downloaded by hovering to the plot tools on the top right.

- **Histograms:** the user can select a variable of interest and choose the type of the histogram (classic/histogram of counts, probability, percentage).
  - 🌈 The user can also choose the bin fill and bin color,
  - #️⃣ as well as the number of bins for continuous variables.
- **Bar charts:** the user can select **up to two variables** for the bar charts. If a second variable is selected, then the user can also select the type of bar chart (grouped or stacked).
  - 🌈 Options for bin fill and outline color are also provided.
