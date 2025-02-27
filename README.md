# WoodMetabolomics
R Codes for analysing mass spectra from JEOL AccuTOF.

The analysis codes all assume that you are working from RStudio with Quarto. See https://quarto.org/docs/get-started/hello/rstudio.html for assistance.
All functions are saved and sourced from a directory "R".

Step 1 assumes that file directory is setup that contains a folder for functions "R", a folder for mass spectra to be sourced from "spectra", a folder for binned and thresholded frequency plots "output", and a folder for preprocessing RF models "preds". 

An additional folder "RFmodels" can be used for Step 2. 

Files are built on a number of other packages including but not limited to:

References

Wickham H, François R, Henry L, Müller K (2023). dplyr: A Grammar of Data Manipulation. R package version 1.1.3.https://CRAN.R-project.org/package=dplyr

Wickham H (2019). conflicted: An Alternative Conflict Resolution Strategy. R package version 1.0.4.https://CRAN.R-project.org/package=conflicted

Kassambara A (2023). ggpubr: 'ggplot2' Based Publication Ready Plots. R package version 0.6.0.https://CRAN.R-project.org/package=ggpubr

Wickham H, Bryan J (2023). tidyverse: Easily Install and Load the 'Tidyverse'. R package version 2.0.0.https://CRAN.R-project.org/package=tidyverse

Hahsler M, Piekenbrock M, Doran D (2023). dbscan: Density-Based Spatial speciesing of Applications with Noise (DBSCAN) and Related Algorithms. R package version 1.1-11.https://CRAN.R-project.org/package=dbscan

Kuhn M (2023). caret: Classification and Regression Training. R package version 6.0-94.https://CRAN.R-project.org/package=caret

Liaw A, Wiener M (2002). Classification and Regression by randomForest. R News, 2(3), 18-22.https://CRAN.R-project.org/package=randomForest

Finch, K., Espinoza, E., Jones, F. A., & Cronn, R. (2017). Source identification of western Oregon Douglas‐fir wood cores using mass spectrometry and random forest classification. Applications in Plant Sciences, 5(5), 1600158

Xie Y (2023). knitr: A General-Purpose Package for Dynamic Report Generation in R. R package version 1.44. https://CRAN.R-project.org/package=knitr

Zhu H (2023). kableExtra: Construct Complex Table with 'kable' and Pipe Syntax. R package version 1.3.4. https://CRAN.R-project.org/package=kableExtra

