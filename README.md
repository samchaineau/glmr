# Why this package ?

Coming from a background of economics, I have been working a lot with econometrics and modelling. Although the process is very interesting, we all faced the same pain point: time. Finding the right variable to put in your models or the right features might take time as it is a try and re-do process. I call this stage "modelling exploration". Exploring models is crucial as we want to find both the most relevant and accurate one. Machines can definitely support us in this task by simulating a great number of models based on a set of variables. 

An R package called "olsrr" attempted to improve this exploring part by the past. It is a package dedicated to simulate a great number of models and extract the particular relevant information from it. After using it, I found two particular pain points that would need to be improved.

The first one is the timing. To model 4095 differents equations, "olsrr" would take between 4 and 5 minutes. Imagine that you have more than 10 variables to model, you have at least one hour of computing time.
The second one is the lack of modeling options. "olsrr" is made only for ordinary least-squared regressions, removing the possibilities of exploring other GLMs. 

"glmr" is a package that improve those parts and offers different tools that facilitates your data processing part and models' filtering by including intuitive features.  

# For who ?

I created "glmr" for data scientists from every backgrounds. After working with R for one year and python for 4 months, I realised that th most important compnent of a package is the user experience offered. I tried to wok a lot on this aspect. Hence, the package has a very intuitive structure as every functions work together (but you can still use them one by one). It should be easy to understand by the broad majority of R users, even the freshers. 

I would say that the best way to use the package is as follow: 

stationarity_check() -> stationarize() -> variables_selection() -> models_creation() -> ols_all() or glm_all() -> overview()

#### Warning:
##### As mentionned in the Overview() part, it is sometimes preferable to use Overview() directly on ols_all() or glm_all() to avoid the creation of a big list of models in your local memory. So Overview() and glm_all() or ols_all() are linked.


The package has a function made to check the stationarity and apply a technique to stationarize the dataset. This feature would be more useful for economists or people working with time-series analysis. 

## Real exemple

I created a simple shiny application that shows you the power of such package and how quick it can be. Even though the application has some limits, it is a good and concrete example.

Here is the link: https://samchaineau.shinyapps.io/test/

# How to use it ? 

Here under is displayed a classic example of how to apply this package on a concrete dataset. I will use the dataset diamonds for the example.

## Loading the packages and the datasets
```{r message=FALSE,warning=FALSE, echo=FALSE}
rm(list=ls())
```

```{r message=FALSE}
if(!require("tidyverse"))install.packages("tidyverse") #The package is backed by some tidyverse functions
if(!require("devtools"))install.packages("devtools") #devtools is used to download the package from Github 
library(devtools) #Load devtools
install_github("samchaineau/glmr") #Uses the function install_github() to install "glmr"

library(tidyverse) #Load tidyverse
library(glmr) #Load glmr

head(diamonds)

load("/Users/samuelchaineau/Desktop/R/R_files/economics.RData")

#This dataset is commonly used as examples in data science introduction
```

## stationarity check()

The function stationarity_check() apply an Augmented Dicker-Fuller Test on every variables and yields you a list of "Stationary" and "Not Stationary" elements based on the p-values of the test. It removes automatically non numeric variables from the analysis. 
```{r message=FALSE, warning=FALSE}
Check <- stationarity_Check(diamonds) #The function tests my dataset diamonds, you can use the default parameters (k = 1, pval = 0.05)

Check <- stationarity_Check(diamonds, k = 3, pval = 0.1) #Or you can tailor your parameters and define your proper ADF test

Check$Stationary #A list of variables' names and their Augmented Dicker-Fuller test p-value

Check$Not_Stationary #A list of variables' names and their Augmented Dicker-Fuller test p-value
```

Let's do a second check on a very simple economic dataset. We can see that the results differ significantly and that there is a need for stationarizing our dataset.
```{r}
Check <- stationarity_Check(economics)

Check$Stationary

Check$Not_Stationary 
```


## stationarize()

The function stationarize() is made to (indeed) stationarize a dataset with a given method and re-checking if the output is stationary. The methods offered are: "Log", "Square", "Difference" and "Division". You can write the methods as you want (e.g "DIFFERENCE" or "DIfferEnce") as they will be put tolower(). The function displays a message with the number of variables that are stationary according to an ADF test after the process. 

```{r message=FALSE, warning=FALSE}
stationarize(economics, Variables = Check$Not_Stationary$Variable, Method = "Difference", message = FALSE)

#Define my dataset, my targeted variables and my method. If you want to apply it on every variables just use name(x) as Variables
```

## variables_selection()

The function variables_selection() shortlists the best regressors from your dataset based on their correlation scores with the targeted variable. You define the number of shortlisted regressors that you want. The function automatically excludes non numeric variables from the analysis and displays a message. 

You might use the caret package to mutate some categorical features in dummies variables.

```{r}
Target <- "carat"
Selected_variables <- variables_selection(diamonds, Target = Target, n = 4) #You define your targeted variable and "n" which is the number of shortlisted regressors

Selected_variables #The output is a dataframe containing the correlation score and the variables' names. 
```

## models_creation()

The function models_creation() creates a single column dataframe with every formulas possible from supplied variables (from the output of variables_selection() ). You can define an upper limit to the number of regressors within a model (a model with too much predictors would be a bit tricky to understand). Eventually, you can insert models with no intercept by setting up NoIntercept paramater to "TRUE". 

```{r}
Target<- "carat" #I still target "carat"
Models_created <- models_creation(Selected_variables$Variables, Target = Target, limit = 3, NoIntercept = TRUE) #I create models with a maximum of 3 regressors and I would like to have also their No Intercept formulas.
Models_created
```

## ols_all() and glm_all()

These functions create directly every models and store them into a list. For ols_all() you just need to supply every formulas. The argument "pval" removes models with at least one regressor with a p-value greater than 0.05. 

As you can see under, two models have been removed when pval = TRUE. 

```{r}
OLS <- ols_all(Models_created$Models, data = diamonds, pval = FALSE)
length(OLS)


OLS <- ols_all(Models_created$Models, data = diamonds, pval = TRUE)
length(OLS)
```

For glm_all(), you need to supply your formulas and specify the family that you use. For the moment I will use a gaussian one (which is an OLS). 

```{r}
GLM <- glm_all(Models_created$Models, ChosenFamily = gaussian, data = diamonds, pval = FALSE)
```


## overview()

The function overview() aims at summarizing every models in one dataframe. It yields a 5 columns dataframe with "n" the number of predictor, the formula, the r.squared, the adj.rsquared and the AIC. 

I strongly advise to use the second methodlogy when you want to simulate a high number of models (more than 500) in order to save your memory space. Otherwise, you would end up with a list of x models which is very heavy to store. 

```{r, message=FALSE}
head(overview(OLS), 5) #Overview can be used on the object containing your models when you have a low amount of models to store

head(overview(ols_all(Models_created$Models, data = diamonds, pval = FALSE)), 5) #You can use it directly on a formula ols_all() or glm_all() to avoid a big object in your environment
```

The two methods yield excatly the same results.

# Performance evaluation on OLS

Here under is displayed a timing comparison between glmr and Olsrr. glmr outperforms Olsrr by far and especially with a large number of simulated models. The vectorial nature of R enables us to be faster than a for loop. 

```{r, echo = FALSE}
library(readxl)
ModvsOlsrr <- read_excel("~/Desktop/ModvsOlsrr.xlsx")

ggplot(ModvsOlsrr) + geom_line(aes(x= `Number of Models`, y = Seconds, color = Package )) + ggtitle("glmr vs Olsrr")
```

# Performance evaluation on GLM 

The innovation in this package is the possibility to simulate Generalized Linear Models. 
However, the computig time may differ significantly from a gaussian family to a poisson or an inverse-gaussian. Hence, it is very important to carefully select your distribution and try to filter as much as possible your list of supplied models. 

To demonstrate this, I used a dataset made of 14 variables. 11 of them are generated with a runif(), one with rgamma(), one with rnorm and one with with rlnorm(). Those last three will be used as "Target" when computing models. 

For each target, I created a set of formulas from 1 regressor to 12 and computed each time a glm_all() with the appropriate ChosenFamily and an ols_all(). 

Here under are displayed my findings. 


```{r, echo=FALSE, warning=FALSE, message=FALSE}
ComparisonDistribution <- read_csv("~/Desktop/EndComparisonDistribution.csv")

ForGLM <- filter(ComparisonDistribution, `Type of models` == "glm")
ForOLS <- filter(ComparisonDistribution, `Type of models` == "ols")

ggplot(ForGLM) + geom_line(aes(x = `Number of Models`, y = Seconds, color = Distribution)) + ggtitle("Time comparison for GLM between distributions")
```

Gamma and gaussian families never reached a timing greater than 25 seconds. However, when we compute models for poisson distribution, we can see a huge increase in computing time with more than 100 seconds for 4095 models. 

Hence, even if the package is dedicated to improve computing time, some specific cases might take time. 


```{r, echo=FALSE, warning=FALSE, message=FALSE}
ggplot(ForOLS) + geom_line(aes(x = `Number of Models`, y = Seconds, color = Distribution))+ ggtitle("Time comparison for OLS between distributions")
```

On the OLS side, we don't have particular trends. The computing time increases proportionally with the number of models that you use as an input. Even with 4095 models, we never reached more than 6.4 seconds. 

## Conclusion: 

The package is available on my Github page. I will do as much as possible to keep it updated. If you would like to do it by yourself, feel free to message me to see what you have in mind. Keep in mind that I have learned R for one year so it might be not the most elegant code. Anyway, I am open to feedbacks :) 

Feel free to contact me for any inquiries you might have.

Thanks for reading and I hope it will help you !

## Contact:

Samuel Chaineau

##### Mail: samuel.chaineau@edu.em-lyon.com
##### LinkedIn: Samuel Chaineau 
##### GitHub: samchaineau
