# Geog418Lab3


## Introduction

This tutorial will walk you through performing a spatial autocorrelation analysis using R.

For context, spatial autocorrelation (SAC) is a metric used in spatial analysis to describe the relationship between an object and the distribution of nearby features (Gedamu et. al., 2024). Similar to other spatial analysis techniques, it aims to determine if a distribution of data is clustered, dispersed or random. Positive SAC refers to the tendency of features that are close together to have similar attributes. In this case, the result would be a clustered distribution. Conversely, negative SAC would refer to nearby features being dissimilar from each other, resulting in a dispersed distribution. Below is a visual example of SAC. 

<img width="544" alt="Screenshot 2024-10-19 at 5 21 43 PM" src="https://github.com/user-attachments/assets/3bbbbc4f-8dfb-425e-bd89-7c8152c21201">

Figure 1: Visual depiction of spatial autocorrelation created by and sourced from ESRI, n.d.

The presence of SAC can have important implications for spatial analysis. While there are multiple ways to quantify SAC, we will be using a technique called Moran's I. This is a statistical measure which will give us a concrete, standardized value to make an appropriate conclusion about the presence of SAC, or lack thereof. For this exercise, we will be examining census data from 2016 in the St. John's area. Although census tract data can provide a wealth of different information, we will be focusing on two variables: Median total income, and respondents with knowledge of the french language. When looking at income, it is important to use the median as the mean can be skewed by the extremely wealthy. Our objective will be to determine whether SAC is present in St. John's for our selected variables. Census data is ideal for this type of analysis, as positive SAC is often found when examining data pertaining to demographics (Li et al., 2012). It is often an appropriate choice because??????? 

These are both important variables to examine from a census point of view, as??

Talk about the census data, get a reference!!

Our first step is to install the appropriate packages for this analysis. Packages are sets of additional functions and commands that can be 
installed through r onto your machine. These can allow you to broaden the scope of your analysis, or create better maps and figures. Each package that you install can be called into use through the library function. Libraries are the tool that allows you to call on and utilize the functions in the package. 

To ensure all steps of this code function properly, install the following packages if not already installed: 
```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages:
#install.packages("knitr")
#install.packages("tmap")
#install.packages("spdep")
#install.packages("raster")
#install.packages("shinyjs")
#install.packages("e1071")
#install.packages("sf")

#Load in libraries:
library("knitr")
library("tmap")
library("spdep")
library("raster")
library("shinyjs")
library("e1071")
library("sf")

```
Now that we’ve installed the proper packages, we can read in our data. In general, when working in R file organization is highly important. It is a good idea to have any files/datasets you will be using in one folder. You can then set this folder as your working directory using the 'setwd(“folder address on your computer”)' function, as seen below. A working directory acts as a home base for your project. R will look for files you call, and save any outputs you may create using your working directory.

For this analysis we will be using a .csv file containing the 2016 census attribute data. 
You can read the csv easily using the read.csv() function. While this file has all the required information about our selected variables across each census tract, it does not have any spatial coordinates, meaning we cannot map this file alone. 
The second file is an .shp file containing census tract boundaries. For this file, the 'st_read()' function from the 'sf' package will be used. This will serve as the spatial component we need for this analysis to continue. 

It is important to consider that the extent of both these datasets concerns all of Canada. Since we will be focusing our analysis on the city of St. John's, it is important to set an appropriate projection. This will ensure that our map outputs generate without any unwanted distortion. To do this we will create a new dataframe for our spatial data, and use the 'st_transform()' function from 'sf' to change the projection of our shapefile. The Coordinate Reference System (CRS) we will be using is CRS:3761, which is a specific NAD83 projection for Newfoundland & Labrador.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#Set Working Directory 
setwd("~/Desktop/Geog418/Lab3")

#Read in Data
csv <- read.csv("~/Desktop/Geog418/Lab3/ucgsJQnBVLvP_data.csv") 
shp = st_read("/Users/liam/Desktop/Geog418/Lab3/Assignment3_Data/lda_000a16a_e.shp")

#Projection transformation for N&L
stjohns = st_transform(shp, crs=3761)

```

This next chunk of code will focus on cleaning data, merging the .csv and the spatial data, and subsetting the extent to St. Johns. Currently the census data does not have informative column names, making it hard to identify and call on certain variables. To remedy this, make a new dataframe containing a list of column names (ensure these are in the correct order). We can then apply this list to our csv by setting 'colnames(csv)' equal to the list. 

Census data is unique in that each observation is complete with a GEOUID. This number essentially ties the observation to a specific census tract location (Statcan. n.d). We will create an additional column 'len' using 'nchar()', which counts the number of characters per ID. Using '$' after an object/dataframe name allows you to reference a specific column. We will see more of this later. Observations with less than 8 characters are missing a dissemination area, making them incomplete. These will be removed by subsetting our csv to only include rows where 'len' is equal to 8.

With our census data cleaned and correctly labeled, we can now merge it with the census tract map. We will use the 'merge()' function. The parameters 'by.x = "DAUID"' and 'by.y = "GEOUID"' tell the function to georeference each observation using its unique Dissemination area ID and Geo ID. This filters each observation into its appropriate polygon in the shapefile. After our files are merged into one, we can create a new object that is a subset of just values in St.John's by referencing the city name column 'census_DAs$CMNAME'. 

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names for csv
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID characters
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

#Merge spatial and aspatial data
census_DAs <- merge(stjohns, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Subset for St. John's
Municp <- subset(census_DAs, census_DAs$CMANAME == "St. John's")

#Convert to Percentage
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```
Continuing with our data cleaning and setup, The 'French Knowledge' variable is currently just a simple count per area. To make it easier to interpret our results, we will want to create a new column that provides the percentage of french speakers per area. To do this we can write a simple percentage calculation using pre-existing fields.

Finally, the last step in our data processing and cleaning is to create specific objects for our two variables of interest, making sure to remove any NA values that could potentially interfere with or skew our analysis later on. 

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA Values
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA Values
French_noNA <- Municp[which(!is.na(Municp$`PercFrench`)),]

```


With our datasets ready to go, we can now calculate some basic descriptive statistics for both Income and French Knowledge. Descriptive statistics are an important metric to understand the shape of our distribution. We will calculate the mean, standard deviation, and skewness for each variable. These all have their own fairly straightforward functions in R as seen below. We will also display these results in a table using the 'kable' function from the 'knitr' package. However, to do this we will need to create a dataframe of our results, defining which results fall in each column, and how many digits to round to. This can then easily be displayed with 'kable' along with an appropriate caption (Table 1). 

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}

#Calculating descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculating descriptive stats for French
meanFrench <- mean(French_noNA$`PercFrench`)
stdevFrench <- sd(French_noNA$`PercFrench`)
skewFrench <- skewness(French_noNA$`PercFrench`)

#Create a dataframe for display in table, specify how many digits of rounding
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table with kable
kable(data, caption = paste0("Descriptive statistics for selected ", 2016, " census variables"))
```
<img width="504" alt="Screenshot 2024-10-20 at 4 39 10 PM" src="https://github.com/user-attachments/assets/aefcbe42-b5f7-4e5e-984b-33acfb9c966c">





To help gain a better understanding of the spatial distribution of our data, we will now map our two variables of interest. There are a couple different ways to map in R, in this instance we will use the 'tmap' package. The shape/extent of the map is defined by 'tm_shape()' by inputting the dataframe of interest. 'tm_polygons()' defines how the polygons appear on the map. This function has a couple key elements:

'col' - selects the column of interest within the dataframe (eg. Median total income)

'title' - Sets the title of the map

'style' - Refers to the reclassification scheme used to assign a color to each polygon. In our case we will use Natural breaks (Jenks) as it will define the most appropriate groups in the data and likely provide the best map output given the distribution of the data. 

'pallete' - Selects the color pallete of the map. You can use the pallete explorer listed below to view options. 

'border.alpha' - Defines the transparency of the borders between polygons. For our purposes we want a value of zero (fully transparent). 

'colorNA' - Defines the color of NA values on the map, should there be any.


Finally, 'tm_layout()' will let us adjust the position of the legend. 

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="St. John's census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a pallete using this tool
# tmaptools::palette_explorer() #Tool for selecting pallettes

#Mapping median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "PuBu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("LEFT", "TOP"))

#Mapping French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "PuBu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("LEFT", "TOP"))

#Print maps side by side 
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
To print the both maps in a figure side by side (Figure 2) you can use 'tm_arrange'. 

<img width="775" alt="Screenshot 2024-10-20 at 4 38 06 PM" src="https://github.com/user-attachments/assets/b82fc73a-01ff-4830-8423-15e004f2dc02">

Figure 2: Maps depicting Median Total Income (left), and Percentage of Correspondants with French Knowledge (right).


## Neighbourhood matrix

In order to conduct our Moran’s i analysis, R requires information about the location of ‘neighbors’ across the data, and how they are defined. To do this, a weighted neighborhood matrix is needed. A weighted neighborhood matrix is a file that defines the spatial neighborhood of each feature (*i*) across a dataset (Moraga, 2023). The spatial neighborhood is then used to define the weight (w<sub>*ij*</sub>)  of every other feature (*j*) relative to *i*, creating a matrix. Both the spatial neighborhood, and how weights are assigned is based on the chosen weighting scheme. For this analysis we will focus on two types of weights: Rook, and Queen (Figure 3). These weighting schemes are named after chess pieces, delineating which directions adjacent features will be considered as a neighbor. Rook Weight considers features directly adjacent only on vertical and horizontal planes. Queen weighting does the same, but also adds features that are diagonally adjacent. In both cases, features considered neighbors are given a binary weight value of 1, while all other points receive 0. 

<img width="568" alt="Screenshot 2024-10-20 at 5 52 42 PM" src="https://github.com/user-attachments/assets/3195f0f7-06d6-40b0-824d-fff6891535a3">

Figure 3: Visual guide to Rook and Queen weighting. Figure created by and sourced from Moraga, 2023.  

We will now generate a list of neighbours for each of our datasets. To do this, we can use the 'poly2nb()' function from the ‘spdep’ package. This function works to build a list of neighbours for each polygon within the dataset given that they have defined borders. The function assumes queen weighting, so when we perform our rook weights we must add 'queen=FALSE'.

Here we also create Income.net and French.net objects using 'nb2lines()'. This function creates vector lines between each feature and their neighbors. We will use this later to map how each polygon relates to eachother. 

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates, st_centroid places the reference coordinate in the center of each polygon
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))


#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)

```

To map the aforementioned Income.net objects we will once again employ 'tmap'. There is no need to map both variables as they both use the same census tracts. In this instance, 'tm_shape()' is once again describing the shape/extent of the map, and 'tm_borders()' is defining border color. Since we are mapping two objects on one map, we use the '+' operator, and use 'tm_shape()' again to map our lines, this time with our .net object. To change the color and width of the lines, you can use 'tm_lines'. We will also make a combined map showing both weighting schemes using the '+' operator again. 

We can print these maps side by side using 'tmap_arrange()', setting the number of columns to 3. 

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="St. John's census dissemination areas showing median total income neighbours queens weight (left) rooks weight (middle) and the combination of the two (right)."}


#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + tm_lines(col='blue')

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net2) + tm_lines(col='red', lwd = 2)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + tm_lines(col='blue', lwd = 2) +
  tm_shape(Income.net2) + tm_lines(col='red', lwd = 2)

#Print maps in a three pane figure THIS IS WRONG FIGURE IT OUT
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

```
<img width="818" alt="Screenshot 2024-10-20 at 6 43 02 PM" src="https://github.com/user-attachments/assets/33803d79-607a-488c-805d-cb495ef57403">

Figure 4: Maps showing different weighting types. 

As seen above in figure 4 these maps show different neighbor weighting types, with each polygon drawing a line to each neighbor. 


We will now create a weight matrix for each of our variables to be used in further anlaysis. To do this in R, we will use the "nb2listw()" function from "spdep". This function will use the list of neighbours previously made, however it will also create a list of assigned weights for each neighbor. The process for how weights are assigned is dependant on the chosen 'style'. This refers to the type of weighting scheme. This function has several weighting options, for example "B" or "W". Type "B" is simple binary coding, where neighbors are assigned a value of 1, and all others 0. Type "W" implies the weighting scheme is row standardized, meaning the weights of all neighbors linked to a feature are equally given and sum to 1. For our purposes we will be using type "W". Another important parameter for this function is 'zero.policy()'. This accounts for the possibility that a feature has no neighbors. If set to FALSE, the program will recognize such a feature as an error and stop the program. Since we do not want this, we will set zero.polic as TRUE, and it will assign a value of zero to any feature with no neighbors.  

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]

```


## Global Moran’s I

Intro to morans I
Equation:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Here, if $x$ is the variable being assessed, $x_i$ is the variable value at a point of interest (i) and $x_j$ represents a neighbour to $x_i$ (here determined by the queen weighting scheme). The spatial weighting applied to the weighting matrix $W_{i,j}$ is multiplied by both the differences of $x_i$ and the mean value of variable $x$, and $x_j$ and the mean value of variable $x$.

The denominator in this case is used to standardize our values, and therefore relatively high values of I correspond with positive spatial autocorrelation, and relatively low values of I correspond with negative spatial autocorrelation. Remember that the global Moran’s I statistic provides an indication of how spatially autocorrelated our data is over the entire dataset, thus representing a spatial pattern at the global scale [15].


```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```


Describe the results.


```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

Describe what the results indicate.

However, we can still go a step further and figure out whether these patterns are statistically significant. To do so, we can use a Z-test. Here our null hypothesis is ?, and the alternate hypothesis is ?. Using an $\alpha$ value of 0.05, if our Z-score falls above or below 1.96, we can say ?. A value greater than +1.96 would imply ?, and a value less than -1.96 would imply ?.

We can calculate a Z-test using the following code:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

The zscores for both variable confirm that ?

## Local spatial autocorrelation

Explain local spatial autocorrelation

The calculation for Local Moran’s I has many of the same features as our global calculation, although arranged in a different way.

```math
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1}
```

Again, instead of typing out these calculations, we can use the localmoran() function to deal with all of the messy calculations for us, as long as we input our variable and weighting scheme.


```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```


Now going back to our basic mapping template we can visualize some of these results to understand what this test is doing.


```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="St. Johns census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores - Income",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("right", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("left", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores - French",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("right", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("left", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)

```

Explain the results.


While these maps are great for visualizing where the data is and getting a rough idea of how many polygons are significantly positively or negatively spatially autocorrelated, it can be even more informative to graph these trends.

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```


```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```


In these plots, the points with diamonds are considered statistically significant, and the regression line shows the overall trend. For both plots we can see that the trend shows?




## Summary

Provide a brief summary.

## References
