
This was a (joint) course project in the Multivariate course alongside Debepsita Mukherjee and Rupayan Mandal. It was supervised by Dr. Swagata Nandi. In this project we have analyzed radiotherapy data.
The set contains data on people undergoing radiotherapy. Variables like number of symptoms, average appetite and average sleep (etc) throughout their treatment were considered. The dataset contained a categorical variable 
for skin reaction. The analysis includes EDA, normality checking and transformations, MANOVA, LDA, PCA and factor analysis. By studying the correlation structure of the data and Kendall’s tau test we conclude that one of
the variables is independent from others and consequently we drop that from our analysis. We checked whether the observations in different categories belong to the same population. From EDA and MANOVA we conclude that the
observations indeed come from the same population. Consequently, discriminant analysis is not meaningful. This fact is supported by APER calculated using one-out cross validation method. From factor analysis we conclude that
one variable is significantly different compared to others. Also among the other four, there are two factors affecting the variables. One factor is affecting appetite and food consumed. This factor could be due to the 
treatment’s effect on mouth/digestive system. The other is affecting the number of symptoms and sleep.
