# DMARC STAT 190 Capstone Project
by: Anthony Esboldt, Tyler Nansel, Riley Schultz, Nathan VanDeWoestyne
<h4>Introdction</h4>
For this project we worked with DMARC to give them valuable insights into their future operations. We aimed to predict the upper bound of food pantry visitors in the Des Moines metro area and what zip codes the additional visitors will be coming from.
<h4>Data</h4>
Clustering_Final.R = Our clustering model to find demographic information of the most common food pantry visitors.
DMARC_Chloropleth_Maps.R = R file with the required libraries to make chloropleth maps for food pantry current visitors, total predicted visitors, and where additional visitors will be from.
<h4>Methods</h4>
We performed k-means clustering in order to find demographic information of the most common food pantry visitors. To predict the upper bound of visitors, we used a GLM model with a poisson distribution and a log link. After predicting the upper bound of visitors, we used a logistic growth function to predict estimated growth every year until we reached the upper bound of visitors in 2060.
<h4>Results</h4>
We esimate there are around 35,000-45,000 people in the Des Moines metro area are at risk of needing assistance from a food pantry. Of the people not currently receiving food pantry assistance, we predict that the zip code with the most people needing assistance was in West Des Moines, zip code 50266. We found this surprising because this zip code has the Jordan Creek Mall area, but got some validation by firsthand accounts by a DMARC board member.
