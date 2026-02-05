# Stress Level Prediction Analysis
This project was developed due to an academic group project and it focused on analyzing stress factors with statistical methods and ML techniques.
## Overview
The analysis is divided into three main phases:
### 1) Exploratory Data Analysis 
* **Descriptive Analysis:** To understand the distribution and main characteristics of the variables.
* **Bivariate Analysis:** To identify relationships and correlations between pairs of variables.

### 2) Unsupervised Learning (Clustering Analysis)
We implemented non-supervised models to group the data:
* **Hierarchical Methods:** Used to explore the data structure.
* **Optimal Cluster Selection:** We determined the best number of clusters using statistical index.
* **K-Means Algorithm** based on **Elbow Method** to determine the optimal number of clusters by plotting the total within-cluster sum of squares against the number of clusters.

### 3) ML predictive analysis on Spark
We implemented a **K-Means algorithm** using the **Spark** framework to predict Stress Levels groups splitting the dataset in **training and test set** and estimating **accuracy rate**
