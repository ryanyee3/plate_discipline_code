# Evaluating plate discipline in Major League Baseball with Bayesian Additive Regression Trees

This repository contains code from the paper [*Evaluating plate discipline in Major League Baseball with Bayesian Additive Regression Trees*](https://arxiv.org/abs/2305.05752).

## Data

We used pitch data from the MLB's Statcaste database.
We scraped this data using the baseballr package.
This script was adapted from Bill Petti's [tutorial](https://billpetti.github.io/2021-04-02-build-statcast-database-rstats-version-3.0/).

We also include code for preprocessing this data in `data_processing.R`.

## Modeling

We include code to reproduce the models we discuss in our paper.
All models are fit using the [flexBART](https://github.com/skdeshpande91/flexBART) package.

