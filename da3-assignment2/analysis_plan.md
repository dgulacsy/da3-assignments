# DA3 Assignment 2: Finding fast growing firms

## Tasks
1. Build a model to predict fast growth of firms
2. design the target (fast growth) variable using one or two years
3. Argue for choice, discussing a few alternatives
4. Build three different models and pick one (should include at least one logit and one random forest)

## Subtasks
1. Look at descriptives, lowess, tabulate factors, make decisions

## Analysis Plan
1. Probability prediction
    - Predict probabilities
    - Look at cross-validated performance and pick your favorite model
2. Classification
    - Think about the business problem, and define your loss function (like FP=3, FN=7)
    - For each model, predict probabilities, look for the optimal classification threshold, calculate expected loss with your loss function. 
    - Pick the model that has the smallest average (over 5 folds) expected loss
3. Discussion of results
    - Show a confusion table (on a selected fold or holdout set)
    - Discuss results, evaluate how useful your model may be


# Scoring
Data prep, label and feature engineering (25%)
Model building and probability prediction and model selection (25%)
Classification (20%)
Discussion of steps, decisions and results (15%)
Explain shortly every modelling decisions
At the end please give a 2-3 paragraph discussion of what you found.
Quality of the write-up (15%)

## Data
bisnode-firms data

###############################################################

# Ideas