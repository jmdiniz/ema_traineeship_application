# Death during hospital admission and up to 30 days after discharge
# For Heart Failure patients

# Features:
## Primary care information (diagnoses, blood pressure values, body mass index)
## First BNP, PCR, TropI, Sodium values
## First Total Cholesterol, HDL Cholesterol, LDL Cholesterol, Hemoglobin, Leukocytes values
## Hospital diagnoses and processes

####################################################

# Import the necessary modules

import pandas as pd
from matplotlib import pyplot as plt
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn import metrics, tree
from sklearn.metrics import confusion_matrix, f1_score, roc_curve, roc_auc_score
from xgboost import XGBClassifier

# Input data

admissions = pd.read_csv("DT_10.csv")

columns_to_convert = list(range(3,17)) + list(range(20,30)) +list(range(87,173))
admissions.iloc[:, columns_to_convert] = admissions.iloc[:, columns_to_convert].astype('object')

categorical_columns = admissions.select_dtypes(include=['object']).columns
df_encoded = pd.get_dummies(admissions, columns=categorical_columns)

print(df_encoded.head())

# Transform data

X = df_encoded.drop(columns=["ID_Doente","ID_EPISODE","Death_Category_Admission_Restricted"]).values
y = df_encoded["Death_Category_Admission_Restricted"].values

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 42)

## Decision Tree (base)

mountain_tree = tree.DecisionTreeClassifier()
mountain_tree.fit(X_train, y_train)
y_pred = mountain_tree.predict(X_test)

print(f1_score(y_test, y_pred, average=None))

tn, fp, fn, tp = confusion_matrix(y_test, y_pred).ravel()

sensitivity = tp / (tp+fn)
print(sensitivity)

specificity = tn / (tn+fp)
print(specificity)

fpr, tpr, thresholds = metrics.roc_curve(y_test, y_pred)
metrics.auc(fpr, tpr)

##### AUROC = 0.6886565189285282 (DT_10)

## Decision Tree - XGBoost

xgboost = XGBClassifier()
xgboost.fit(X_train, y_train)
y_pred = xgboost.predict(X_test)

print(f1_score(y_test, y_pred, average=None))

fpr, tpr, thresholds = metrics.roc_curve(y_test, y_pred)
metrics.auc(fpr, tpr)

##### AUROC = 0.7273956944500559 (DT_10)

param_grid = {
    'max_depth': [5, 6, 7, 8],
    'learning_rate': [0.1], # derived from [0.1, 0.01, 0.001], 
    'subsample': [0.5, 0.6, 0.7, 0.8]
}

# Create the XGBoost model object
xgb_model = XGBClassifier()

# Create the GridSearchCV object
grid_search = GridSearchCV(xgb_model, param_grid, cv=5, scoring='roc_auc')

# Fit the GridSearchCV object to the training data
grid_search.fit(X_train, y_train)

# Print the best set of hyperparameters and the corresponding score
print("Best set of hyperparameters: ", grid_search.best_params_)
print("Best score: ", grid_search.best_score_)

##### AUROC = 0.8516271352263376 (DT_10)

best_pars = grid_search.best_params_
best_model = grid_search.best_estimator_

print(grid_search.score(X_test,y_test))

##### AUROC = 0.8611352403269901 (DT_10)

# Predict probabilities
lr_probs = grid_search.predict_proba(X_test)

# Keep probabilities for the positive outcome only
lr_probs = lr_probs[:, 1]
ns_probs = [0 for _ in range(len(y_test))]

# Calculate scores
lr_auc = roc_auc_score(y_test, lr_probs)
ns_auc = roc_auc_score(y_test, ns_probs)

# Summarize scores
print('Logistic: ROC AUC=%.3f' % (lr_auc))

# Calculate roc curves
lr_fpr, lr_tpr, _ = roc_curve(y_test, lr_probs)
ns_fpr, ns_tpr, _ = roc_curve(y_test, ns_probs)

# Plot the roc curve for the model
plt.plot(ns_fpr, ns_tpr, linestyle='--', label='No Skill')
plt.plot(lr_fpr, lr_tpr, marker='.', label='XGBoost (Fine Tuned)')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.legend()
plt.show()