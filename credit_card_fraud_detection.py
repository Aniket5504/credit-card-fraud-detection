import os
import sys
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import (accuracy_score, precision_score, recall_score,
                              f1_score, roc_auc_score, confusion_matrix,
                              classification_report)
from imblearn.over_sampling import SMOTE
import matplotlib.pyplot as plt
import seaborn as sns
import json

# ---------------------------------------------------------------------------
# 1. Load data
# Looks for creditcard.csv in the SAME FOLDER as this script — works on
# Windows, Mac, and Linux without any hardcoded paths.
# ---------------------------------------------------------------------------
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_PATH = os.path.join(SCRIPT_DIR, "creditcard.csv")

if not os.path.exists(DATA_PATH):
    print(f"ERROR: Could not find 'creditcard.csv' in: {SCRIPT_DIR}")
    print("Download the dataset from: https://www.kaggle.com/mlg-ulb/creditcardfraud")
    print("and place 'creditcard.csv' in the same folder as this script.")
    sys.exit(1)

df = pd.read_csv(DATA_PATH)
print("Class distribution:\n", df['Class'].value_counts())
fraud_pct = df['Class'].value_counts(normalize=True)[1] * 100
print(f"Fraud %: {fraud_pct:.4f}%")

# ---------------------------------------------------------------------------
# 2. Preprocessing
# ---------------------------------------------------------------------------
X = df.drop('Class', axis=1)
y = df['Class']

scaler = StandardScaler()
X['Amount'] = scaler.fit_transform(X[['Amount']])
X['Time'] = scaler.fit_transform(X[['Time']])

# ---------------------------------------------------------------------------
# 3. Train/test split (stratified, BEFORE SMOTE to avoid leakage)
# ---------------------------------------------------------------------------
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)

print("\nBefore SMOTE - Train class distribution:")
print(y_train.value_counts())

# ---------------------------------------------------------------------------
# 4. Apply SMOTE only on training data
# ---------------------------------------------------------------------------
smote = SMOTE(random_state=42)
X_train_res, y_train_res = smote.fit_resample(X_train, y_train)

print("\nAfter SMOTE - Train class distribution:")
print(pd.Series(y_train_res).value_counts())

# ---------------------------------------------------------------------------
# 5. Baseline model (no SMOTE) for comparison
# n_jobs=-1 uses all available CPU cores on whatever machine runs this.
# ---------------------------------------------------------------------------
baseline_model = RandomForestClassifier(n_estimators=30, random_state=42, n_jobs=-1, max_depth=12)
baseline_model.fit(X_train, y_train)
y_pred_baseline = baseline_model.predict(X_test)
y_proba_baseline = baseline_model.predict_proba(X_test)[:, 1]

baseline_metrics = {
    "accuracy": accuracy_score(y_test, y_pred_baseline),
    "precision": precision_score(y_test, y_pred_baseline, zero_division=0),
    "recall": recall_score(y_test, y_pred_baseline, zero_division=0),
    "f1": f1_score(y_test, y_pred_baseline, zero_division=0),
    "auc_roc": roc_auc_score(y_test, y_proba_baseline)
}

# ---------------------------------------------------------------------------
# 6. SMOTE-balanced model
# ---------------------------------------------------------------------------
model = RandomForestClassifier(n_estimators=30, random_state=42, n_jobs=-1, max_depth=12)
model.fit(X_train_res, y_train_res)

y_proba = model.predict_proba(X_test)[:, 1]
threshold = 0.50
y_pred = (y_proba >= threshold).astype(int)

smote_metrics = {
    "accuracy": accuracy_score(y_test, y_pred),
    "precision": precision_score(y_test, y_pred, zero_division=0),
    "recall": recall_score(y_test, y_pred, zero_division=0),
    "f1": f1_score(y_test, y_pred, zero_division=0),
    "auc_roc": roc_auc_score(y_test, y_proba)
}

print("\n=== BASELINE (No SMOTE) ===")
for k, v in baseline_metrics.items():
    print(f"{k}: {v:.4f}")

print("\n=== WITH SMOTE ===")
for k, v in smote_metrics.items():
    print(f"{k}: {v:.4f}")

print("\nConfusion Matrix (SMOTE model):")
cm = confusion_matrix(y_test, y_pred)
print(cm)

print("\nClassification Report (SMOTE model):")
print(classification_report(y_test, y_pred, digits=4))

# ---------------------------------------------------------------------------
# 7. Save outputs (also relative to script location, not hardcoded)
# ---------------------------------------------------------------------------
results = {"baseline": baseline_metrics, "smote": smote_metrics, "confusion_matrix": cm.tolist()}
with open(os.path.join(SCRIPT_DIR, 'results.json'), 'w') as f:
    json.dump(results, f, indent=2)

importances = pd.Series(model.feature_importances_, index=X.columns).sort_values(ascending=False).head(10)
plt.figure(figsize=(8, 5))
sns.barplot(x=importances.values, y=importances.index)
plt.title('Top 10 Feature Importances - Random Forest')
plt.xlabel('Importance')
plt.tight_layout()
plt.savefig(os.path.join(SCRIPT_DIR, 'feature_importance.png'), dpi=120)
plt.close()

plt.figure(figsize=(5, 4))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=['Legit', 'Fraud'], yticklabels=['Legit', 'Fraud'])
plt.title('Confusion Matrix (SMOTE + Random Forest)')
plt.ylabel('Actual')
plt.xlabel('Predicted')
plt.tight_layout()
plt.savefig(os.path.join(SCRIPT_DIR, 'confusion_matrix.png'), dpi=120)
plt.close()

print("\nDone. Results and plots saved in:", SCRIPT_DIR)