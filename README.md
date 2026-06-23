# Credit Card Fraud Detection

A machine learning pipeline to detect fraudulent credit card transactions in a highly imbalanced dataset using Random Forest and SMOTE oversampling.

## Overview

Credit card fraud is rare — in this dataset, only **0.17%** of transactions are fraudulent. Standard classifiers tend to ignore the minority class entirely when data is this imbalanced. This project builds and compares two Random Forest models (with and without SMOTE) to handle that imbalance, and evaluates them using metrics suited to imbalanced classification (precision, recall, F1, AUC-ROC) rather than accuracy alone.

The script is **portable** — it automatically locates `creditcard.csv` in the same folder it's run from, so it works on Windows, macOS, and Linux without editing any file paths.

## Dataset

- **Source:** [Credit Card Fraud Detection dataset (Kaggle)](https://www.kaggle.com/mlg-ulb/creditcardfraud)
- **Size:** 284,807 transactions, 31 columns
- **Features:** `Time`, `Amount`, and 28 anonymized PCA-transformed features (`V1`–`V28`)
- **Target:** `Class` (0 = legitimate, 1 = fraud)
- **Class balance:** 492 fraud cases out of 284,807 (~0.17%)

> ⚠️ The dataset is **not included** in this repo due to size/licensing. Download `creditcard.csv` from Kaggle and place it in the **same folder** as `fraud_detection_portable.py`.

## Approach

1. **Preprocessing** — Scaled `Time` and `Amount` using `StandardScaler` (the `V1`–`V28` features are already PCA-transformed and scaled).
2. **Train/test split** — Stratified 80/20 split, performed *before* SMOTE to prevent data leakage into the test set.
3. **Baseline model** — Random Forest trained directly on the imbalanced training data.
4. **SMOTE oversampling** — Synthetic Minority Over-sampling applied only to the training set to balance the classes (227,451 vs 227,451 after resampling).
5. **SMOTE model** — Random Forest trained on the balanced training data.
6. **Evaluation** — Both models evaluated on the untouched, imbalanced test set using accuracy, precision, recall, F1-score, AUC-ROC, and a confusion matrix.

## Results

| Metric | Baseline (No SMOTE) | With SMOTE |
|---|---|---|
| Accuracy | 99.95% | 99.83% |
| Precision | 92.86% | 51.23% |
| Recall | 79.59% | **84.69%** |
| F1-score | 85.71% | 63.85% |
| AUC-ROC | 0.9745 | 0.9717 |

**Key takeaway:** SMOTE improves fraud *recall* (catching more actual fraud cases) at the cost of *precision* (more false positives). This is the expected and well-documented precision/recall tradeoff in imbalanced classification — which model to deploy depends on whether missing fraud (false negatives) or flagging legitimate transactions (false positives) is more costly for the business.

## Tech Stack

- **Language:** Python 3
- **Libraries:** scikit-learn, pandas, NumPy, imbalanced-learn (SMOTE), Matplotlib, Seaborn

## Project Structure

```
credit-card-fraud-detection/
├── fraud_detection_portable.py   # Main pipeline script (portable, no hardcoded paths)
├── creditcard.csv                # Dataset (not included, download from Kaggle)
├── results.json                  # Saved evaluation metrics (generated on run)
├── feature_importance.png        # Top 10 feature importances plot (generated on run)
├── confusion_matrix.png          # Confusion matrix heatmap (generated on run)
└── README.md
```

## How to Run

1. Clone this repository:
   ```bash
   git clone https://github.com/<your-username>/credit-card-fraud-detection.git
   cd credit-card-fraud-detection
   ```
2. Install dependencies:
   ```bash
   pip install pandas numpy scikit-learn imbalanced-learn matplotlib seaborn
   ```
3. Download `creditcard.csv` from [Kaggle](https://www.kaggle.com/mlg-ulb/creditcardfraud) and place it in the project root (same folder as the script).
4. Run the script:
   ```bash
   python fraud_detection_portable.py
   ```
   If the dataset isn't found, the script will print a clear error message instead of crashing.
5. Outputs:
   - Console: class distribution, metrics, confusion matrix, classification report
   - `results.json`: saved metrics
   - `feature_importance.png` and `confusion_matrix.png`: saved plots, written next to the script

## Future Improvements

- Tune the classification threshold (currently 0.50) to optimize for business-specific precision/recall tradeoffs
- Try alternative resampling techniques (e.g. ADASYN, undersampling, class weighting)
- Experiment with other models (XGBoost, LightGBM, Logistic Regression) for comparison
- Add cross-validation for more robust performance estimates

## License

This project is open source and available under the [MIT License](LICENSE).
