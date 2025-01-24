# 🚨 Phishing-URL-Detection
This project focuses on using machine learning to combat phishing attacks by accurately classifying URLs as legitimate or phishing.

## 📜 Project Overview

Phishing URLs are malicious links crafted to steal sensitive information like login credentials or financial data. By analyzing features of URLs, we build machine learning models that classify URLs as either legitimate or phishing. Using the **PhiUSIIL dataset** from the UCI Machine Learning Repository, we achieved near-perfect classification accuracy.

### Key Highlights
- **Dataset:** 235,795 samples with 51 predictors.
- **Best Model:** Support Vector Machines (SVM) with 99.91% accuracy.
- **Key Features Analyzed:**
  - URL similarity index
  - Number of external references
  - Use of HTTPS, obfuscation, and special characters

---

## 💡 Features and Approach

### 1. Data Preprocessing
- Handled skewness, scaling, and outliers using Box-Cox transformation.
- Encoded categorical variables and addressed class imbalance.
- Removed low-variance predictors and highly correlated variables to reduce multicollinearity.

### 2. Model Development
- **Linear Models:** Logistic Regression, LDA, PLSDA, Penalized Models.
- **Non-linear Models:** SVM, Neural Networks, FDA, and others.
- Used stratified sampling and 3-fold cross-validation for robust evaluation.

### 3. Performance Metrics
- Evaluated models using **Kappa Score** to address class imbalance.
- Confusion matrices analyzed for false positives and false negatives.

---

## 🚀 Results

### Top Model: Support Vector Machines (SVM)
- **Accuracy:** 99.96%
- **Kappa Score:** 0.9991
- **Key Features:** URL similarity index, line of code, number of images.

| Model                  | Training Accuracy | Testing Accuracy | Testing Kappa |
|------------------------|-------------------|------------------|---------------|
| Logistic Regression    | 87.04%           | 88.28%           | 0.7522        |
| Penalized Model        | 90.84%           | 90.75%           | 0.8059        |
| Support Vector Machines| 99.92%           | 99.96%           | 0.9991        |
| Neural Networks        | 99.90%           | 99.94%           | 0.9989        |

### Top 5 Most important Predictors
1. URLSimilarityIndex
2. NoOfExternalRef
3. LineOfCode
4. HasSocialNet
5. HasDescription
