### How to Use

The **AMOS CFA One-Click Plugin** is designed for single-group, first-order reflective confirmatory factor analysis (CFA) in IBM SPSS Amos 31.

### Step 1: Prepare Your Data

Before running the plugin:

* Clean your `.sav` dataset.
* Make sure the CFA indicators contain no missing values.
* Reverse-code any negatively worded items where necessary.
* Ensure all CFA indicators are numeric.
* Check for serious outliers, coding errors, and variables with little or no variation.

### Step 2: Create the CFA Model in AMOS

Open **AMOS Graphics** and:

1. Select your SPSS `.sav` data file.
2. Draw the latent constructs.
3. Connect each construct to its observed indicators.
4. Add error terms to the observed variables.
5. Make sure each indicator loads on only one construct.
6. Name or label your latent constructs clearly.

The plugin is intended for **first-order reflective CFA models without cross-loadings or structural paths between latent constructs**.

### Step 3: Estimate the Model

Before using the plugin:

1. Click **Calculate Estimates** in AMOS.
2. Confirm that the model estimates successfully.
3. Check that AMOS does not report identification problems, negative error variances, or other improper solutions.

### Step 4: Install the Plugin

In AMOS Graphics:

1. Go to **Plugins → Plugins → Create**.
2. Open the supplied `AMOS31_CFA_OneClick_Plugin_v1_6.vb` file.
3. Copy the entire code.
4. Paste it into the AMOS plugin editor. 
5. Click **Check Syntax**.
6. Save the plugin.

After saving, the plugin should appear in the **Plugins** menu.

### Step 5: Run the CFA One-Click Report

With your CFA model open:

1. Go to the **Plugins** menu.
2. Select **CFA One-Click Report**.
3. Wait for AMOS to estimate the model and generate the report.
4. The report will open automatically in your web browser.

### Output

The plugin produces four main tables:

**Table 1 — Model Fit**

Includes:

* Chi-square
* Degrees of freedom
* p-value
* CMIN/df
* CFI
* RMSEA
* RMSEA 90% confidence interval
* PCLOSE

**Table 2 — Measurement Model**

Includes:

* Standardized factor loadings
* Average Variance Extracted (AVE)
* Cronbach's alpha

**Table 3 — Fornell-Larcker Criterion**

* The diagonal contains the square root of AVE.
* The lower triangle contains latent construct correlations.
* The criterion is satisfied when the square root of AVE is greater than the absolute correlation between constructs.

**Table 4 — HTMT**

Provides the Heterotrait-Monotrait ratio of correlations for assessing discriminant validity.

### Important

This plugin automates CFA reporting. It does not determine whether a model is theoretically appropriate or statistically acceptable. Researchers should still inspect model fit, factor loadings, reliability, discriminant validity, residuals, and the theoretical justification for the measurement model.
