# STAT337-LTMLE-Application
This repository includes information on our STAT 337 Final Project, in the application of longitudinal targeted maximum likelihood estimation (LTMLE) to a football player statistics dataset.

The method is based on Feng et al.'s study of insulin resistance trajectories and cardiovascular disease risk.

Paper can be found here: https://link.springer.com/article/10.1186/s12933-025-02651-6 

Feng, Yaning, et al. “Assessing the Impact of Insulin Resistance Trajectories on Cardiovascular Disease Risk Using Longitudinal Targeted Maximum Likelihood Estimation.” Cardiovascular Diabetology, vol. 24, no. 1, Mar. 2025, p. 112. DOI.org (Crossref), https://doi.org/10.1186/s12933-025-02651-6. 

We applied the same methodology to football data, examining how changes in player performance (measured by passing yardage) influence the probability of continued participation.

# LTMLE Structure, as we defined it

- W: baseline variable (Years_Played_Before_t1)
- L: time-varying covariates (passing stats)
- A: treatment (Yards_Category)
- Y: outcome (Played_in_t3)

# Treatments

- (0,0): low → low
- (0,1): low → high (improved)
- (1,0): high → low (declined)
- (1,1): high → high

Two models were used:
- **Maximal model**: includes all covariates
- **Minimal model**: includes only key variables

---


## Repository Structure

### Report
Includes our Finalized Written Report.

### Tables of Output
This folder contains the tables used in the final report, including variable definitions, treatment trajectory descriptions, and a summary of LTMLE results.

- Table 1: Defines the variables used in the LTMLE framework (W, L, A, Y)
- Table 2: Describes treatment trajectories (abar values)
- Table 3: Summarizes model results, including ATE, relative risk (RR), odds ratio (OR), and p-values

### Model Output Files

- **LTMLE_Output_Script.pdf**  
  Contains the full results of the LTMLE models, including:
  - Additive Treatment Effect (ATE)
  - Relative Risk (RR)
  - Odds Ratio (OR)
  - p-values and confidence intervals

---

### R_Codes

- **LTMLE_Testing_2.R**  
  Main analysis script. Runs LTMLE on the dataset and produces:
  - maximal model (all covariates)
  - minimal model (reduced variables)
  - comparison of treatment trajectories

- **LTMLE_Testing.R**  
  Earlier version of the analysis script used for testing.

- **LTMLE_Output_Script.Rmd**  
  R Markdown file used to generate formatted output and visualizations.

- **LTMLE_Output_Script.docx**  
  Word document version of the formatted output.

- **Sports_Dataset_Processing_2.R / Sports_Dataset_Processing_3.R**  
  Scripts used to clean and structure the dataset into longitudinal format (t1, t2, t3).

- **Sports_Dataset_Retriever.R**  
  Script used to extract and organize raw football data into usable format.


---

## Key Findings

- Players who improved performance (0 → 1) had a significantly higher probability of continuing to play.
- The minimal model produced stronger and more significant results than the maximal model.
- Correlations between variables and the outcome were generally weak, suggesting limitations in the dataset.

---

## Contributors

- Siyaa Bhargava  
- Laura Aleksonis  
- Xhemka Elezi