# Role Analysis

Public code for the manuscript: Tyler M. Barrett, Charles Kevin Tiu, Kayla Kauffman, Michelle Pender, Jean Yves Rabezara, Prisca Rahary, Lin-Fa Wang, Randall A. Kramer, Voahangy Soarimalala, Peter J. Mucha, James Moody, Charles L. Nunn. Identifying Social-Epidemiological Roles Associated with Viral Expousre Using Regular Equivalence Blockmodeling.
[medRxiv preprint](https://www.medrxiv.org/content/10.1101/2025.11.05.25339611v2).

## role_analysis.R

This R script contains the code to prepare the social network data for analysis, conduct
the role analysis using regular equivalence blockmodeling, and model the relationship
between roles and viral exposure.

It requires the following data sources:
  1. Survey_Demographic_Health.csv
  2. Name_Table.csv
  3. C5_All_HitsList.csv and C10_All_HitsList.csv (VirScan Results)

The data are not publicly available due to privacy or ethical considerations.

## role_analysis_summary_figure.R

This R script contains the code to generate a summary figure to provide a high-level
characterization of the role analysis results. It requires the role analysis
results generated with role_analysis.R.

## triad_positions_figure.R

This code generates the triad position typology figure. No data are required.