# surveillance-anxiety-public-spaces

Data, code, and figures for two published papers examining festival-goers' attitudes toward surveillance and policing in public spaces.

## Papers

**2020 pilot:** Crampton, J.W., Hoover, K.C., Smith, H., Graham, S., & Berbesque, J.C. (2020). Smart festivals? Security and freedom for well-being in urban smart spaces. *Annals of the American Association of Geographers*, 110(6), 1997--2015. DOI: [10.1080/24694452.2019.1662765](https://doi.org/10.1080/24694452.2019.1662765)

**2022 full study:** Hoover, K.C., Crampton, J.W., Smith, H., & Berbesque, J.C. (2022). Surveillance, trust, and policing at music festivals. *The Canadian Geographer*, 66(2), 202--219. DOI: [10.1111/cag.12695](https://doi.org/10.1111/cag.12695)

## Data

Online survey of adult festival-goers collected in two waves (September 2018 -- October 2019). Wave 1: n = 189; Wave 2: n = 27; final analytic sample n = 216 after exclusion of two respondents under 18. The 2020 pilot used a snapshot of the data at n = 201. Original pilot data are also available on [figshare](https://figshare.com).

Raw data are provided under CC BY 4.0 as stated in the published papers. Demographic identifiers have been removed from the comments tab to protect respondent privacy.

## Repository Structure

```
data-raw.xlsx                        # cleaned SurveyMonkey export (two-wave)
revised-data.xlsx                    # analytical tabs output by ETL (9 tabs)
revised-data-ranked-choice.xlsx      # ranked choice data for pilot figures

revised-etl.R                        # ETL: raw → analytical tabs
revised-models.R                     # descriptives, models, results sinks
revised-figures.R                    # figures for 2022 paper (fig1--fig5)
revised-annals-figures.R             # figures for 2020 pilot paper

revised-fig1.png                     # 2022: general safety concerns
revised-fig2.png                     # 2022: specific safety concerns
revised-fig3.png                     # 2022: safety measures
revised-fig4.png                     # 2022: feelings about surveillance
revised-fig5.png                     # 2022: surveillance behaviour

revised-annals-fig1.png              # 2020: safety concerns overall
revised-annals-fig2.png              # 2020: safety concerns by gender
revised-annals-fig3.png              # 2020: safety measures by gender
revised-annals-fig4.png              # 2020: feelings about surveillance by gender
revised-annals-fig-rc-stacked.png    # 2020: ranked choice stacked bar
revised-annals-fig-rc-dot.png        # 2020: ranked choice mean rank by gender
revised-annals-fig-rc-heat.png       # 2020: ranked choice heatmap

revised-results-*.txt                # model results and fit checks

renv.lock                            # R environment lockfile
renv/                                # renv library
```

## Reproducing the Analysis

Restore the R environment:
```r
renv::restore()
```

Then run scripts in order:
```
revised-etl.R              # produces revised-data.xlsx and revised-data-ranked-choice.xlsx
revised-models.R           # produces model RDS objects and results txt files
revised-figures.R          # produces revised-fig1.png through revised-fig5.png
revised-annals-figures.R   # produces revised-annals-fig*.png
```

## A Note on Reproducibility

The 2020 pilot was published before full methods documentation was standard practice in this research group. Figures for both papers use the revised 6-category safety concern coding from the 2022 analysis (consolidating Sexual Harassment and Sexual Assault into Sexual Safety; dropping Unwanted Solicitation as analytically distinct). The pilot paper used an 8-category scheme; minor differences from the published figures reflect this consolidation. Gender coding for the pilot figures collapses Trans and Intersex into Non-binary due to small cell sizes, consistent with the published Table 1. These decisions are documented in the ETL script and in the portfolio page.

## License

Code and scripts © Kara C. Hoover, licensed under the [MIT License](LICENSE).

Data, figures, and written content © Kara C. Hoover, licensed under [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/).