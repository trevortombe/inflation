# Replication code for Chen and Tombe (2023)

*Canadian Public Policy, forthcoming*

The file "ReplicationClean.R" pulls all necessary data to replicate all figures and tables in the paper. It fetches the most recently available, which may have been revised since the paper's publication. The file "DataForPaper.RData" provides the data vintages used by the paper. The file "setup.R" is called by the replication file.

A FRED API key is required to run the analysis. The FRED data used in the paper is provided within the DataForPaper file, but any future updates to that data require you enter your API key in the "setup.R" file.

---

![](Figures/Figure1.png)

![](Figures/Figure2.png)

![](Figures/Figure3.png)

![](Figures/Figure5a.png)

Other figures are found within the "Figures" directory. Table 1 is provided there as a plain text file.
