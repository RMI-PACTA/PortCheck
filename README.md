### This project was funded by the European Commission through LIFE program under grant: LIFE16 GIC/FR/000061 - PACTA
# PortCheck

This code tests the alignment of Portfolios against climite scenarios.

### Steps to run a PortCheck:

1. Set up the 2dii Dev environment.  First clone the ```Common``` repository and then run ```Common/2dii-init.R```.  Note: this currently defines ```PORTS.PATH```  as ```Dropbox (2 Investing)\PortCheck\02_PortfolioData```

2. Decide on a ```BatchName```. Use it wherever you see "BatchName" below.

3. Make a Parameter File.  Put it here: 
```
PORTS.PATH\99_ParameterFiles\BatchName_ParameterFile.csv
```

4. Put the portfolio data file in 
```
PORTS.PATH\04_Others\BatchName\BatchName_Input.csv
```

5. Prepare the portfolio data.  Run:
```
DataImport.R  
```
  Note: This runs 
  * ```proj-init.R``` (project-specific constants and file locations)
  * ```GlobalPortCheckFunctions.R``` (project-specific functions)
  * ```proj-init-overrides.R``` (if your file paths are different, change the constants here)

6. Run ```PortCheck_Equity.R``` (runs with Parameter File)

7. Run ```PortCheck_Debt.R``` (change the BatchName to NovPortChecks in the first lines of code)

8. Run ```BatchGraphGenerator_v6_KH.R``` (runs with Parameter File)

9. Check the reports: 
```
PORTS.PATH\03_Results\05_Reports\04_Others\BatchName
```
