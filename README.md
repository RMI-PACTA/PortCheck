# PortCheck

This code tests the alignment of Portfolios against climite scenarios.

### Steps to run a PortCheck:

Note: ```PORT.DATA.PATH``` is currently ```Dropbox (2 Investing)\PortCheck\02_PortfolioData```.

1. Set up the 2dii Dev environment.  First clone the ```Common``` repository and then run ```Common/2dii-init.R```.

2. Decide on a BatchName. Use it wherever you see "BatchName" below.

3. Make a Parameter File.  Put it here: 
```
PORT.DATA.PATH\99_ParameterFiles\BatchName_ParameterFile.csv
```
4. Put the portfolio data file in 
```
PORT.DATA.PATH\04_Others\BatchName\BatchName_Input.csv
```
5. Prepare the portfolio data.  Run:
```
DataImport.R  
```
Note: This runs 
* ```PortCheck-init.R``` (project-specific constants and file locations)
* ```GlobalPortCheckFunctions.R``` (project-specific functions)
* ```user-overrides.R``` (if your file paths are different, change the constants here)

6. Run ```PortCheck_Equity.R``` (runs with Parameter File)

7. Run ```PortCheck_Debt.R``` (change the BatchName to NovPortChecks in the first lines of code)

8. Run ```BatchGraphGenerator_v6_KH.R``` (runs with Parameter File)

9. Check the reports: 
```
PORT.DATA.PATH\03_Results\05_Reports\04_Others\BatchName
```
