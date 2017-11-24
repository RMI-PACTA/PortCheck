# PortCheck

This code tests the alignment of Portfolios against cliamite scenarios.

### Steps to run a PortCheck:

0) Decide on a BatchName

1) Make a Parameter File and put it in 
PORT.DATA.PATH\99_ParameterFiles\BatchName_ParameterFile.csv

2) Put the portfolio data file in 
PORT.DATA.PATH\04_Others\BatchName\BatchName_Input.csv

3) Set file paths, etc
Run 2dii-init.R

4) Prepare the portfolio data
Run DataImport.R (runs with Parameter File).  
Note: This runs PortCheck-init.R (project-specific constants and file locations), GlobalPortCheckFunctions.R (project-specific functions), and user-overrides.R (if your file paths are different, change the constants here)

5) Run PortCheck_Equity.R (runs with Parameter File)

6) Run PortCheck_Debt.R (change the BatchName to NovPortChecks in the first lines of code)

7) Run BatchGraphGenerator_v6_KH (runs with Parameter File)

8) Check the reports: C:\Users\Clare\Dropbox (2° Investing)\PortCheck\03_Results\05_Reports\04_Others\BatchName

