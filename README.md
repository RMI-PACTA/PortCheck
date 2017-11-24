# PortCheck

This code tests the alignment of Portfolios against cliamite scenarios.

## Steps to run a PortCheck:
 
1) Set Parameter File with a specific BatchName (ie. NovPortChecks – not important what it is but must stay constant)
2) Put Portfolio input file into: C:\Users\USER.NAME\Dropbox (2° Investing)\PortCheck\02_PortfolioData\04_Others\BatchName
3) Run 2dii-init.R to set file paths, etc
4) Prepare the portfolio data: Run DataImport (runs with Parameter File).  
Note: This runs PortCheck-init.R (project-specific constants and file locations), GlobalPortCheckFunctions.R (project-specific functions), and user-overrides.R (if your file paths are different, change the constants here)
5) Run PortCheck_Equity.R (runs with Parameter File)
6) Run PortCheck_Debt.R (change the BatchName to NovPortChecks in the first lines of code)
7) Run BatchGraphGenerator_v6_KH (runs with Parameter File)
8) Check the reports: C:\Users\Clare\Dropbox (2° Investing)\PortCheck\03_Results\05_Reports\04_Others\BatchName

