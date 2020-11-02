#### Clone both script and doc folder to your local. 

#### In your local folder, open up execute.r file under script. 

There are a few steps before running the program.
1)  Input your user id to access database
```R
con <- dbConnect(
  odbc::odbc(),
  Driver='freetds',
  Server='rasdata.rasgcp.net',
  Database='ras_sas',
  uid= <your user id>,
  pwd=rstudioapi::askForPassword("Database password"),
  Port=1433
)
```
2) Comment out the country you are not producing
```R
  #CountryCode = 'GBR'
CountryCode = 'USA'
```
3) Set your local file path for A) where the scripts are B) where the management file is C) where the plots folder should be created at
```R
## file path where management file is
input_path = "~/Project/ManagementFiles"
## file path where plots and files export to
file_path = "~/Project/Schedulescopy/doc"
## file path where script are
scripts_path = "~/Project/Schedulescopy/script"
```
4) Make sure the management file is up to date
```R
excelfile = '20201022 SchedulesManagement.xlsx'
```

You are GOOD TO EXECUTE the execute.r file. You will expcet to see 1 .txt, 1 .csv and 3 .xlsx files. The .txt file is the one to upload. 
