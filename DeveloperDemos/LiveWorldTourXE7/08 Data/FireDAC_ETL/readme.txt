ETLSimple1.dpr

pawel.glowacki@embarcadero.com
12 Sept 2014

ETLSimple1 demo demonstrates some of the components from the XE7 new “FireDAC ETL” category.
The project contains TBatchMove component and connected to it “FDBatchMoveDataSetReader” and “FDBatchMoveTextWriter”.
There is just one line of code under the button to call “BatchMove1.Execure” which takes data from the FireDAC “salaries” query and writes the output to the text file in “C:\Data” directory.
