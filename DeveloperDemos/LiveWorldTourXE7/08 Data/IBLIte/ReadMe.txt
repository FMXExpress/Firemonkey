This demo is aimed to show IBLite working on all platforms. 

You will need to
1) Get IBLite Licenses
2) Place it in C:\Users\Public\Documents\Embarcadero\InterBase\redist\InterBaseXE3 and name it reg_iblite.txt
Video notes on this here:
http://delphiaball.co.uk/2014/09/03/rad-studio-xe7-includes-iblite-for-all-platforms/ 

The application creates a database on the fly so no need to put any files else where. 

To make it easier to demo, I suggest setting up PAServer on Windows and using that for a loop back to run the application. 
Blog notes on the steps here:
http://delphiaball.co.uk/2014/09/04/debugging-to-pa-server-on-windows/

The Demo:
In short, it creates a database with two tables (Foo and Fee), and allows you to navigate the data and edit it. You can also connect to a remote InterBase server (using IBLite as a client) to view data directly which is *unique* to InterBase from mobile with our components. If you connect to a remote database it will not add sample data, if you connect to one with a full path it will not.

To demo, launch the application, open the Multi-view and click connect. (If you wish to connect to a remote database, put the InterBase path in (e.g. 192.168.0.1:c:\data\mydata.ib)

You can then select from the multi-view a table name and that will run 'Select * from <tablename>'
The rest should be self explanitory, but make some changes, close and re-open show persistance etc. 
