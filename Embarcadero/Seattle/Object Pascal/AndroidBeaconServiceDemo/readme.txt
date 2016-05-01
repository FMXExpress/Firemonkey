Steps to compile this demo.

1.- Open BeaconServiceDemo.groupproj
2.- Compile android service: libBeaconService.so
3.- Add android service to BeaconServiceApp
    a) Active BeaconServiceApp project
    b) Expand Target Platforms node
	c) Active Android platform
	d) Right click on Android platform node
	c) Click on Add Android Service... 
	e) Select BeaconService folder, click Next
	f) You have to see  libBeaconService.so, BeaconService.jar and BeaconServiceUnit.pas files. Click finish.
4.- Run BeaconServiceApp

Notice that you will have to set the MonitorizedRegions in the beacon component according to beacon UUID
