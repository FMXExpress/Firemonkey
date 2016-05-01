Steps to compile this demo.

1.- Open NotificationServiceDemo.groupproj
2.- Compile android service: libNotificationService.so
3.- Add android service to NotificationApp
    a) Active NotificationApp project
    b) Expand Target Platforms node
	c) Active Android platform
	d) Rigth click on Android platform node
	c) Click on Add Android Service... 
	e) Select NotificationService folder, click Next
	f) You have to see  libNotificationService.so, NotificationService.jar and NotificationServiceUnit.pas files. Click finish.
4.- Run NotificationApp