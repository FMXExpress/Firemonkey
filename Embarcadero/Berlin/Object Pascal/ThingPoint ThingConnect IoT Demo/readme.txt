EMS Thingpoint Sample modified to use Polar H7 as source for heart rate monitor. (09/25/2015)
7/24/15

Projects
========

ThingpointProject.dpr, ThingpointProjectCpp.cbproj
--------------------------------------------------

This application does three things:  Simulates data generated from things, caches data from things, and responds to remote requests for data.

1. Simulates data

A heart rate monitor is simulated by continuously generating a random integer value.  A blood pressure monitor is simulating by generating random values when a button is clicked.

2. Caches data

All data from the things is cached in memory.  Each cache entry includes a time stamp.

3. Responds to data requests

The TEMSEdgeService component is used to register an edgemodule with EMS.  EMS clients and custom resources may make requests to edgemodules through the EMS API.

TMeasurementsResource implements an edgemodule resource that has a two GET endpoints.  Both endpoints responds with a JSON array of measurements.  The array includes the most recent data value in the cache for each thing, along with a timestamp.  One of endpoints also responds with mean and standar deviation. 


ClientProject.dpr
-----------------

This application makes requests for thing data.  Two kinds of requests are demonstrated.

1. Custom Resource Request 

The client makes a request to a custom resource in the EMS server.  The custom resource responds with a summary of all data from all edgemodules.

2. Edge Request

The client makes a request to a particular edgemodule.  The EMS server is responsible for routing the request to the named edgemodule.

MobileClientProject.dpr
-----------------

Similar to ClientProject.dpr in functionality.  Has a mobile application UI.  Requests detailed data, or normal data. Detailed data includes mean and standard deviation.


CustomResourcePackage.dpr
-------------------------

This package implements an EMS custom resource.

The custom resource implements a single GET method that responds with a JSON array containing the thing data for each edgemodule.  The implementation of this method queries for the names of edgemodules, then makes a request to each.

Using the sample applications
=============================

Start projects
--------------
1. Open CustomResourcePackage.dpr, ClientProject.dpr and ThingpointProject.dpr
2. Run CustomResourcePackage.dpr without debugger.  This should start EMSDevServer and register the custom resource in CustomResourcePackage.bpl.
3. Run ThingpointProject.dpr without debugger
3.1  Test the connection to the EMS server
3.2  Enter a name for the edgemodule, activate the edgemodule, and test.
4. (Optional) Run another instance of ThingpointProject (using windows explorer).  Enter a diffent name and different port.
5. Run ClientProject
5.1 Test the connection to the EMS server

Generate data
-------------
1. In Thingpoint window, click the "Heart Rate" tab.  
1.1 Click "Start" to continously generate bpm.
2. Click the "Blood Pressure" tab.  
2.1 Click "Notify" to generate a single blood pressure measurement.

Request data
------------
1. In Client window, click the "Edge Request" tab. The combo box should be populated with the edgemodule names.
1.1 Click "Execute".  The grid should show most recent data from the selected Thingpoint.
2 Click "Custom Resource Request" tab.
2.1 Click "Execute".  The grid should show most recent data from all Thingpoints. Note that execute will fail if CustomResourcePackage.bpl has not be registered with EMSDevServer.
3. Check "Auto Refresh" to continuously execute requests on a timer event.
  
