BaaS ToDo List Demo Application Readme 

The ToDo List BaaS demo allows you to post and get data from your data collection using Kinvey as your 
Backend As a Service Provider. This demo requires that you sign up for an account on Kinvey.com.
Once you have signed up for an account, you will need to enter the App Key, App Secret and Master Key 
on the KinveyProvider component on the Data Module. 

While this demo uses Kinvey as the Backend As a Service Provider, you can also use the Parse Provider 
component instead by deleting the Kinvey provider component and connecting it to the Backend Storage component.

Once you have signed up for a Kinvey account, and have the application deployed to your device, you will 
be able to see your data by logging into your account on Kinvey.com, and going to Add-Ons->Data & Storage ->Data Store. 

BaaS requires Open SSL. You will need to get the necessary library files onto your file system before deploying 
your application. 


Windows:
You can install the SSL library and copy the libeay32.dll and ssleay32.dll files to your system path.  
A link to the SSL library can be accessed from our documentation at:
 
http://docwiki.embarcadero.com/RADStudio/XE5/en/Tutorial:_Using_the_REST_Client_Library_to_Access_REST-based_Web_Services 


iOS Device: 
You need the libcrypto.a and libssl.a SSL library files on your system. You can get them by going to:  

https://github.com/st3fan/ios-openssl

Download the zip from the github URL and then extract it to find the .a files in the lib directory. 
Once you have the files, just put them into both the Release and Debug folders under 
C:\Program Files (x86)\Embarcadero\Studio\14.0\lib\iosDevice . If the files are missing, you will get a linker error.
For C++, also make sure to go to Project->Options->C++ Linker and then check 'Link with SSL and Crypto'.


iOS Simulator, Mac OS X and Android: 
There are no additional steps since the files are already available on your file system.


