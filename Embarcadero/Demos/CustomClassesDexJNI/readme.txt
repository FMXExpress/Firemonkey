This was originally part of the Developer Skill Sprint

Integrate More Android with a JNI Call to your Android App
from July 8th, 2014 by Jim McKeeth

For more skill sprints:
http://www.embarcadero.com/landing-pages/skill-sprints

For more information on this demo:
http://delphi.org/2014/07/custom-classes-dex/

The base64coder.jar comes from http://www.source-code.biz/base64coder/java/ 
Copyright 2003-2010 Christian d'Heureuse, Inventec Informatik AG, Zurich, Switzerland
www.source-code.biz, www.inventec.ch/chdh
Used under the BSD License or MIT License

To build the demo:

* Examine the createdex.bat file to make sure it refers to the correct location for your dx.bat utility and the fmx.jar & android-support-v4.jar files.
* Run the createdex.bat file to create the classes.dex file which includes the two jar files above, plus the base64coder.jar file.
* Double check that the Deployment Manager references the new classes.dex and not the old ones, and that the remote path is "classes\"
* Notice that the android.JNI.Base64Coder.pas file wraps and exposes the methods of the base64coder class.
* Run the app on your Android device and verify that it works as expected.

The Base64Coder.JAR is Android specific, so it will not work on iOS or Windows.