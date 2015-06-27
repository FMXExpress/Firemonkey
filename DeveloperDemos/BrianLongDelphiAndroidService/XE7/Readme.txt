Service example
===============

This project implements an Android service along with a couple of broadcast receivers to catch and respond to intent messages flung around the system. The service and receiver classes are implemented as extremely shallow Java classes, which need to be compiled and archived and are then included in the Delphi project.

The Java code calls into native Delphi code to implement the "business end" of the service and receivers.

Build the Java support files:
----------------------------

In the project's java directory are some source files:

 - java\src\com\blong\test\ActivityReceiver.java
 - java\src\com\blong\test\SampleService.java
 - java\src\com\blong\test\ServiceReceiver.java

These need to be compiled to .class files and then archived into a .jar file, which is to be merged into the Android package for this sample app.

To set this up, follow these steps:

 - Review the build.bat file and ensure the environment variables are set correctly:
    - ANDROID needs to point to your Android SDK base directory, e.g. C:\Users\Public\Documents\RAD Studio\12.0\PlatformSDKs\adt-bundle-windows-x86-20130522\sdk or C:\Android\android-sdk-windows
    - ANDROID_PLATFORM needs to point at an installed SDK platform installation, e.g. %ANDROID%\platforms\android-15 or %ANDROID%\platforms\android-17. Check for one that is installed.
    - JAVA needs to point to the JDK bin directory, where javac.exe and jar.exe reside
    - EMBO_LIB needs to point to the subdirectory of Delphi's lib directory containing fmx.jar
 - Run build.bat
 - You should now have a new file in the project directory tree called java\output\jar\Delphi_Service.jar
 
Add the Java library to the project
-----------------------------------

This file is added to the project in the Project Manager, under Target Platforms, Android, Libraries.
This is already done in the demo.

Register the service:
---------------------

This has already been done for this demo. It involves the following:

 - Open the Android manifest template, AndroidManifest.template.xml, which is generated on first compile, from the project directory
 - Add in a description of the service in the application element:
 
<service android:name="com.blong.test.SampleService" />

Build the Delphi Android application library:
--------------------------------------------

In the IDE choose Project | Compile ServiceApp (or press Ctrl+F9)

Deploy the library to an Android application package (.apk):
-----------------------------------------------------------

In the IDE choose Project | Deploy libServiceApp.so

Install the app on the device:
-----------------------------

In the IDE choose Run | Run Without Debugging (or press Ctrl+Shift+F9)
This will implicitly do the compile and deploy steps above, so they are actually optional.
This step will uninstall the app if already installed and then install the newly built version.
