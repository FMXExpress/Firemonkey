Service example
===============

This project implements an Android service along with a couple of broadcast receivers to catch and respond to intent messages flung around the system. The service and receiver classes are implemented as extremely shallow Java classes, which need to be compiled and converted to an appropriate executable format (dex), and then merged into the other dex code deployed with an Android app.

The Java code calls into native Delphi code to implement the "business end" of the service and receivers.

Build the Java support files:
----------------------------

In the project's java directory are some source files:

 - java\src\com\blong\test\ActivityReceiver.java
 - java\src\com\blong\test\SampleService.java
 - java\src\com\blong\test\ServiceReceiver.java

These need to be compiled to .class files, archived into a .jar file, converted from Java byte code to DEX (Dalvik Executable) format and merged into the normally used (by Delphi's Android deployment process) classes.dex.

To set this up, follow these steps:

 - Ensure the useful subdirectory of Android SDK's build-tools directory is on the system PATH (e.g. C:\Android\android-sdk-windows\build-tools\18.0.1 or C:\Users\Public\Documents\RAD Studio\12.0\PlatformSDKs\adt-bundle-windows-x86-20130522\sdk\android-4.2.2)
 - Ensure the Java Development Kit's bin directory is on the system PATH (e.g. C:\Program Files (x86)\Java\jdk1.6.0_23\bin)
 - Run a command prompt in the project's java subdirectory and ensure you can successfully launch each of these:
    - javac
    - jar
    - dx
 - Review the build.bat file and ensure the environment variables are set correctly:
    - ANDROID needs to point to your Android SDK base directory, e.g. C:\Users\Public\Documents\RAD Studio\12.0\PlatformSDKs\adt-bundle-windows-x86-20130522\sdk or C:\Android\android-sdk-windows
    - ANDROID_PLATFORM needs to point at an installed SDK platform installation, e.g. %ANDROID%\platforms\android-15 or %ANDROID%\platforms\android-17. Check for one that is installed.
    - DX_LIB needs to point to the lib subdirectory under the Android SDK build-tools directory, e.g. %ANDROID%\build-tools\18.0.1\lib or %ANDROID%\build-tools\android-4.2.2\lib
    - EMBO_DEX needs to point to the Delphi-supplied Android classes.dex file, wrapped in quotes to take care of any spaces in the path, e.g. "C:\Program Files (x86)\Embarcadero\RAD Studio\12.0\lib\android\debug\classes.dex"
 - Run build.bat
 - You should now have a new file in the project directory tree called java\output\dex\classes.dex
 
This file replaces the supplied classes.dex and has the Java service and broadcast receiver compiled classes included in it.

Set the new classes.dex for deployment:
--------------------------------------

Open the project in the IDE
Choose Project | Deployment
Note that the classes.dex file from the project's java\output\dex directory is set to be deployed.
You must ensure that the default classes.dex file is de-selected. As you switch configurations, this de-selection will be lost (the file will be re-set to be deployed) and will need to again be de-selected. This is an IDE bug with Delphi XE5 RTM and XE5 UP1 and is logged in Quality Central as bug 118472.

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
