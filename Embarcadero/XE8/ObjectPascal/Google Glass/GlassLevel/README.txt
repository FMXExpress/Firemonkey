This sample project demonstrates the following:

*  Voice launching an app on Google Glass
*  Use of the Google Glass style
*  Use of a WakeLock to keep an Android device awake
*  Other common Google Glass behavior

Overview
--------

You provide the input to this app by moving your head (while wearing Google
Glass). As you tip your head from left to right you will see the line also tip.
As your lean your head forward and backward you will see the line also move up
and down. The value of the lines movement is displayed on the lower corners.

Voice Launching on Google Glass
-------------------------------

The voice trigger is added in multiple steps. In the
AndroidManifest.template.xml file the following lines were added:

Inside the <intent-filter> element
<action
  android:name="com.google.android.glass.action.VOICE_TRIGGER" />


Inside the <activity> element
<meta-data
  android:name="com.google.android.glass.VoiceTrigger"
  android:resource="@xml/voice_trigger_start" />

Right below the <%uses-permission%> line you need to add the following special
permission to use a non-standard voice trigger command.
<uses-permission
  android:name="com.google.android.glass.permission.DEVELOPMENT" />

The @xml/voice_trigger_start is a reference to the voice_trigger_start.xml file,
which is also included in the project.

It is a standard XML file with the element:
<trigger keyword="Am I level?" />

Where "Am I level?" is the voice trigger keyword.

The voice_trigger_start.xml also requires deployment to the res\xml folder. This
is specified under the Project -> Deployment screen for the Android platform.

For more information on the Google Glass Voice Trigger, check out the GDK
documentation:
https://developers.google.com/glass/develop/gdk/input/voice


Use of the Google Glass Style
-----------------------------

The Google Glass style is added to a project by dropping a TStyleBook, double
clicking on it, and selecting the Google Glass style file found in the styles
folder:

C:\Users\Public\Documents\Studio\14.0\Styles\Android

Then on the form select the new TStyleBook via the StyleBook property.

The Google Glass Style provides common visual elements found on Google Glass,
for example a black background with large colorful text. For more information on
the style guidelines see:

https://developers.google.com/glass/design/style/index


Use of a WakeLock to Keep an Android Device Awake
-------------------------------------------------

The WakeLock is technically deprecated on Android, but is still the best way to
have the most granular control over when and how a device stays away from your
app.

The unit Androidapi.JNI.PowerManager.pas contains the JNI Interface for the
PowerManager and WakeLock classes. It also contains a few helper methods for
acquiring a WakeLock. It is important that your app always releases any WakeLock
it acquires. The code makes use of the Application Events Platform Service to
release and acquire the WakeLock at the appropriate times.

To obtain WakeLock Uses Permission go to Project > Options menu from within the 
IDE and then Uses Permissions. At the top select "All configurations - Android
platform" and then select "Wake Lock" from the list. (It is already selected 
for this sample.)


Other Common Google Glass Behavior
----------------------------------

This project uses the Google Glass skin, which is available via the dropdown
from the Design view. The skin is 427x240. The Google Glass display is 640x360
at 1.5 pixel density (640/1.5=427).

Google Glass apps typically don’t have a title bar. This is disabled in two
places. From the design surface, select the Form and change the BorderStyle
property to None and this will hide the title bar at design time.

For runtime, go to Project -> Options -> Version Info and select
"All configurations – Android platform" at the top and change the "theme" key's
value to "No TitleBar".

Glass does not have an app management interface. To uninstall your app use the
following command-line:

adb shell pm uninstall com.embarcadero.GlassLevel

You need to make sure adb is available in the path. By default it is found in
C:\Users\Public\Documents\Studio\14.0\PlatformSDKs\adt-bundle-windows-x86-20131030\sdk\platform-tools




