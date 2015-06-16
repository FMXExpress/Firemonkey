
INSTRUCTIONS TO USE THE TMSFMXPLANNER CLOUD CALENDAR DEMO


Introduction
------------
TMSFMXPLANNER can connect to the Google Live Calendar or Microsoft Live Calendar
This is done via the TMS Cloud Pack for FireMonkey components TTMSFMXCloudGCalendar and TTMSFMXCloudLiveCalendar and non-visual components: TTMSFMXPlannerGoogleAdapter & TTMSFMXPlannerLiveAdapter. 
The components TTMSFMXPlannerGoogleAdapter & TTMSFMXPlannerLiveAdapter are not installed by default as it cannot be assumed that TMS Cloud Pack for FireMonkey will be always installed.

Preparing setup
---------------
Make sure the latest version of TMS Pack for FireMonkey is installed.
Then install TMS Cloud Pack for FireMonkey (http://www.tmssoftware.com/site/tmsfmxcloudpack.asp)

From the IDE, create a new package and add the files FMX.TMSPlannerGoogleAdapter.pas, FMX.TMSPlannerLiveAdapter.pas, FMX.TMSPlannerAdapterReg.pas from the TMS Pack for FireMonkey source folder. Then compile & install this package in the IDE. The IDE should have automatically added references to the TMS Pack for FireMonkey and TMS Cloud Pack for FireMonkey package files to the requires list. If the IDE did not do this automatically, add TMSFMXPackPkgDXE*.dcp and TMSFMXCloudPackPkgDXE*.dcp to the requires list (* = 5,6,7 or 8 depending on the version of the IDE being used)

When this package is compiled & installed in the IDE, the components TTMSFMXPlannerGoogleAdapter & TTMSFMXPlannerLiveAdapter should be available on the tool palette and the demo can be opened & used. 

Registering the application for Google Calendar and/or Microsoft Live Calendar
------------------------------------------------------------------------------
Follow the steps in the TMS Cloud Pack for FireMonkey documentation to register for an application key & secret for Google Calendar and/or Microsoft Live Calendar and set this key in the file APPIDS.INC in the demo source folder:


//please specify the keys here

const
  GAppkey = ;
  GAppSecret = ;

  LiveAppkey = ;
  LiveAppSecret = ;

With these IDs correctly specified, the demo is ready to run and connect to a cloud calendar.
