This is modified from the original Leap Motion download by Michael Van Canneyt.

His original version is available via the Blasie Pascal Magazine  
www.BlaisePascal.eu 
Issue NR 31/32 pgs 33-39 by Michaël Van Canneyt

Changes were tested with Delphi XE5 by Jim McKeeth jim.mckeeth@embarcadero.com

-----


This package provides a connection to the Leap Motion device.

The package works for Delphi (7, but higher should not present any problems) and FPC/Lazarus

The package contains the following files:

delphileapr.dpk
 A Delphi 7 runtime package that contains the delphi version of the Leap motion controller.
 (This package can be used in newer Delphis too)

delphileapd.dpk
 A Delphi 7 design-time package that installs the Leap motion controller on the component palette.

 
The Leap Motion controller relies on the Bauglir Web Socket implementation. 
The Delphi\Bauglirwebsocket directory contains a Lazarus and Delphi package for this component.
(dsynapse for delphi, laz_synapse for delphi)

The Bauglir websocket implementation is based on the Synapse TCP/IP library.
The Delphi\synapse directory contains a Lazarus and Delphi package for this component.

The Delphi version of the leap motion component depends on the SuperObject JSON object.
(because this component still supports Delphi 7). The Lazarus version of the leap motion
relies on the standard JSON support of Free Pascal.

If you already use synapse, bauglir websockets or superobject in Delphi, it is recommended to use
your own version of these libraries, and adapt the dependencies of the delphileapr package.
(and possibly the lazarus package too)

leapmotion.bpg
 A Delphi Project Group that groups all Delphi packages and delphi projects.
 
The component itself is implemented in the following units:

leapdata: Contains all the Leap Motion data model structures, and the abstract TLeapController class.
wsleap: Contains the Websocket version of the Leap Controller.
leapjson: a Lazarus version of the JSON to Leap data conversion routines.
dleapjson: a Delphi version of the JSON to Leap data conversion routines.

There are several demo programs:
Lazarus:
--------
- Testleap: a simple test program.
- Fingers: a test program that draws the tips of pointables as circles on the screen.
- TapDemo: a test program that mimics a POS application and uses Tap Gesture recognition to
   select items.
- SwipeDemo: a test program that mimics a media playlist and uses Swipe Gesture recognition to
   scroll through a grid.
- CircleDemo: a test program that mimics a foto viewing application and uses Circle Gesture recognition to
   rotate a picture.   
   
Delphi:
-------
- Fingers: a test program that draws the tips of pointables as circles on the screen.

The other Lazarus applications should be easily converted to a Delphi program by renaming the 
.lfm file to a .dfm file, and converting any {$R *.lfm} directives to {$R *.dfm}