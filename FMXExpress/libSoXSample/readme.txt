Delphi XE8 libSOX Multi Platform Example
http://www.fmxexpress.com/

Find out more about SoX here:
http://sox.sourceforge.net/
SoX is licensed under the GNU GPL and GNU LGPL.

The header file was translated from SoX 14.4.1.


Description: Delphi XE8 translation of the libSOX header file for Android, IOS, OSX, and Windows.

Windows 32:
Deploy the LibSOX32.dll file to the same directory as the EXE.

Windows 64:
Deploy the LibSOX64.dll file to the same directory as the EXE.

IOS32+IOS64:
The libsox.a static library must be in the project directory when you compile.

Mac OSX:
Deploy the libsox.1.dylib file to the project directory.

Android:
Deploy the following shared objects to assets\internal\

liblpc10.so, libgsm.so, libogg.so, libvorbis.so, libvorbisenc.so, libvorbisfile.so, libFLAC.so, 
libmp3lame.so, libmad.so, libpng.so, libsmr.so, libsmrx.so, libsndfile.so, libwavpack.so, 
libfmemopen.so, libsox.so


Sample application:

For the sample deploy bwoman.au to the same location as your executable. On IOS deploy to Startup\Documents.
On Android deploy to assets\internal\.