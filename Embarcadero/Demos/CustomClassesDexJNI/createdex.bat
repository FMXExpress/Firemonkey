@echo off

REM /-------------------------------------------------------------------
REM | This software is Copyright (c) 2014 Embarcadero Technologies, Inc.        
REM | You may only use this software if you are an authorized licensee          
REM | of one of Embarcadero's developer tools products.
REM | This software is considered a Redistributable as defined under
REM | the software license agreement that comes with the Embarcadero Products
REM | and is subject to that software license agreement.
REM \-------------------------------------------------------------------

call "C:\Users\Public\Documents\Embarcadero\Studio\14.0\PlatformSDKs\adt-bundle-windows-x86-20131030\sdk\build-tools\19.1.0\dx.bat" --dex --output="classes.dex" "C:\Program Files (x86)\Embarcadero\Studio\14.0\lib\android\release\android-support-v4.jar" "C:\Program Files (x86)\Embarcadero\Studio\14.0\lib\android\release\fmx.jar" "base64coder.jar"
dir classes.dex
