@echo off

setlocal

rem if x%ANDROID% == x set ANDROID=C:\Android\android-sdk-windows
rem set ANDROID_PLATFORM=%ANDROID%\platforms\android-15
rem set DX_LIB=%ANDROID%\build-tools\18.0.1\lib

rem if x%ANDROID% == x set ANDROID=C:\Users\Public\Documents\RAD Studio\12.0\PlatformSDKs\adt-bundle-windows-x86-20130522\sdk
rem set ANDROID_PLATFORM=%ANDROID%\platforms\android-17
rem set DX_LIB=%ANDROID%\build-tools\android-4.2.2\lib

if x%ANDROID% == x set ANDROID=C:\Android\adt-bundle-windows-x86-20130522\sdk
set ANDROID_PLATFORM=%ANDROID%\platforms\android-17
set DX_LIB=%ANDROID%\build-tools\android-4.2.2\lib

set EMBO_DEX="C:\Program Files (x86)\Embarcadero\RAD Studio\12.0\lib\android\debug\classes.dex"
set PROJ_DIR=%CD%
set VERBOSE=0

echo.
echo Compiling the Java service activity source files
echo.
mkdir output 2> nul
mkdir output\classes 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=-verbose
javac %VERBOSE_FLAG% -source 1.6 -target 1.6 -Xlint:deprecation -cp %ANDROID_PLATFORM%\android.jar -d output\classes src\com\blong\test\ActivityReceiver.java src\com\blong\test\SampleService.java src\com\blong\test\ServiceReceiver.java

echo.
echo Creating jar containing the new classes
echo.
mkdir output\jar 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=v
jar c%VERBOSE_FLAG%f output\jar\test_classes.jar -C output\classes com

echo.
echo Converting from jar to dex...
echo.
mkdir output\dex 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=--verbose
call dx --dex %VERBOSE_FLAG% --output=%PROJ_DIR%\output\dex\test_classes.dex --positions=lines %PROJ_DIR%\output\jar\test_classes.jar

echo.
echo Merging dex files
echo.
java -cp %DX_LIB%\dx.jar com.android.dx.merge.DexMerger %PROJ_DIR%\output\dex\classes.dex %PROJ_DIR%\output\dex\test_classes.dex %EMBO_DEX%

echo Tidying up
echo.
del output\classes\com\blong\test\ActivityReceiver.class
del output\classes\com\blong\test\SampleService.class
del output\classes\com\blong\test\ServiceReceiver.class
rmdir output\classes\com\blong\test
rmdir output\classes\com\blong
rmdir output\classes\com
rmdir output\classes
del output\dex\test_classes.dex
del output\jar\test_classes.jar
rmdir output\jar

echo.
echo Now we have the end result, which is output\dex\classes.dex

:Exit

endlocal
