@echo off
echo Cleaning...
rd bin /S /Q
md bin
echo.

REM Delphi XE2
set delphi=C:\Program Files (x86)\Embarcadero\RAD Studio\9.0
REM Delphi XE5
set delphi=C:\Program Files (x86)\Embarcadero\RAD Studio\12.0

echo Build Win32 CubeMan3D demo
"%delphi%\bin\dcc32.exe" -B -Ebin CubeMan3D.dpr
ren bin\CubeMan3D.exe CubeMan3D_Win32.exe
echo.

echo Build Win64 CubeMan3D demo
"%delphi%\bin\dcc64.exe" -B -Ebin CubeMan3D.dpr
ren bin\CubeMan3D.exe CubeMan3D_Win64.exe
echo.

echo Build OSX32 CubeMan3D demo
"%delphi%\bin\dccosx.exe" -B -Ebin CubeMan3D.dpr
echo.

echo Build OSX32 Bundle
cd bin
md CubeMan3D_OSX32.app\Contents\MacOS
move CubeMan3D CubeMan3D_OSX32.app\Contents\MacOS\CubeMan3D_OSX32
copy "%delphi\binosx32\libcgunwind.1.0.dylib" CubeMan3D_OSX32.app\Contents\MacOS\libcgunwind.1.0.dylib
..\zip -r -9 -m CubeMan3D_OSX32.zip CubeMan3D_OSX32.app
cd..
echo.

pause