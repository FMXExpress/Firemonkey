@echo off
setlocal

REM ----------------------------------------------------------------
REM Assumes 'win32\debug' and 'win64\debug' exist (created by IDE)
REM ----------------------------------------------------------------

:WIN64
set SOURCES=*.cpp tests\TestEntries.cpp
set OPTS=-I%TP%\runtime -L%TP%\lib\win64\debug\psdk
call %TP%\emuvcl\runb64.bat %OPTS% -tWCF %SOURCES% -o win64\debug\TestBed.exe

:WIN32
call %TP%\emuvcl\runbcc.bat -tWCF -I%TP%\runtime -ewin32\debug\Testbed.exe *.cpp tests\*.cpp