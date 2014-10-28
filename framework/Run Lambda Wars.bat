@echo off
:Build
@echo Building Lambda Wars
cabal install
:Choice
choice /c QRP /n /m "[Q]uit, [R]ebuild, or [P]lay?"
if errorlevel 3 goto Play
if errorlevel 2 goto Build
if errorlevel 1 goto End
:Play
@echo Starting Lambda Wars
start /d "dist\build\lambda-wars" lambda-wars.exe
goto Choice
:End