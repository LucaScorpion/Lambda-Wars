@echo off
:Choice
choice /c QBCP /n /m "[Q]uit, [B]uild, [C]opy resources, or [P]lay?"
if errorlevel 4 goto Play
if errorlevel 3 goto Copy
if errorlevel 2 goto Build
if errorlevel 1 goto End
:Build
@echo Building Lambda Wars
cabal install
goto Choice
:Copy
@echo Copying resources
xcopy /s /i ".\src\Resources" ".\dist\Build\lambda-wars\lambda-wars-tmp\Resources"
goto Choice
:Play
@echo Starting Lambda Wars
start /d "dist\build\lambda-wars" lambda-wars.exe
goto Choice
:End