@echo off
if exist ".\dist" goto Play
@echo Building Lambda Wars
cabal install
@echo Copying resources
xcopy /s /i ".\src\Resources" ".\dist\Build\lambda-wars\lambda-wars-tmp\Resources"
:Play
@echo Starting Lambda Wars
start /d ".\dist\build\lambda-wars" lambda-wars.exe