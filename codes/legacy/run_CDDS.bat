@echo off
pushd %~dp0

:: Delete everything in the ./Condor folder
echo Cleaning up ./Condor folder...
if exist ".\Condor\" (
    rmdir /s /q ".\Condor"
    mkdir ".\Condor"
    echo Folder cleaned.
) else (
    echo No existing folder found. Creating new one...
    mkdir ".\Condor"
)

:: Run the first R script, stop if it fails
Rscript ..\Condor_run_basic.R config_CDDS.R || (
    echo First script failed. Exiting...
    popd
    exit /b 1
)

:: Run the second R script
Rscript ..\Condor_run_stats.R config_CDDS.R