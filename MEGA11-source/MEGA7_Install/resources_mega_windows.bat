if "%1"=="true" goto build_open_source

;;echo off
echo "deleting old files"
echo "removing %localappdata%\MEGAX\MEGA11*\Private\"
for /D %%G in ("%localappdata%\MEGAX\MEGA11*") do (
	if exist "%%G\Private\" (
		echo "removing %%G\Private\"
		rmdir /S /Q "%%G\Private\"
	)
)

..\lazres.exe ..\resources_mega_windows.lrs @.\resource_file_list_windows.txt

goto skip_open_source

:build_open_source
lazres ..\resources_mega_windows.lrs @.\resource_file_list_windows_open_source.txt
if %ERRORLEVEL% NEQ 0 exit /B %ERRORLEVEL%
exit 0
:end

:skip_open_source
	echo "finished:
: