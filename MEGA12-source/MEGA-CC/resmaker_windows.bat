if "%1"=="true" goto build_open_source

lazres resources_windows.lrs @.\resource_file_list_windows.txt
goto finished

:build_open_source
lazres resources_windows.lrs @.\resource_file_list_windows_open_source.txt
if %ERRORLEVEL% NEQ 0 exit /B %ERRORLEVEL%
exit 0
:end

:finished
echo "resource built"
:end