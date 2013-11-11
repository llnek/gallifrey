@echo off

set CWD=%CD%
set ARGC=0
for %%x in (%*) do set /A ARGC+=1

set GALLIFREY_HOME=%~dp0..
set BINDIR=%~dp0
set PATCHDIR=%GALLIFREY_HOME%\patch\*
set DISTRO=%GALLIFREY_HOME%\dist\exec\*
set BOOT=%GALLIFREY_HOME%\dist\boot\*
set LIBDIR=%GALLIFREY_HOME%\lib\*

set BCP=%BOOT%;%LIBDIR%;%CLASSPATH%
set LOG4J=etc\log\logback.xml
set L4JFILE=%CD%\%LOG4J%
set L4J=file:/%L4JFILE%
set LOGCFG=%L4J:\=/%
set LOGREF=-Dlogback.configurationFile=%LOGCFG%
set BASEDIR=-Dgallifrey.home=%GALLIFREY_HOME%
set BG=false
set DBGOPTS=
set ECODE=0
set KPORT=4444
set KILLPORT=-Dgallifrey.kill.port=%KPORT%
set LIBP=-Djava.library.path=$GALLIFREY_HOME/bin
set NETTY=-Dio.netty.eventLoopThreads=16

set JPROF=-agentpath:/Applications/jprofiler7/bin/macos/libjprofilerti.jnilib=port=8849
set VMXRGS=-XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:MaxPermSize=256m
set CLDR=-Djava.system.class.loader=com.zotoh.gallifrey.loaders.ExecClassLoader
set MAINCZ=comzotohcljc.tardis.etc.core

if NOT EXIST %L4JFILE% SET LOGREF=

REM 48G
REM VMARGS=-Xms8192m -Xmx49152m
REM 36G
REM VMARGS=-Xms8192m -Xmx36864m
REM 32G
REM VMARGS=-Xms8192m -Xmx32768m
set VMARGS=-Xms512m -Xmx4096m %VMXRGS%

if "%JAVA_HOME%" == "" goto noJavaHome
:b0
set JAVA_CMD=%JAVA_HOME%\bin\java.exe

if %ARGC% EQU 2 goto testStartBG
:b1

if %ARGC% EQU 1 goto testDebug
:b2

REM ********************************************************
REM run in foreground
REM ********************************************************
cd %BINDIR%
:appfg
REM CMDLINE="%JAVA_CMD%" -cp "%BCP%" "%LIBP%" %DBGOPTS% "%LOGREF%" "%NETTY%" "%KILLPORT%" "%BASEDIR%" %CLDR% %MAINCZ% "%GALLIFREY_HOME%" %*
if %BG% == "true" goto runcmd
call :splash
:runcmd
"%JAVA_CMD%" -cp "%BCP%" "%LIBP%" %DBGOPTS% "%LOGREF%" "%NETTY%" "%KILLPORT%" "%BASEDIR%" %CLDR% %MAINCZ% "%GALLIFREY_HOME%" %*
set ECODE=%ERRORLEVEL%
goto end

REM ********************************************************
REM run in background
REM ********************************************************
:appbg
goto end


REM ********************************************************
REM test for start in background
REM ********************************************************
:testStartBG
if "%1%2" == "startbg" set BG=true
goto b1

REM ********************************************************
REM test for debug mode
REM ********************************************************
:testDebug
if "%1" == "debug" set DBGOPTS=-agentlib:jdwp=transport=dt_socket,server=y,address=8787,suspend=n
goto b2


REM ********************************************************
REM set java_home
REM ********************************************************
:j764
set JAVA_HOME=C:\Program Files\Java\jre7
goto b0
:j664
set JAVA_HOME=C:\Program Files\Java\jre6
goto b0
:j732
set JAVA_HOME=C:\Program Files (x86)\Java\jre7
goto b0
:j632
set JAVA_HOME=C:\Program Files (x86)\Java\jre6
goto b0


REM ********************************************************
REM test for java_home
REM ********************************************************
:noJavaHome
echo No JAVA_HOME set, attempt to reference standard java location.
if exist "C:\Program Files\Java\jre7" goto j764
if exist "C:\Program Files\Java\jre6" goto j664
if exist "C:\Program Files (x86)\Java\jre7" goto j732
if exist "C:\Program Files (x86)\Java\jre6" goto j632
echo Please set JAVA_HOME


:splash
  echo   ____   ____  _      _      ____  _____  ____     ___  __ __ 
  echo  /    T /    T| T    | T    l    j|     ||    \   /  _]|  T  T
  echo Y   __jY  o  || |    | |     |  T |   __j|  D  ) /  [_ |  |  |
  echo |  T  ||     || l___ | l___  |  | |  l_  |    / Y    _]|  ~  |
  echo |  l_ ||  _  ||     T|     T |  | |   _] |    \ |   [_ l___, |
  echo |     ||  |  ||     ||     | j  l |  T   |  .  Y|     T|     !
  echo l___,_jl__j__jl_____jl_____j|____jl__j   l__j\_jl_____jl____/ 
goto runcmd


REM ********************************************************
REM eof
REM ********************************************************
:end
cd %CWD%
exit /B %ECODE%



