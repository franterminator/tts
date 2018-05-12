ECHO off
CLS


:MENU
ECHO.
ECHO ...............................................
ECHO PRESS 1, 2 OR 3 to select your task, or 4 to EXIT.
ECHO ...............................................
ECHO.
ECHO 1 - Open Grapher
ECHO 2 - Open justOne and magickimage
ECHO 3 - Open pointTrack
ECHO 4 - EXIT
ECHO.


SET /P M=Type 1, 2, 3, or 4 then press ENTER:
IF %M%==1 GOTO GRAPHER
IF %M%==2 GOTO JUSTONE
IF %M%==3 GOTO POINTTRACK
IF %M%==4 GOTO EOF


:GRAPHER
SET /P L=Type the relative path of the results file (include the name of the file) then press ENTER:
python grapher.py %L%
GOTO MENU

:JUSTONE
python justOne.py img/filenames_step.txt
cd img
magick @filenames_step.txt resultados.gif
GOTO MENU

:POINTTRACK
python pointtrack.py img/points_track.txt
GOTO MENU