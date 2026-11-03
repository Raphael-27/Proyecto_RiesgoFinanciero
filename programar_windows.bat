@echo off
rem Programa el ETL en el Programador de tareas de Windows (reemplaza a Airflow, que es de Python).
rem Se ejecuta de lunes a viernes a la hora indicada abajo (HORA LOCAL DE ESTE EQUIPO).
rem La hora debe quedar ~1 h 30 min despues del cierre del mercado (16:00 Nueva York) para cumplir la
rem metrica de oportunidad (<= 2 h). En Ciudad de Mexico: 16:30 en invierno de EE. UU., 15:30 en verano.
set HORA=16:30

for /f "delims=" %%i in ('where Rscript 2^>nul') do set RSCRIPT=%%i
if "%RSCRIPT%"=="" (
  echo No se encontro Rscript. Agrega la carpeta bin de R al PATH, por ejemplo C:\Program Files\R\R-4.x.x\bin
  exit /b 1
)
schtasks /Create /SC WEEKLY /D MON,TUE,WED,THU,FRI /ST %HORA% /TN "ETL_Riesgo_Financiero" /F ^
  /TR "cmd /c cd /d \"%~dp0\" && \"%RSCRIPT%\" pipeline.R >> data\log_etl.txt 2>&1"
if errorlevel 1 (echo No se pudo crear la tarea. Ejecuta este archivo como administrador.) else (echo Tarea creada: ETL_Riesgo_Financiero a las %HORA% de lunes a viernes. Registro en data\log_etl.txt)
rem Para quitarla:  schtasks /Delete /TN "ETL_Riesgo_Financiero" /F
