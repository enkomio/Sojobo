@echo off
cls

@rem Move to src directory
cd Src

@rem install fake
dotnet tool install fake-cli --tool-path fake

@rem paket.exe install
paket.exe install
if errorlevel 1 (
  exit /b %errorlevel%
)

".\fake\fake.exe" run build.fsx %*

cd ..