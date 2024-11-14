@echo off
setlocal enabledelayedexpansion
set count=1
for %%f in (*) do (
    ren "%%f" "!count!_b%%~xf"
    set /a count+=1
)
