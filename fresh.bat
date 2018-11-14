# Clean up the directory after a LaTeX build. Windows version
del example*.pdf
del example*.log
del example*.aux
del example*.bbl
del example*.blg
del example*.tex
del example*.toc
del example*.upa
rmdir /S /Q knitr-cache-tex
rmdir /S /Q knitr-figs