export CDS_AUTO_64BIT=ALL
find worklib/ -type f  -name "*.*" -delete

xmvhdl -cdslib ./cds.lib -V93 ./src/analyzer.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/TestAnalyzer.vhdl

xmelab -cdslib ./cds.lib -work worklib -access rwc worklib.TestAnalyzer:main

xmsim -GUI -cdslib ./cds.lib worklib.TestAnalyzer:main
