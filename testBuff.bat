export CDS_AUTO_64BIT=ALL
find worklib/ -type f  -name "*.*" -delete

xmvhdl -cdslib ./cds.lib -V93 ./src/packBuffer.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/TestBuffer.vhdl

xmelab -cdslib ./cds.lib -work worklib -access rwc worklib.ModuleTestsOfBuffer:arch

xmsim -GUI -cdslib ./cds.lib worklib.ModuleTestsOfBuffer:arch
