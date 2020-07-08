export CDS_AUTO_64BIT=ALL
find worklib/ -type f  -name "*.*" -delete

xmvhdl -cdslib ./cds.lib -V93 ./src/writer.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/registersBlock.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/modelMemory.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/TestWriter.vhdl

xmelab -cdslib ./cds.lib -work worklib -access rwc worklib.TestWriter:main

xmsim -GUI -cdslib ./cds.lib worklib.TestWriter:main
#sleep 20