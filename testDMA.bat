export CDS_AUTO_64BIT=ALL
find worklib/ -type f  -name "*.*" -delete

xmvhdl -cdslib ./cds.lib -V93 ./src/analyzer.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/inputBlock.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/packBuffer.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/writer.vhdl 
xmvhdl -cdslib ./cds.lib -V93 ./src/registersBlock.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/DMA.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/TestDMA.vhdl

xmelab -cdslib ./cds.lib -work worklib -access rwc worklib.TestDMA:arch

xmsim -GUI -cdslib ./cds.lib worklib.TestDMA:arch
#sleep 20