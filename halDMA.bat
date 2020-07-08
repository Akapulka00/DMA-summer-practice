export CDS_AUTO_64BIT=ALL
path_to_ncsim=$(which ncsim)
path_to_ncsim_suffix="bin/ncsim"
find worklib/ -type f  -name "*.*" -delete

xmvhdl -cdslib ./cds.lib -V93 ./src/analyzer.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/inputBlock.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/packBuffer.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/writer.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/registersBlock.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/DMA.vhdl

xmelab -cdslib ./cds.lib -work worklib -access rwc worklib.DMA:main

hal -logfile hal.log -cdslib ./cds.lib worklib.DMA:main  -gui
