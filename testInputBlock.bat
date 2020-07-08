export CDS_AUTO_64BIT=ALL
find worklib/ -type f  -name "*.*" -delete

xmvhdl -cdslib ./cds.lib -V93 ./src/inputBlock.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/modelNetworkController.vhdl
xmvhdl -cdslib ./cds.lib -V93 ./src/TestInputBlock.vhdl

xmelab -cdslib ./cds.lib -work worklib -access rwc worklib.TestInputBlock:main

xmsim -GUI -cdslib ./cds.lib worklib.TestInputBlock:main
