# DMA-summer-practice
 Test task on my first work:)

Develop RTL model and execute check synthesizability (HAL) of DMA block, which processed of thread of packets from network controller and record it in memory block. All interfaces are synechron.
 
Interface with network controller:
- data (32 bit)
- flags (4 bit){one bit of flags match one byte of date, if he equals 0 byte is data, else byte is service symbol(0 - EOP, 1 - EEP, 2 - FILL)}
- valid {word on bus}
- ready {dma is ready for receive}
 
Interface with memory:
- address (20 bit)
- data (64 bit)
- write {command for write}
- ready {memory is ready for write of data}

Format records in memory:
Two memory area(settled on start address and size)
area 1 - In it records packet of data, with alignment on 8 byte
area 2 - It is area of descriptors of packets

Every packet have matched 1 descriptors of length 8 byte: field(63 bit) of packet length parameterized - length packet in bytes, field of mark of packet end state(located in bit #63 ) 0 - EOP, 1 - EEP
 
Service interface for set up registers:
- adress (5 bit)
- data_in (32 bit)
- data_out (32 bit)
- write - {command for write/read}

 Set of registers:
- start address of first area
- size of first area
- counter of address of first area
- start address of second area
- size of second area
- counter of address of second area 
- flag - register (service marks for interruptions)
 
Interface of interruption:
- interruption on memory fields ended
- interruption on records packet in memory

