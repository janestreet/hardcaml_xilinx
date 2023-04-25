## Release v0.16.0

* `True_dual_port_ram`, `Dual_port_ram`, `Simple_dual_port_ram`, `Memory_builder` - allow
  naming of underlying `multiport_memory` primitive in simulation builds.
* `Fifo_sync`, `Xpm_fifo_sync` expose optional cascade height and nearly full/empty level parameters.
* `Ram_with_resizing` simple dual port ram where the read and write data widths may differ by a
   power of 2.  Suitable for simulation and synthesis.
* Expose ICAPE3 primtiive
