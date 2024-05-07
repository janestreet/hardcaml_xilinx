## Release v0.17.0

* Port to `ppx_hardcaml`
* Add `Clocking_mode` type to represent synchronous or asynchronous clocking in xpm generated RAMs.
* Expose `Sysmone1` component 
* Various FIFO improvements and fixes
  - Configurable read latency.
  - Expose fifo memory type, empty/full thresholds and reset busy for the XPM generated
    FIFO.
  - Better simulation modelling of the XPM fifo.
* Various updates to `Ram_with_resizing`
  - Support asynchronous clocks.
  - Support byte enables.
  - Better simulation modelling.

## Release v0.16.0

* `True_dual_port_ram`, `Dual_port_ram`, `Simple_dual_port_ram`, `Memory_builder` - allow
  naming of underlying `multiport_memory` primitive in simulation builds.
* `Fifo_sync`, `Xpm_fifo_sync` expose optional cascade height and nearly full/empty level parameters.
* `Ram_with_resizing` simple dual port ram where the read and write data widths may differ by a
   power of 2.  Suitable for simulation and synthesis.
* Expose ICAPE3 primtiive
