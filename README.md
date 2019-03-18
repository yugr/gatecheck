Gatecheck is a sloppy script which looks for violations
of gating requirements (all registers wider than 4 bits
must be clock-gated).

Script is intentinally simple, ignoring macro and treating
global, cross-module analysis in fast but primitive way.

For info on usage, run
```
$ gatecheck.pl --help
```

TODO (must):
* merge splitted definition of module inputs/outputs e.g.
```
module mod(input x);
reg [2:0] x;
```

TODO (may):
* parse concatenations in NBAs i.e. `{x,y} <= ...`
* respect parameter assignments in module instantiations
* delay warning till parameter is actually used to calculate some register width
* evaluate more types of expressions (indexing, `clog2()`, etc.)

Known issues:
* global parameter matching algorithm is extremely naive
  so may easily match wrong parameters (seems to be precise enough
  for current sources though)
* declaration of same register under different
  branches of macro or parameter condition will
  result in "duplicate declaration" warnings:
```
`ifdef AMBER_WISHBONE_DEBUG
 reg  [7:0]              jitter_r = 8'h0f;
 reg  [1:0]              start_read_r = 'd0;
`else
 reg                     start_read_r = 'd0;
`endif
```
* declaration of same register in tasks will result
  in "duplicate declarations" warnings
* macro in the middle of construct will break parsing:
```
module test(
  output ddr3_ck_n,
`ifdef XILINX_SPARTAN6_FPGA
  inout mcb3_rzq,
`endif
);
```
* macro definition of range will cause parse errors:
```
wire [`MY_SIZE] x;
```
* Verilog attributes are not parsed:
```
(* keep = "true", max_fanout = 30 *) output  reg   [DATA_WIDTH-1:0]   data;
```
* delays are not parsed:
```
r <= #25 0;
wire #1 cpci_rd_wr_L;
```
