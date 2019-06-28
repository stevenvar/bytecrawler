open OByteLib
open OByteLib.Instr

open Mlstack
open Value
open Cycles
open Intrp



let _ =
  let speclist = [("-i", Arg.String (fun s -> interp s), "Enables interpreter mode");
                  ("-f", Arg.Set_float Cycles.freq, "Compute the time with a given frequency (in MHz)");
                  ("-ci", Arg.Set_string Cycles.instr_costs, "Path of the instructions costs table");
                  ("-cp", Arg.Set_string Cycles.prim_costs, "Path of the primitives costs table");
                 ]
   in let usage_msg = "Usage : bytecrawler file \n Options : "
   in Arg.parse speclist (fun s -> count_cost s) usage_msg
