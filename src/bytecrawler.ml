open OByteLib
open OByteLib.Instr

open Mlstack
open Value
open Cycles
open Intrp



let () =
   let speclist = [("-i", Arg.String (fun s -> interp s), "Enables interpreter mode")
                   ]
   in let usage_msg = "Usage : bytecrawler file \n Options : "
   in Arg.parse speclist (fun s -> count_cycles s) usage_msg
