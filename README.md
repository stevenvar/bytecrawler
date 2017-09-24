# bytecrawler

Bytecrawler is a tool that reads OCaml bytecode with some kind of very simple abstract intepreter, and associates for each of the visited bytecode instructions a WCET, thus computing a total WCET for the program by adding all of them. This WCET computation is done with the bound-t time and stack analyser for AVR microcontrollers.

This tool is intended for my own use for the moment, and is prone to various bugs.
