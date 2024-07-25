## Tools
The programs in this directory are not from the book _Apple Pascal Games_, and they are not written in Apple Pascal.
They are utilities I have created using [Free Pascal](https://www.freepascal.org/) to aid me in (re)creating
the gaming programs on modern hardware and preparing them to be transferred to Apple II floppies.

These utilities are:
- **a2texity**: This program takes standard editor output, and converts it to the Apple TEXT format as described in the _Apple Pascal Language Reference_ (1980), pp. 12-13.  I work on Linux, but the utility should work with DOS/Windows-style CR-LF files as well.

- **a2upper**: This program converts a source code file to uppercase characters.  When I copy the original program from the book, I type it out in mixed case.  Once I have tested it, I convert a copy to uppercase to commit to the repo and then create the "d3" version using the mixed case source as a starting point.

After installing Free Pascal, the utilities may be built easily from the command line.  For example:
```
$ fpc a2textify
Free Pascal Compiler version 3.2.2 [2024/07/06] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling a2textify.pp
a2textify.pp(107,5) Warning: unreachable code
Linking a2textify
169 lines compiled, 0.0 sec
1 warning(s) issued
```
The warning is because the code never calls the **PadBlock** procedure.  This is intentional (for now) as padding the TEXT file
confuses [Apple Commander](https://applecommander.github.io/) when I try to import it into a disk image as explained
in the [lengthy header comment](https://github.com/dendrown/apple-pascal-games/blob/main/Tools/a2textify.pp#L12) in the source.
Unfortunately, padding is a requirement for Apple's TEXT standard.  There is a work-around that allows you to pad the file from
with the Apple Pascal editor by loading the textified work file (`SYSTEM.WRK.TEXT`), deleting any duplicate lines that show up
after the final `END` in the program, and then saving the updated code.

If anyone wishes to use **a2textify** to load these programs, please open an issue.  Ideally, we can find a way to transfer
the correctly padded work file to a disk image, and then I'll reinstate the call to **PadBlock** in the utility.
