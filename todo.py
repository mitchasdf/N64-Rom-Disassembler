
'just pretend this string is not here'


'''  ADDITIONS

A button for the jumps window which labels all jumps to a function using the function title comment.

A function optimiser which will condense a chosen function down to as little instructions as possible by removing
  redundant code and modifying all code utilising redundant code.

A script for PJ64D to collect all callers to functions which this app has no jumps calculated for, then incorporate 
  collected data.

Some more documentation on each instruction, such as which parameter exactly is what.
  (confusion with parameters of certain instructions such as MTC1 being back-to-front (compared to other instructions))

A way to manage app config items. Currently, say for instance you load up super mario 64 and name it as "firsthack.n64",
the app will register "firsthack.n64" and begin all preparations such as determining CIC chip and what not, which
for the purpose of this app, will affect where jumps point to. If the user was to then delete firsthack.n64 and then
open up banjo-tooie and name that as "firsthack.n64", then it will see that there is already an entry for that rom,
and it will load up the CIC chip for super mario 64. So to a beginner, they will have no idea why jumps aren't pointing
to the right place, and why other weird stuff is happening.

Figure out why I have to manually add 45 pixels to the Y placement of widgets on the main window.
  It may not be 45 pixels on every machine.

'''


'''  CHANGES

Tweak jump mapping to be a little smarter. I aim to have it calculate target addresses for JR/JALR instructions if the
  JR target is statically calculated.
    Not sure if worth it/possible, due to "branch tables" and otherwise lack-thereof.

When cutting a section of code where branches target, modify the branches on paste so they still point to the same code.
  Possibly could substitute with a hotkey such as Shift+Up/Down when selecting the branch target.

'''


'''  BUGS

None I know of right now. I always fix major bugs as soon as I encounter them and then commit the changes.

'''


'''  MINOR KNOWN ISSUES

NOPs won't replace all blank lines straight away. However, the function which handles your changes will treat all
  blank lines as NOP anyway.

Moving your mouse to the far-right of any text box and double-clicking that line will cause text on the following
  line to become highlighted. This is a cause by something in the backend of TKinter, which I don't know how to change.
    I have tried to fix this by simply moving the user's "selection start" and "selection end" cursors, but doing so
    seems to corrupt the text boxes beyond repair so much that restarting the program is the only way to fix them.
    Because of that, this has become part of the "too hard" pile.

Very rarely, "Calculate checksum when saving" will un-check itself. I really am stumped over what causes this.
  For all I know, it's only happening to me during debugging. This is easy to notice when it has become un-checked,
  as your rom will fail to boot if you have edited any part of the ROM between 0x1000 and 0xF5240.

Very rarely, something will cause any of the sub-windows such as comments navigator and script generation to not be
  coloured properly when opening them, also throw a bunch of tkinter errors in the command prompt.
  I have not been able to find the cause of this. It can be resolved by saving your work and restarting the program.

'''
