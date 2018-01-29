
'just pretend this string is not here'


'''  ADDITIONS

A button for the jumps window which labels all jumps to a function using the function title comment.

A function optimiser which will condense a chosen function down to as little instructions as possible by removing
  redundant code and modifying all code utilising redundant code.

A script for PJ64D to collect all callers to functions which this app has no jumps calculated for, then incorporate 
  collected data.

Some more documentation on each instruction, such as which parameter exactly is what.
  (confusion with parameters of certain instructions such as MTC1 being back-to-front (compared to other instructions))

'''


'''  CHANGES

Tweak jump mapping to be a little smarter. I aim to have it calculate target addresses for JR/JALR instructions if the
  JR target is statically calculated.
    Not sure if worth it/possible, due to "branch tables" and otherwise lack-thereof.

When cutting a section of code where branches target, modify the branches on paste so they still point to the same code.
  Possibly could substitute with a hotkey such as Shift+Up/Down when selecting the branch target.

'''


'''  BUGS

None I know of right now. I always fix bugs as soon as I encounter them and then commit the changes.

'''


'''  MINOR KNOWN ISSUES

NOPs won't replace all blank lines straight away. However, the function which handles your changes will treat all
  blank lines as NOP anyway.

Moving your mouse to the far-right of any text box and double-clicking that line will cause text on the following
  line to become highlighted. This is a cause by something in the backend of TKinter, which I don't know how to change.
    I have tried to fix this by simply moving the user's "selection start" and "selection end" cursors, but doing so
    seems to corrupt the text boxes beyond repair so much that restarting the program is the only way to fix them.
    Because of that, this has become part of the "too hard" pile.

'''
