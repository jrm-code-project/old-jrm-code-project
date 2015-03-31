# Introduction #

The K Machine project was one I put a lot of work into.  The project was morphed practically beyond recognition after LMI went bankrupt, but I can still find some remnants of some interesting code.

# Details #

At the beginning, when we were desiging the machine, we would hack up some Lisp code to try out our ideas.  These were primitive simulators, and they were only crude models of the real machine.  At one point, though, I had to start writing the bootstrap code.  I started with some simulation code, but it eventually grew to be the true bootstrap when the hardware became available.

The memory card had a `spy' interface that allowed other cards on the bus to read and write memory and some of the registers.  A debug card was installed on the bus and a Lambda was hooked up to the debug card.  The Lambda would externally halt the K, load up the initial few pages of instruction memory, reset the program counter and then let the machine run.  It was also possible to single step the machine through the debug card.  This was really handy.

The K-Machine could also boot from an internal ROM.  A jumper on the memory board configured this option.

  * [debug-board.lisp](http://jrm-code-project.googlecode.com/svn/trunk/kmachine/kbug/debug-board.lisp)
  * [kbug.lisp](http://jrm-code-project.googlecode.com/svn/trunk/kmachine/kbug/kbug.lisp)
> These files implement a debugger that ran on the Lambda and manipulated the K through the debug interface.  I wrote some of the code in here, but some is clearly the work of others.

  * [xboot.lisp](http://jrm-code-project.googlecode.com/svn/trunk/kmachine/xboot.lisp)
> This file looks the most like my original boot code.  You can find variously modified copies of it around the source tree.  The cold-load image was built on the Lambda and downloaded to the K.  The K would begin execution at `cold-boot-function`.

Symbols from the HW package would generally be compiled as individual instructions or instruction fragments by the compiler.  The very first call `hw:write-open-active-return #x101112`, installs a stack frame.

  * [global-registers.lisp](http://jrm-code-project.googlecode.com/svn/trunk/kmachine/global-registers.lisp)
> The macro `prims::setup-initial-values-of-global-registers` initializes the global registers as specified in this file.

A call to `cold-initialize-call-hardware` gets the call hardware zeroed out.  `cold-initialize-call-hardware` is an example of writing some _very_ low-level code in Lisp.  It's kind of interesting that this can be written as a function call, though.  It has to got through a bit of a contortion to return to its caller because the call hardware is zeroed out by running this.

The call to `trap::illop` halts the machine, and the debugger can read a code that indicates the halt message.  The debugger can also proceed the machine after a halt, so calls to `illop` are more of the nature of a breakpoint.  Since the call hardware is working, we can now use normal function calls.  A call to `event-horizon` (from which we never return) takes us to the next stage in the boot process.

The plan for cold booting was that a boot image on the disk would be plopped into RAM by the boot loader.  The boot image would have some tables at the beginning that would contain the values to put in the GC, transporter, and data-type RAMs.  By 'direct-mapping' the first few virtual pages to the physical memory, we can read the tables.

  * [cold/gc-ram.lisp](http://jrm-code-project.googlecode.com/svn/trunk/kmachine/cold/gc-ram.lisp)
> Here is the code I wrote for loading and using the GC RAM.  Just for kicks, take a look at the `flip` function.  This begins the collection of a quantum by flipping it into oldspace.  We simply turn on the oldspace bit.  The `modify-gc-ram` macro expects a thunk as an argument and I rely on the compiler to simply inline that thunk.

  * [cold/transporter-ram.lisp](http://jrm-code-project.googlecode.com/svn/trunk/kmachine/cold/transporter-ram.lisp)
> Some of this code I wrote to load up the transporter RAM.  It uses the same technique as the GC RAM.  There is some new code in here that looks like an initial stab at hooking in the GC, but it doesn't look quite right.

I had started writing the garbage collector as LMI imploded, but didn't get very far.  The design was fairly simple, but the Gigamos people didn't know enough details to finish it, so there is some weird stuff kicking around in this code.

  * [asm/datatype.lisp](http://jrm-code-project.googlecode.com/svn/trunk/kmachine/asm/datatype.lisp)
> I'm not sure whether I wrote this or not.  The `load-dt-ram-pattern` function looks familiar, so maybe.  We tried to avoid using K-machine assembly code as much as possible, but it was tricky to load the datatype ram.  You had to drop two objects of the correct type through the pipeline and arrange for a write pulse as the objects hit the ALU input registers.  This might be why there is assembly code in here.  I do remember that loading the datatype RAM took the longest part of the boot sequence --- something like 2 or 3 seconds.




