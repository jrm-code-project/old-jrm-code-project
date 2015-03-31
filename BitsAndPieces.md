# Introduction #

Here are various and sundry pieces of code with commentary.


# Details #

  * [lispm.init](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/lispm.init)
> My init file for the Lisp Machine.  Not especially interesting, but we have to begin somewhere.  The bobhack file gave me a mouse cursor that was J. R. "Bob" Dobbs with his pipe being the hot spot.  Unfortunately, that would cause occasional microcode crashes (something to do with the size of the bitmap).  I also had a cons cell mouse cursor and a middle finger cursor.  I started using a Dvorak keyboard at LMI.

  * [custom.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/custom.lisp)
> Some customizations.  These are embarrassing.  Well, I was young.  I wouldn't use most of these now because it is important that other programmers can read your code.  These little name changes and hacks just add a layer of obfuscation.  I still think question marks are better than the trailing P however.

  * [Pace's files](http://jrm-code-project.googlecode.com/svn/trunk/lambda/pace/)
> Pace Willisson was one of best programmers I've ever worked with.  Here are some of his files.

  * [disable-page-out-words.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/pace/disable-page-out-words.lisp)
> The LMI Lambda had a background process that would `scrub' dirty pages and write them back to disk.  It would also free the scrubbed pages, but that turned out to be a bad idea.  This hack that Pace wrote disables the freeing of the scrubbed pages.

  * [RG's files](http://jrm-code-project.googlecode.com/svn/trunk/lambda/rg/)
> [Rick Greenblatt](http://en.wikipedia.org/wiki/Richard_Greenblatt_(programmer)) is one of the amazing people I have had the pleasure to work with.  We've had our differences over the years, but I have learned a lot from Rick.  Here are some of his files.

  * [old-micro-tracer.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/rg/old-micro-tracer.lisp)
> When I started at LMI, Rick asked me to dust off the microcode tracer.  This was a program designed to check assertions in the Lambda microcode.  I played around with Rick's version for a while, but I found the code difficult to understand and wrote my own version.  The version here has a mixture of some code of mine with Rick's original code.

  * [m-p.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/m-p.lisp)
  * [numsets.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/numsets.lisp)
  * [uregs.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/uregs.lisp)
  * [udasm.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/udasm.lisp)
  * [utrace.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/utrace.lisp)
> These files are parts of my version of the microcode tracer.  I started hitting the limitations of the LMI Lambda fairly quickly.  Closures didn't work right, lexical environments were limited to 64 variables, the garbage collector would crap out and halt the machine.  It was a nightmare.  Rick suggested that I simply fix the compiler and microcode.  So I became a compiler and microcode hacker.

  * [qcp1.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/sys/qcp1.lisp)
> The first problem was aliasing of lexical variables in parallel environments.  This turned out to be a thorny problem.  The `breakoff` function in qcp1 handles lexical closures.  Right after this function is a comment I wrote that explains the rather nasty solution.  We temporarily replace the lexical variables with GENSYMS to avoid the aliasing, but set them back when we do the recursive compile.  Some of the comments in this file are mine, but I forget which ones.

  * [closures.txt](http://jrm-code-project.googlecode.com/svn/trunk/lambda/ulambda/closures.txt)
  * [uc-stack-closure.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/ulambda/uc-stack-closure.lisp)
  * [lex-fix.txt](http://jrm-code-project.googlecode.com/svn/trunk/lambda/ulambda/lex-fix.txt)
  * [uc-closure.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/ulambda/uc-closure.lisp)
> The second problem turned out to be bugs in the microcode.  The original closure microcode was overly complicated and buggy.  I rewrote good chunks of it and finally rewrote the entire heap allocated closure routines.

> As it turned out, Pace and I discovered a fundamental problem in the stack closure implementation.  It was possible, under certain circumstances, to create a stack frame that contained pointers to dead structure that appeared to be stack closures.  An unlucky garbage collection at that point would trash the stack.  We couldn't figure out a viable solution to the problem, so we made the decision to simply allocate all closures in the heap.  At that time, we had enough confidence in the generational GC, so the decision was easy.

> Lowell Hawkinson, however, was not too happy about this.  The PICON group at LMI (which later broke off to start the [GENSYM](http://www.gensym.com/) corporation) was his pet project and he worked hard to ensure that the PICON program would do no consing whatsoever when it was running in a steady state.  Of course the implicit consing done by heap-allocation of closures threw a monkey wrench in that plan.  We pointed out that the choice was between consing and random, unpredictable crashing, and that the generational garbage collector actually worked, he grudgingly accepted it.

  * [Lambda Microcode](http://jrm-code-project.googlecode.com/svn/trunk/lambda/ulambda/)
  * [ubin](http://jrm-code-project.googlecode.com/svn/trunk/lambda/ubin/)
> Here is the microcode to the LMI Lambda.  I did a bunch of tweaking and fixing here as you can see if you look at the ulambda-comments.txt file.  When I started using closures I became the major source of runtime garbage at LMI.  Most users simply turned of the garbage collector, wrote code that avoided consing, and rebooted once the machine ran out of memory.  I thought that the machine should be taking care of the garbage rather than me, so I got nominated to get the GC working.

> It turned out that there were few bugs in the GC that Ken Sinclair wrote.  The majority of `GC' bugs were errors in the rest of the runtime system that violated the invariants that were required by the GC.  This would cause random crashes when some invalid pointer caused the GC to trash part of the heap.  The bug wasn't caused by the GC, but we always blamed the messenger.  I spent the better part of year tracking these things down and became very familiar with all different parts of the microcode.  As an example, look at the ulambda-comments for late March to early April 1986.  It took me a week to find the bug in [uc-storage-allocation.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/ulambda/uc-storage-allocation.lisp) (look in x-make-array.  The fix is to put a NIL in m-d before returning).

  * [cold](http://jrm-code-project.googlecode.com/svn/trunk/lambda/cold/)
  * [debugger](http://jrm-code-project.googlecode.com/svn/trunk/lambda/debugger/)
  * [demo](http://jrm-code-project.googlecode.com/svn/trunk/lambda/demo/)
  * [dired-fsdebug](http://jrm-code-project.googlecode.com/svn/trunk/lambda/dired-fsdebug/)
  * [examples](http://jrm-code-project.googlecode.com/svn/trunk/lambda/examples/)
  * [file](http://jrm-code-project.googlecode.com/svn/trunk/lambda/file/)
  * [io](http://jrm-code-project.googlecode.com/svn/trunk/lambda/io/)
  * [io1](http://jrm-code-project.googlecode.com/svn/trunk/lambda/io1/)
  * [network](http://jrm-code-project.googlecode.com/svn/trunk/lambda/network/)
  * [sys](http://jrm-code-project.googlecode.com/svn/trunk/lambda/sys/)
  * [sys2](http://jrm-code-project.googlecode.com/svn/trunk/lambda/sys2/)
  * [ubin](http://jrm-code-project.googlecode.com/svn/trunk/lambda/ubin/)
  * [ulambda](http://jrm-code-project.googlecode.com/svn/trunk/lambda/ulambda/)
  * [window](http://jrm-code-project.googlecode.com/svn/trunk/lambda/window/)
  * [zmail](http://jrm-code-project.googlecode.com/svn/trunk/lambda/zmail/)
  * [zwei](http://jrm-code-project.googlecode.com/svn/trunk/lambda/zwei/)
> These directories contain the basic Lisp Machine code.

  * [des-data.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/des-data.lisp)
  * [fdes-microcode.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/fdes-microcode.lisp)
  * [fdes.lisp](http://jrm-code-project.googlecode.com/svn/trunk/lambda/jrm/fdes.lisp)
> My friend Bob Baldwin was working on a password cracker at MIT.  I thought I'd try doing the same on a Lisp Machine.  It turns out that the LMI Lambda was built with 4K by 1 RAMs for the register file.  This was double the number of registers that the CADR had, but since the Lambda instructions were the same size, there was no way to address the extra registers.  The extra address line was instead routed to a config register and was never used.

> The Lambda had pageable microcode.  You could actually write microcode on the fly, assemble it into the micro-paging area and access it through dynamically generated misc instructions.  I wrote some microcode that loaded the hidden register set with DES S-boxes.  Some more microcode would swap the register sets by toggling the config register, run several rounds of DES, the swap the registers back.  A little bit of Lisp code glued this together so that the machine could run dictionary attacks in microcode in the background.

> The barrel shifter on the Lambda made it easy to do the kind of bit manipulation you want with DES, but the Lambda was never a blazingly fast machine, so the password cracker was no faster than VAX/8600.  It was a fun hack, though.

> Here's a little detour through the [K-Machine Bits and Pieces](http://code.google.com/p/jrm-code-project/wiki/KMachineBits).

  * [Sherman](http://jrm-code-project.googlecode.com/svn/trunk/sherman/)
> Sherman is a compiler that compiled [Rebol](http://www.rebol.com/) to Scheme.  Rebol is a lightweight language that was designed by [Carl Sassenrath](http://en.wikipedia.org/wiki/Carl_Sassenrath).  I joined Rebol Technologies in the summer of 1998 and wrote the first implementation of the Rebol interpreter.  Carl and I had a falling out in December of 1998 (an interesting topic for another time), and I decided to see if I couldn't write a compiler for the language.  The main challenge is that the language is context-sensitive and parsing depends on runtime values.  It isn't possible to build a useful abstract syntax tree at compile time because it is impossible to determine the role each identifier plays until it's value is known.  Furthermore, an identifier may indicate a variable at some times or a function call at others.

> The trick of Sherman is to simply generate the control flow for all the parse options and dispatch to the correct one at runtime.  This makes a combinatorical explosion, but it is possible to prune the control flow tree if the compiler can make assumptions about certain identifiers.  I added a declaration form to the language for this purpose.  The output of Sherman is a Scheme program that is equivalent to the original Rebol program.  The Scheme version can be compiled with the Scheme compiler and it will run quite a bit faster than the original Rebol.

> Rebol 1.0 was very Scheme-like, but the context-sensitive parsing allows you to omit most of the grouping (parenthesis).  Here is [Ackermann's function](http://en.wikipedia.org/wiki/Ackermann_function) in Rebol:
```
ack: func [m n] [
    if zero? :m [
        :n + 1
        ]
    else [
        if zero? :n [
            ack :m - 1 1
            ]
        else [
            ack :m - 1 ack :m :n - 1  ;; Note this line.
            ]
        ]
    ]
```
> Check out the final recursive call to ack.  The Rebol interpreter correctly determines that this should be parsed `(ack (- m 1) (ack m (- n 1)))` rather than any other potential way.  Rebol 1.0 was properly tail-recursive, which was a true bear to achieve, and it featured first-class continuations (this latter was trivial to add because of the way I had to do the tail recursion).

> I figured out a neat trick for parsing non-lisp stuff without a lot of effort.  The file [R2S.L](http://jrm-code-project.googlecode.com/svn/trunk/sherman/R2S.L) is a [Flex](http://flex.sourceforge.net/) program that parses Rebol.  The production rules in the parse simply print the parsed token, but with a few strategically placed parenthesis.  The result is a text file that can be easily read by a Lisp system.  Here is the output of the Ackermann program from above:
```
`(SHERMAN ,(LIST->BLOCK `(,(LIST->BLOCK `()) 

declare ,(LIST->BLOCK `(standard-definitions )) 

declare ,(LIST->BLOCK `(arity ack 2)) 
ack: func ,(LIST->BLOCK `(m n )) ,(LIST->BLOCK `(
    if zero? :m ,(LIST->BLOCK `(
        :n + 1
        )) 
    else ,(LIST->BLOCK `(
        if zero? :n ,(LIST->BLOCK `(
            ack :m - 1 1
            )) 
        else ,(LIST->BLOCK `(
            ack :m - 1 ack :m :n - 1
            )) 
        )) 
    )) 

print ack 3 5 ; should print 253


))) ;; END of SHERMAN CODE
```
> It's a bit ugly, but it is a legal Scheme backquoted list.

> Carl abandoned my implementation after I left Rebol Technologies.  He completely rewrote the interpreter and, in the process, substantially changed the semantics of the language.  Rebol 2.0 is a quirky and bizarre language.  I don't see any opportunity in the language so I never tried to write a compiler for Carl's semantics.  Rebol seems to still be popular in France, but I think the company has shrunk down to be just Carl and his wife.

  * [Gobblet](http://jrm-code-project.googlecode.com/svn/trunk/scheme/gobblet/)
> [Gobblet](http://www.blueorangegames.com/gobblet.php) is a really cool board game from [Blue Orange Games](http://www.blueorangegames.com/).  It's easy to learn, games are reasonably short, but the strategy can get quite difficult.  After we played it around the lab at Northeastern, I decided to write a computer player.  The board representation is similar to that used in chess programs.  A bitvector represents a slice through the board and 8 of these are used to represent the board.  The advantage of a bitvector is that you can operate on the entire board in parallel.  Incidentally, this version of Gobblet is unrelated to the version that comes with [PLT Scheme](http://www.plt-scheme.org/).

  * [Fog](http://jrm-code-project.googlecode.com/svn/trunk/fog/)
> [FOG](http://www.computing.surrey.ac.uk/research/dsrg/fog/) is a meta-compiler for C++.  Ed Willink wrote this as part of his Doctoral dissertation.  The code here is a modification of the original FOG parser that emits s-expressions as it parses the C++ code.  The output can be further processed by Scheme or Lisp.  The source tree is set up as a Visual Studio Express project.

  * [Evil](http://jrm-code-project.googlecode.com/svn/trunk/scheme/evil/)
> Evil is a C++ to Scheme --- yes, _from_ C++ _to_ Scheme --- that I wrote to help me translate the GUI layer of DrScheme.  Input like this:
```
void wxMediaStreamOut::PrettyFinish()
{
  if (!bad && col) {
    f->Write("\n", 1);
    col = 0;
  }
}
```
> is translated to output like this:
```
  (define-method
    ("void" (wx-media-stream-out pretty-finish))
    (call-with-current-continuation
      (lambda (return)
        (if (and (not (->boolean (instance-ref ':arrow self 'bad)))
                 (->boolean (instance-ref ':arrow self 'col)))
          (begin
            (send (instance-ref ':arrow self 'f) 'write "\n" 1)
            (instance-set! ':arrow self 'col 0))
          #f)
        (return (void)))))
```
> It ain't pretty, but it beats doing it by hand.

> I'll get around to documenting it later (maybe earlier if someone nudges me).  For people interested in getting into CLOS, this has some good examples.













