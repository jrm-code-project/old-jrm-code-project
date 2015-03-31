# Introduction #

Using MIT-Scheme on .NET or perhaps Mono.


# Details #

Here's how to bootstrap ab initio.

You will need a native version of MIT Scheme in order to generate the initial set of binary files.  If you do not have this, you cannot do an ab initio bootstrap.  In addition, you will need the MIT Scheme source files.  You can download these from
http://www.gnu.org/software/mit-scheme/

You will need the most recent sources.  I had to make a couple of tiny tweaks to
deal with some primitives that I simply cannot implement on the CLR (there is a
primitive that truncates a string by smashing its allocation header.)

Start up MIT Scheme and make sure the SF program is loaded (it is preloaded in the compiler band).  Perform these steps:
```
(cd "path\\to\\mit-scheme\\src\\runtime\\")

(load "runtime.sf")

(cd "..\\sf\\")

(load "sf.sf")
```
Note that you no longer have to deal with utabmd.bin

This will create the initial set of binaries.  These binaries are in the native MIT Scheme format, but don't worry, the C# code knows how to read them.

Move the .bin, .crf, .fre, .pkd, from the runtime directory to the `<solution>`\\BootstrapLib\\Runtime\\ directory.

Move the .bin, .crf, .fre, .pkd, from the sf directory to the `<solution>`\\BootstrapLib\\Sf\\ directory.

Using MIT Scheme again,
```
(cd "solution\\BootstrapLib\\Runtime\\")

(sf "site.scm")
```

Once you do this, you ought to be able to start Scheme and get to the prompt.

_Incomplete_ _instructions_ More in a bit.