# Introduction #

On the home page, I stated that programs are art.


# Details #

Many people have made the connection between programming and art.  I believe the connection to be a literal one, not a metaphor.

## A Programming Joke ##

In [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/sicp/), Exercise 1.3 asks the user to

> Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

The point of the exercise is to introduce students to case analysis via the `cond` operation.  The desired solution is something like this:

```
(define (sum-square-largest x y z)
  (cond ((and (> y x) (> z x)) ;; y and z are largest
         (+ (* y y) (* z z)))
        ((and (> x y) (> z y)) ;; x and z are largest
         (+ (* x x) (* z z)))
        ((and (> x z) (> y z)) ;; x and y are largest
         (+ (* x x) (* y y)))))
```
Of course there are numerous variations on this answer, but the important point is that there is a three-way conditional that reflects the property that at most one of x, y, or z is the smallest number.  The symmetry in the problem is matched by a symmetry in the solution.

Consider this solution:
```
(define (sum-square-largest x y z)
  (cond ((and (< x y) (< x z)) ;; x is smallest
         (+ (* y y) (* z z)))
        (else (sum-square-largest y z x))))
```
When I first showed this solution to my colleagues the universal reaction was one of surprised laughter.  The solution doesn't seem to have enough "parts" to work, nonetheless it does.  I think the short solution is surprising because everyone knows the intent of the exercise is to teach multi-way branches, and the recursive call shows up unexpectedly.  Joe Garvin gives this alternative:
```
(define (sls x y z) 
   (cond ((> z x) (sls z y x)) 
         ((> z y) (sls x z y)) 
         (else (+ (* x x) (* y y)))))
```

The code above is a joke.  The setup is the problem description and the first solution.  The punchline is when you encounter a recursive call instead of the remaining two thirds of the program.  The object of the code is to surprise the reader, not to perform an actual computation.

## So Where's the Art? ##

I don't want to get involved in the debate over what constitutes `art'.  I will only point out that some computer programs satisfy much of the criteria that is often used to determine what art is.

  * It is a product of human skill and imagination.

  * It is intended to communicate in addition to any other function it may have.

  * There is an aesthetic associated with it.

  * The aesthetics and ideas communicated transcend the particulars of an individual work.

The definition of art has changed over time, and some works that have been ridiculed at first have later been accepted as sublime.


What I'm trying to do on this web site is present my work, in the context in which it was developed, not for the purpose of maintaining them as working programs, but for the purpose of communicating my thoughts and my aesthetics about code.  Perhaps I'm being pompous and no one cares.  Perhaps someone will be inspired to create something new.

# Feedback and Comments #

(I'm intending to add more to this essay as the muse strikes.  I enjoy receiving comments even while I work on this.)

(I guess someone mentioned [this page on reddit](http://programming.reddit.com/info/1nb8t/comments).  There are some interesting comments there.  I'm going to address some here.)

jeroenl:
Am I the only one or is nobody seeing that his proposed "better" solution is potentially a crasher thanks to an endless recursion?

I'm not proposing either as "better", I'm proposing the second as "funnier", and suggesting that other qualitative, non-technical measures might exist.  For the record, though, these programs fail if the numbers are all the same.  The trivial fix is left to the reader.

mgsloan wrote:
If this is proof that programs are art, then I hope that I never work with an artist-programmer.

I don't think that the short version is more _artistic_ than the long.  I mention this example because it struck me that everyone thought it was funny or amusing.  This is an emotional reaction that is unrelated to its nominal purpose.  Of course I've heard code described as "beautiful" or "ugly", but this is very much a "joke".  Not only is it a joke, but you have to have a fair amount of training to appreciate it.  I started to wonder if there were other emotions one could convey through code.

So for the record:  The code above is **art** in that I wrote it for no purpose other than to communicate some humor.  It is _not_ intended to be an example of good programming any more than an Escher print is intended to be an example of an architectural plan.  I wouldn't consider it _fine_ art either.  It reminds me of this limerick:
```
  A man from Kalamazoo
  Wrote limericks that stopped at line two.
```
No one would place this alongside Eliot's _Wasteland_, but it is an art of a sort.

thenoxx writes:
Well, they definitely are Post-Modern Art, or better than it, as it takes years of schooling to understand the beauty in them, and they have absolutely zero accessibility to the common man.

I find that to be a shame.  I wish I could show this to my mom and get her to laugh.

Other people question whether coding is an art or a craft.  Certainly the bulk of code is craft.  I find it interesting that some people seem to be almost offended by the notion that some code could have artistic value.  Others seem to think it to be the height of self-indulgence to suggest that a mundane activity such as programming might have any resemblence whatsoever to classical art.

I have to say that I largely agree.  The bulk of code, even good code, is as artistic as a Thomas Kinkcaid print.  I've always felt that people who compared code with art were delusional at best.  Maybe I've fallen into that delusion.  But I have seen code that inspired me to emulate it, and code that filled me with disgust.

I'll agree that I'm being too broad with the term _art_, but I'm interested in what goes beyond the _craft_ part of code.

jamescoleuk writes:
You're intentionally using a cliched image of art, but maybe (this could be wrong) the difference is this: a program simply does not have enough to say about what it means to be human.

James raises an interesting point.  How much does it have to say?  If it doesn't say it literally, does metaphor suffice?  Can Cage's acoustic work be considered art?