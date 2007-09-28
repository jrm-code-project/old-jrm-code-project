using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    sealed class SimpleEvaluator
    {
        private SimpleEvaluator ()
        {
        }

        static Cons Evlis (object formlist)
        {
            Cons evaluated = null;
            while (true) {
                Cons formpair = formlist as Cons;
                if (formpair != null) {
                    evaluated = new Cons (Eval (formpair.Car), evaluated);
                    formlist = formpair.Cdr;
                }
                else if (formpair == null)
                    return (Cons) CL.Reverse (evaluated);
                else
                    throw new NotImplementedException ();
            }
        }

        static public object Eval (object form)
        {
            Cons pair = form as Cons;
            if (pair != null) {
                object op = pair.Car;
                object operands = pair.Cdr;
                object sym = op as Symbol;
                if (sym != null) {
                    return CL.Apply (CL.SymbolFunction (sym), Evlis (operands));
                }
                else
                    throw new NotImplementedException ("Operator not a symbol");
            }
            else {
                Symbol identifier = form as Symbol;
                if (identifier != null) {
                    return CL.SymbolValue (form);
                }
                else {
                    string literal = form as String;
                    if (literal != null) {
                        return literal;
                    }
                    else
                        throw new NotImplementedException ("No lambda functions yet");
                }
            }
        }
    }
}
