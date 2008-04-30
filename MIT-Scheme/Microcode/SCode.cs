using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    public class SCode
    {
    }

    sealed class Assignment : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string target;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode value;

        public Assignment (string target, SCode value)
        {
            this.target = target;
            this.value = value;
        }

        public string Name
        {
            get
            {
                return this.target;
            }
        }

        public override string ToString ()
        {
            return "#<ASSIGNMENT " + this.target + ">";
        }
    }

    sealed class Combination : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode [] components;

        public Combination (SCode [] components)
        {
            this.components = components;
        }
    }

    sealed class Combination1 : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand;

        public Combination1 (SCode rator, SCode rand)
        {
            this.rator = rator;
            this.rand = rand;
        }

        public SCode Operator
        {
            get
            {
                return this.rator;
            }
        }
    }

    class Combination2 : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand1;

        public Combination2 (SCode rator, SCode rand0, SCode rand1)
        {
            this.rator = rator;
            this.rand0 = rand0;
            this.rand1 = rand1;
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1)
        {
            return new Combination2 (rator, rand0, rand1);
        }

        public SCode Rand1
        {
            get
            {
                return this.rand1;
            }
        }

        public SCode Rator
        {
            get
            {
                return this.rator;
            }
        }
    }

    sealed class Comment : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object text;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode code;

        public Comment (SCode code, object text)
        {
            this.code = code;
            this.text = text;
        }
    }

    class Conditional : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode predicate;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode consequent;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode alternative;

        Conditional (SCode predicate, SCode consequent, SCode alternative)
        {
            this.predicate = predicate;
            this.consequent = consequent;
            this.alternative = alternative;
        }

        public static SCode Make (SCode predicate, SCode consequent, SCode alternative)
        {
            return new Conditional (predicate, consequent, alternative);
        }

        public SCode Predicate
        {
            get
            {
                return this.predicate;
            }
        }

        public SCode Consequent
        {
            get
            {
                return this.consequent;
            }
        }

        public SCode Alternative
        {
            get
            {
                return this.alternative;
            }
        }
    }

    sealed class Definition : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string name;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode value;

        public Definition (string name, SCode value)
        {
            this.name = name;
            this.value = value;
        }

        public string Name
        {
            get
            {
                return this.name;
            }
        }
    }

    sealed class Disjunction : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode predicate;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode alternative;

        public Disjunction (SCode predicate, SCode alternative)
        {
            this.predicate = predicate;
            this.alternative = alternative;
        }

        public SCode Alternative
        {
            get
            {
                return this.alternative;
            }
        }
    }

    class Lambda : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly string [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode body;

        public Lambda (SCode body, string [] formals)
        {
            // body can be null?!
            if (formals == null)
                throw new ArgumentNullException ("formals");
            this.body = body;
            this.formals = formals;
        }

        public SCode Body
        {
            get
            {
                return this.body;
            }
        }

        public string Name
        {
            get
            {
                return this.formals [0];
            }
        }

        public string [] Formals
        {
            get
            {
                return this.formals;
            }
        }

        public int FormalOffset (string name)
        {
            for (int i = 0; i < formals.Length; i++)
                //if (name == formals [i])
                if (Object.ReferenceEquals (name, formals [i]))
                    return i - 1;
            return -1;
        }
    }

    sealed class ExtendedLambda : Lambda
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly uint required;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly uint optional;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly bool rest;

        public ExtendedLambda (SCode body, string [] formals, uint required, uint optional, bool rest)
            : base (body, formals)
        {
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }
    }

    sealed class PrimitiveCombination0 : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive procedure;

        public PrimitiveCombination0 (Primitive procedure)
        {
            this.procedure = procedure;
        }
    }

    sealed class PrimitiveCombination1 : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Primitive1 procedure;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg0;

        PrimitiveCombination1 (Primitive1 procedure, SCode arg0)
        {
            this.procedure = procedure;
            this.arg0 = arg0;
        }

        public static SCode Make (Primitive1 rator, SCode rand)
        {
            return new PrimitiveCombination1 (rator, rand);
        }

    }

    class PrimitiveCombination2 : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive2 rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand1;

        protected PrimitiveCombination2 (Primitive2 rator, SCode rand0, SCode rand1)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            if (rand0 == null)
                throw new ArgumentNullException ("rand0");
            if (rand1 == null)
                throw new ArgumentNullException ("rand1");
            this.rator = rator;
            this.rand0 = rand0;
            this.rand1 = rand1;
        }

        public Primitive2 Rator
        {
            get
            {
                return this.rator;
            }
        }

        public SCode Rand0
        {
            get
            {
                return this.rand0;
            }
        }


        public SCode Rand1
        {
            get
            {
                return this.rand1;
            }
        }

        public static SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
                return new PrimitiveCombination2 (rator, rand0, rand1);
        }
    }

    sealed class PrimitiveCombination3 : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Primitive procedure;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg0;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg1;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg2;

        public PrimitiveCombination3 (Primitive procedure, SCode arg0, SCode arg1, SCode arg2)
        {
            this.procedure = procedure;
            this.arg0 = arg0;
            this.arg1 = arg1;
            this.arg2 = arg2;
        }
    }

    sealed class Quotation : SCode
    {
        // Space optimization.
        static Dictionary<object, Quotation> table = new Dictionary<object, Quotation> (8000);
        static Quotation QuoteNull;

        static int cacheHits;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object item;

        Quotation (object item)
        {
            this.item = item;
        }

        static bool cacheItem (object item)
        {
            return (item is bool)
                || (item is char)
                || (item is int)
                || (item is string)
                //|| (item is Constant)
                || (item is Primitive)
                //|| (item is ReferenceTrap)
                ;
        }

        public static Quotation Make (object item)
        {
            if (item == null) {
                if (QuoteNull == null)
                    QuoteNull = new Quotation (null);
                return QuoteNull;
            }
            else if (cacheItem (item)) {
                Quotation probe;
                cacheHits++;
                if (table.TryGetValue (item, out probe) != true) {
                    cacheHits--;
                    probe = new Quotation (item);
                    table.Add (item, probe);
                }
                return probe;
            }
            else
                return new Quotation (item);
        }

        public object Quoted
        {
            get
            {
                return this.item;
            }
        }

        public override string ToString ()
        {
            if (this.item == null)
                return "#<SCODE-QUOTE NULL>";
            else
                return "#<SCODE-QUOTE " + this.item.ToString () + ">";
        }
    }

    sealed class Sequence2 : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode first;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode second;

        public Sequence2 (SCode first, SCode second)
        {
            if (first == null)
                throw new ArgumentNullException ("first");
            if (second == null)
                throw new ArgumentNullException ("second");
            this.first = first;
            this.second = second;
        }

    }

    sealed class Sequence3 : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode first;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode second;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode third;

        public Sequence3 (SCode first, SCode second, SCode third)
        {
            if (first == null)
                throw new ArgumentNullException ("first");
            if (second == null)
                throw new ArgumentNullException ("second");
            if (third == null)
                throw new ArgumentNullException ("third");
            this.first = first;
            this.second = second;
            this.third = third;
        }
    }

    class Variable : SCode
    {
        public readonly string name;

        public Variable (string name)
        {
            this.name = name;
        }
    }
}
