using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;

namespace Microcode
{
    [Serializable]
    class PrimitiveCombination1 : SCode
    {
#if DEBUG
        [NonSerialized]
        static Histogram<string> histogram = new Histogram<String> ();
        string histogramKey;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod1 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg0;

        protected PrimitiveCombination1 (Primitive1 procedure, SCode arg0)
            : base (TC.PCOMB1)
        {
            this.procedure = procedure;
            this.method = procedure.Method;
            this.arg0 = arg0;
#if DEBUG
            this.histogramKey = procedure.ToString () + " " + arg0.GetType ().Name.ToString ();
#endif
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = procedure.Method;
        }

        public static SCode Make (Primitive1 rator, SCode rand)
        {
            if (rator == null) throw new ArgumentNullException ("rator");
            return (Configuration.EnableSuperOperators && rand is Argument) ? PrimitiveCombination1A.Make (rator, (Argument) rand)
                :  new PrimitiveCombination1 (rator, rand);
        }

        public static SCode Make (Primitive1 rator, object rand)
        {
            if (rator == null) throw new ArgumentNullException ("rator");
            return PrimitiveCombination1.Make (rator, EnsureSCode(rand));
        }

        public Primitive1 Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.procedure;
            }
        }

        public SCode Operand
        {
            [DebuggerStepThrough]
            get
            {
                return this.arg0;
            }
        }

        [SchemePrimitive ("PRIMITIVE-COMBINATION1?", 1, true)]
        public static bool IsPrimitiveCombination1 (out object answer, object arg)
        {
            answer = arg is PrimitiveCombination1;
            return false;
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optArg0 = this.arg0.Bind (ctenv);
                return  (optArg0 == this.arg0) ? this
                    : PrimitiveCombination1.Make (this.procedure, optArg0);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.arg0.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.arg0);
            histogram.Note (this.histogramKey);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
#endif
            if (this.method (out answer, ev0)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.arg0.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.arg0.UsesAny (formals);
        }

        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveCombination1Frame0 : SubproblemContinuation<PrimitiveCombination1>, ISystemVector
    {

        public PrimitiveCombination1Frame0 (PrimitiveCombination1 expression, Environment environment)
            : base (expression, environment)
        {
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }

         #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

   }

    /// <summary>
    /// A call to a primitive with argument as the argument.
    /// </summary>
    [Serializable]
    class PrimitiveCombination1A : PrimitiveCombination1
    {
        int offset;
        protected PrimitiveCombination1A (Primitive1 procedure, Argument arg0)
            : base (procedure, arg0)
        {
            this.offset = arg0.Offset;
        }

        public static PrimitiveCombination1A Make (Primitive1 rator, Argument rand)
        {
            if (rator == null) throw new ArgumentNullException ("rator");
            return
              (rand is Argument0) ? (PrimitiveCombination1A) PrimitiveCombination1A0.Make (rator, (Argument0) rand)
              : (rand is Argument1) ? (PrimitiveCombination1A) PrimitiveCombination1A1.Make (rator, (Argument1) rand)
              : new PrimitiveCombination1A (rator, rand);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("Should not need binding.");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

            if (this.method (out answer, environment.ArgumentValue (this.offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }

    }

//    /// <summary>
//    /// A call to a primitive with lexical variable as the argument.
//    /// </summary>
//    [Serializable]
//    class PrimitiveCombination1L : PrimitiveCombination1
//    {
//#if DEBUG
//        [NonSerialized]
//        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
//#endif
//        readonly object varname;
//        readonly int depth;
//        readonly int offset;

//        PrimitiveCombination1L (Primitive1 procedure, LexicalVariable arg0)
//            : base (procedure, arg0)
//        {
//            this.varname = arg0.Name;
//            this.depth = arg0.Depth;
//            this.offset = arg0.Offset;
//        }

//        public static SCode Make (Primitive1 rator, LexicalVariable rand)
//        {
//            if (rator == null) throw new ArgumentNullException ("rator");
//            return new PrimitiveCombination1L (rator, rand);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should not need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Primitive.hotPrimitives.Note (this.procedure);
//            procedureHistogram.Note (this.procedure);
//#endif
//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//            object arg = null;
//            if (environment.FastLexicalRef (out arg, this.varname, this.depth, this.offset))
//                throw new NotImplementedException ();

//            if (this.method (out answer, arg)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null; // dispose of the evidence
//                    // set up the interpreter for a tail call
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

/// <summary>
/// A call to a primitive with argument0 as the argument.
/// </summary>
[Serializable]
class PrimitiveCombination1A0 : PrimitiveCombination1A
{
#if DEBUG
    [NonSerialized]
    static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
#endif

    protected PrimitiveCombination1A0 (Primitive1 procedure, Argument0 arg0)
        : base (procedure, arg0)
    {
    }

    public static SCode Make (Primitive1 rator, Argument0 rand)
    {
        if (rator == null) throw new ArgumentNullException ("rator");
        else if (rator == Primitive.Find ("CAR", 1)) return PrimitiveCarA0.Make (rator, rand);
        else if (rator == Primitive.Find ("CDR", 1)) return PrimitiveCdrA0.Make (rator, rand);
        else if (rator == Primitive.Find ("NULL?", 1)) return PrimitiveIsNullA0.Make (rator, rand);
        else if (rator == Primitive.Find ("PAIR?", 1)) return PrimitiveIsPairA0.Make (rator, rand);
        return new PrimitiveCombination1A0 (rator, rand);
    }

    public override SCode Bind (LexicalMap ctenv)
    {
        throw new NotImplementedException ("Should not need binding.");
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        PrimitiveCombination1A0.procedureHistogram.Note (this.procedure);
        Primitive.hotPrimitives.Note (this.procedure);
#endif
        // It is expensive to bounce down to invoke the procedure
        // we invoke it directly and pass along the ref args.

        if (this.method (out answer, environment.Argument0Value)) {
            TailCallInterpreter tci = answer as TailCallInterpreter;
            if (tci != null) {
                answer = null; // dispose of the evidence
                // set up the interpreter for a tail call
                expression = tci.Expression;
                environment = tci.Environment;
                return true;
            }
            else
                throw new NotImplementedException ();
        }
        else return false;
    }
}

/// <summary>
/// A call to a primitive with argument0 as the argument.
/// </summary>
[Serializable]
class PrimitiveCombination1A1 : PrimitiveCombination1A
{
#if DEBUG
    [NonSerialized]
    static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
#endif

    protected PrimitiveCombination1A1 (Primitive1 procedure, Argument1 arg0)
        : base (procedure, arg0)
    {
    }

    public static SCode Make (Primitive1 rator, Argument1 rand)
    {
        //if (rator == null) throw new ArgumentNullException ("rator");
        //else if (rator == Primitive.Find ("CAR", 1)) return PrimitiveCarA1.Make (rator, rand);
        //else if (rator == Primitive.Find ("CDR", 1)) return PrimitiveCdrA1.Make (rator, rand);
        return new PrimitiveCombination1A1 (rator, rand);
    }

    public override SCode Bind (LexicalMap ctenv)
    {
        throw new NotImplementedException ("Should not need binding.");
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        PrimitiveCombination1A1.procedureHistogram.Note (this.procedure);
        Primitive.hotPrimitives.Note (this.procedure);
#endif
        // It is expensive to bounce down to invoke the procedure
        // we invoke it directly and pass along the ref args.

        if (this.method (out answer, environment.Argument1Value)) {
            TailCallInterpreter tci = answer as TailCallInterpreter;
            if (tci != null) {
                answer = null; // dispose of the evidence
                // set up the interpreter for a tail call
                expression = tci.Expression;
                environment = tci.Environment;
                return true;
            }
            else
                throw new NotImplementedException ();
        }
        else return false;
    }
}

/// <summary>
/// A call to car with argument0 as the argument.
/// </summary>
[Serializable]
sealed class PrimitiveCarA0 : PrimitiveCombination1A0
{
    PrimitiveCarA0 (Primitive1 procedure, Argument0 arg0)
        : base (procedure, arg0)
    {
    }

    public static new SCode Make (Primitive1 rator, Argument0 rand)
    {
        if (rator == null) throw new ArgumentNullException ("rator");
        return new PrimitiveCarA0 (rator, rand);
    }

    public override SCode Bind (LexicalMap ctenv)
    {
        throw new NotImplementedException ("Should not need binding.");
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm();
#endif
        // put a try/catch here
        answer = ((Cons) environment.Argument0Value).Car;
        return false;
    }
}

///// <summary>
///// A call to car with argument0 as the argument.
///// </summary>
//[Serializable]
//sealed class PrimitiveCarA1 : PrimitiveCombination1A1
//{
//    PrimitiveCarA1 (Primitive1 procedure, Argument1 arg0)
//        : base (procedure, arg0)
//    {
//    }

//    public static new SCode Make (Primitive1 rator, Argument1 rand)
//    {
//        if (rator == null) throw new ArgumentNullException ("rator");
//        return new PrimitiveCarA1 (rator, rand);
//    }

//    public override SCode Bind (LexicalMap ctenv)
//    {
//        throw new NotImplementedException ("Should not need binding.");
//    }

//    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//    {
//        // put a try/catch here
//        answer = ((Cons) environment.Argument1Value).Car;
//        return false;
//    }
//}


/// <summary>
/// A call to car with argument0 as the argument.
/// </summary>
[Serializable]
sealed class PrimitiveCdrA0 : PrimitiveCombination1A0
{
    PrimitiveCdrA0 (Primitive1 procedure, Argument0 arg0)
        : base (procedure, arg0)
    {
    }

    public static new SCode Make (Primitive1 rator, Argument0 rand)
    {
        if (rator == null) throw new ArgumentNullException ("rator");
        return new PrimitiveCdrA0 (rator, rand);
    }

    public override SCode Bind (LexicalMap ctenv)
    {
        throw new NotImplementedException ("Should not need binding.");
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
#endif
        // put a try/catch here
        answer = ((Cons) environment.Argument0Value).Cdr;
        return false;
    }
}

//    /// <summary>
//    /// A call to car with argument0 as the argument.
//    /// </summary>
//    [Serializable]
//    sealed class PrimitiveCdrA1 : PrimitiveCombination1A1
//    {
//        PrimitiveCdrA1 (Primitive1 procedure, Argument1 arg0)
//            : base (procedure, arg0)
//        {
//        }

//        public static new SCode Make (Primitive1 rator, Argument1 rand)
//        {
//            if (rator == null) throw new ArgumentNullException ("rator");
//            return new PrimitiveCdrA1 (rator, rand);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should not need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            // put a try/catch here
//            answer = ((Cons) environment.Argument1Value).Cdr;
//            return false;
//        }
//    }

/// <summary>
/// A call to car with argument0 as the argument.
/// </summary>
[Serializable]
sealed class PrimitiveIsNullA0 : PrimitiveCombination1A0
{
    PrimitiveIsNullA0 (Primitive1 procedure, Argument0 arg0)
        : base (procedure, arg0)
    {
    }

    public static new SCode Make (Primitive1 rator, Argument0 rand)
    {
        if (rator == null) throw new ArgumentNullException ("rator");
        return new PrimitiveIsNullA0 (rator, rand);
    }

    public override SCode Bind (LexicalMap ctenv)
    {
        throw new NotImplementedException ("Should not need binding.");
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
#endif
        // put a try/catch here
        answer = (environment.Argument0Value == null) ? Constant.sharpT : Constant.sharpF;
        return false;
    }
}

/// <summary>
/// A call to car with argument0 as the argument.
/// </summary>
[Serializable]
sealed class PrimitiveIsPairA0 : PrimitiveCombination1A0
{
    PrimitiveIsPairA0 (Primitive1 procedure, Argument0 arg0)
        : base (procedure, arg0)
    {
    }

    public static new SCode Make (Primitive1 rator, Argument0 rand)
    {
        if (rator == null) throw new ArgumentNullException ("rator");
        return new PrimitiveIsPairA0 (rator, rand);
    }

    public override SCode Bind (LexicalMap ctenv)
    {
        throw new NotImplementedException ("Should not need binding.");
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
#endif
        answer = (environment.Argument0Value is Cons) ? Constant.sharpT : Constant.sharpF;
        return false;
    }
}


}