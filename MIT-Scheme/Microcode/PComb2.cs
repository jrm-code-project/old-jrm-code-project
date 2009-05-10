using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class PrimitiveCombination2 : SCode, ISerializable, ISystemHunk3
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<string> histogram = new Histogram<String> ();
        string histogramKey;

        protected Type rand0Type;
        protected Type rand1Type;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive2 rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod2 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand1;

        protected PrimitiveCombination2 (Primitive2 rator, SCode rand0, SCode rand1)
            : base (TC.PCOMB2)
        {
            this.rator = rator;
            this.method = rator.Method;
            this.rand0 = rand0;
            this.rand1 = rand1;
#if DEBUG
            this.histogramKey = rator.ToString() + " " + rand0.GetType ().Name.ToString () + " " + rand1.GetType ().Name.ToString ();
            rand0Type = rand0.GetType ();
            rand1Type = rand1.GetType ();
#endif
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = rator.Method;
        }

        public Primitive2 Rator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public SCode Rand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand0;
            }
        }

        public SCode Rand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand1;
            }
        }

        static SCode Rewrite (Primitive2 old, Primitive1 op, SCode rand)
        {
            //Console.Write ("\n; Flatten {0} => {1}", old.Name, op.Name);
            return PrimitiveCombination1.Make (op, rand);
        }

        static SCode Rewrite (Primitive2 old, Primitive2 op, SCode rand0, SCode rand1)
        {
            //Console.Write ("\n; Flatten {0} => {1}", old.Name, op.Name);
            return PrimitiveCombination2.Make (op, rand0, rand1);
        }

        static SCode Rewrite1 (SCode rand0) {
            //Console.Write ("\n; Flatten Less-than-fixnum => not positive");
            return PrimitiveCombination1.Make (Primitive.Not, PrimitiveCombination1.Make (Primitive.PositiveFixnum, rand0));
        }

        static SCode Simplify (Primitive2 old, SCode rand)
        {
            //Console.Write ("\n; Simplify {0}", old.Name);
            return rand;
        }

        static SCode StandardMake (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.Cons) ? PrimitiveCons.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.IsEq) ? PrimitiveIsEq.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.ObjectIsEq) ? PrimitiveIsObjectEq.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.CharIsEq) ? PrimitiveIsCharEq.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive2 && rator == Primitive.IntIsEq) ? PrimitiveIsIntEq.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.IsFixnumEqual) ? PrimitiveIsFixnumEqual.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.GeneralCarCdr) ? PrimitiveGeneralCarCdr.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.GreaterThanFixnum) ? PrimitiveGreaterThanFixnum.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.LessThanFixnum) ? PrimitiveLessThanFixnum.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.PlusFixnum) ? PrimitivePlusFixnum.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive2 && rator == Primitive.IsObjectType) ? PrimitiveIsObjectType.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                Configuration.EnableInlinePrimitive2 && rator == Primitive.RecordRef) ? PrimitiveRecordRef.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive2 && rator == Primitive.Vector8BRef) ? PrimitiveVector8BRef.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnableInlinePrimitive2 && rator == Primitive.VectorRef) ? PrimitiveVectorRef.Make (rator, rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnablePrimitive2Specialization &&
                 rand0 is PrimitiveCar) ? PrimitiveCombination2Car.Make (rator, (PrimitiveCar) rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnablePrimitive2Specialization &&
                 rand0 is LexicalVariable) ? PrimitiveCombination2L.Make (rator, (LexicalVariable) rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnablePrimitive2Specialization &&
                 rand0 is Quotation) ? PrimitiveCombination2Q.Make (rator, (Quotation) rand0, rand1) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnablePrimitive2Specialization &&
                 rand1 is LexicalVariable) ? PrimitiveCombination2SL.Make (rator, rand0, (LexicalVariable) rand1) :
                (Configuration.EnableSuperOperators &&
                 Configuration.EnablePrimitive2Specialization &&
                 rand1 is Quotation) ? PrimitiveCombination2SQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2 (rator, rand0, rand1);
        }

        static SCode SpecialMake (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            if (rator == Primitive.BoolIsEq) {
                return Rewrite (rator, ((bool) rand0.Quoted) ? Primitive.IsSharpT : Primitive.Not, rand1);
            }
            else if (rator == Primitive.IntIsEq) {
                if ((int) rand0.Quoted == 0)
                    return Rewrite (rator, Primitive.ObjectIsZero, rand1);
                else
                    return StandardMake (rator, rand0, rand1);
            }
            else if (rator == Primitive.IsEq) {
                if (rand0.Quoted == null)
                    return Rewrite (rator, Primitive.IsNull, rand1);
                else if (rand0.Quoted is char)
                    return Rewrite (rator, Primitive.CharIsEq, rand0, rand1);
                else if (rand0.Quoted is string     ||
                         rand0.Quoted is Primitive  ||
                         rand0.Quoted is ReturnCode ||
                         rand0.Quoted is Symbol)
                    return Rewrite (rator, Primitive.ObjectIsEq, rand0, rand1);
                else if (rand0.Quoted is bool)
                    return Rewrite (rator, Primitive.BoolIsEq, rand0, rand1);
                else if (rand0.Quoted is int)
                    return Rewrite (rator, Primitive.IntIsEq, rand0, rand1);
                else
                    Debugger.Break ();
            }
            else if (rator == Primitive.FixnumIsGreaterThan) {
                if ((int) rand0.Quoted == 0) {
                    return Rewrite (rator, Primitive.FixnumIsNegative, rand1);
                }
                else if ((int) rand0.Quoted != 2
                    && (int) rand0.Quoted != 10)
                    Debugger.Break ();
            }
            else if (rator == Primitive.FixnumMultiply) {
                if ((int) rand0.Quoted <= 1) {
                    Debugger.Break ();
                }
            }
            else if (rator == Primitive.IsFixnumEqual) {
                if ((int) rand0.Quoted == 0)
                    return Rewrite (rator, Primitive.IsFixnumZero, rand1);
                else if ((int) rand0.Quoted != 1
                    && (int) rand0.Quoted != 2
                    && (int) rand0.Quoted != 4
                    && (int) rand0.Quoted != 5
                    && (int) rand0.Quoted != 8
                    && (int) rand0.Quoted != 10
                    && (int) rand0.Quoted != 14
                    && (int) rand0.Quoted != 15
                    && (int) rand0.Quoted != 34
                    && (int) rand0.Quoted != 39
                    && (int) rand0.Quoted != 128
                    && (int) rand0.Quoted != 55296
                    && (int) rand0.Quoted != 56320
                    )
                    Debugger.Break ();
            }
            else if (rator == Primitive.LessThanFixnum) {
                if ((int) rand0.Quoted == 0) {
                    return Rewrite (rator, Primitive.PositiveFixnum, rand1);
                }
                Debugger.Break ();
            }
            else if (rator == Primitive.FixnumAnd) {
                if ((int) rand0.Quoted != 1
                    && (int) rand0.Quoted != 2
                    && (int) rand0.Quoted != 4
                    && (int) rand0.Quoted != 8
                    && (int) rand0.Quoted != 192
                    && (int) rand0.Quoted != 63488
                    && (int) rand0.Quoted != 64512
                    && (int) rand0.Quoted != 65534)
                    Debugger.Break ();
            }
            else if (rator == Primitive.FixnumLsh) {
                if ((int) rand0.Quoted != 1
                    && (int) rand0.Quoted != 255)
                    Debugger.Break ();
            }
            else if (rator == Primitive.FixnumOr) {
                if ((int) rand0.Quoted == 0)
                    return Simplify (rator, rand1);
                else if ((int) rand0.Quoted != 128
                         && (int) rand0.Quoted != 192
                         && (int) rand0.Quoted != 224
                         && (int) rand0.Quoted != 240
                    && (int) rand0.Quoted != 55296
                    && (int) rand0.Quoted != 56320
                    )
                    Debugger.Break ();
            }
            else if (rator == Primitive.FixnumSubtract) {
                if ((int) rand0.Quoted == 0)
                    return Rewrite (rator, Primitive.FixnumNegate, rand1);
                else if ((int) rand0.Quoted != 8)
                    Debugger.Break ();
            }
            else if (rator == Primitive.FlonumMultiply) {
                if ((double) rand0.Quoted != 2
                    && (double) rand0.Quoted != 4.3429448190325175)

                    Debugger.Break ();
            }
            else if (rator == Primitive.IntegerAdd) {
                if ((int) rand0.Quoted != 2)
                    Debugger.Break ();
            }
            else if (rator == Primitive.IntegerIsEqual) {
                if ((int) rand0.Quoted != 1
                    && (int) rand0.Quoted != 2)
                    Debugger.Break ();
            }
            else if (rator == Primitive.IntegerMultiply) {
                if (rand0.Quoted is int
                    && (int) rand0.Quoted != 2
                    && (int) rand0.Quoted != 100000)
                    Debugger.Break ();
            }
            else if (rator == Primitive.IntegerShiftLeft) {
                if ((int) rand0.Quoted != 1)
                    Debugger.Break ();
            }
            else if (rator == Primitive.PlusFixnum) {
                if ((int) rand0.Quoted == 1)
                    return Rewrite (rator, Primitive.FixnumAdd1, rand1);
            }
            else if (true
                && !(rand1 is Quotation)
                && rator != Primitive.CharIsEq
                && rator != Primitive.Cons
                && rator != Primitive.FlonumDivide
                && rator != Primitive.FlonumExpt
                && rator != Primitive.FlonumSubtract
                // Don't rewrite generics because we don't know the ultimate
                // semantics.
                && rator != Primitive.GenericAdd
                && rator != Primitive.GenericDivide
                && rator != Primitive.GenericIsEqual
                && rator != Primitive.GenericIsLessThan
                && rator != Primitive.GenericMultiply
                && rator != Primitive.GenericSubtract
                && rator != Primitive.IsObjectType
                && rator != Primitive.MapCodeToMachineAddress
                && rator != Primitive.MapMachineAddressToCode
                && rator != Primitive.ObjectIsEq
                && rator != Primitive.ObjectSetType
                && rator != Primitive.PrimitiveIsObjectType
                && rator != Primitive.PrimitiveObjectSetType
                && rator != Primitive.StringRef
                && rator != Primitive.SystemListToVector
                && rator != Primitive.VectorCons
                && rator != Primitive.VectorRef
                && rator != Primitive.WithInterruptMask
                )
                Debugger.Break ();
            return StandardMake (rator, rand0, rand1);
        }

        static SCode Break ()
        {
            Debugger.Break ();
            return null;
        }

        static SCode SpecialMake (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                 (rator == Primitive.GreaterThanFixnum) ? (((int) rand1.Quoted == 0)
                                                          ? Rewrite (rator, Primitive.PositiveFixnum, rand0)
                                                          : ((int) rand1.Quoted >= 1) ? StandardMake (rator, rand0, rand1)
                                                          : Break())
                : (rator == Primitive.IntegerAdd) ? ((rand1.Quoted is long && (long) rand1.Quoted == 4294967291)
                ? StandardMake (rator, rand0, rand1) 
                : ((int) rand1.Quoted == 1) ? Rewrite (rator, Primitive.IntegerAdd1, rand0)
                : Break())
                 : (rator == Primitive.IntegerIsGreater) ? ((rand1.Quoted is long && (long) rand1.Quoted == 4294967291)
                                                                ? StandardMake (rator, rand0, rand1) 
                                                                : ((int) rand1.Quoted == 0) ? Rewrite (rator, Primitive.IntegerIsPositive, rand0)
                                                                : Break ())
                : (rator == Primitive.IntegerMultiply) ? ((
                (rand1.Quoted is int && (int) rand1.Quoted == 2)
                || (rand1.Quoted is long && (long) rand1.Quoted == 4294967291))
                ? StandardMake (rator, rand0, rand1) 

                : Break ())
                : (rator == Primitive.IntegerSubtract) ? (((int) rand1.Quoted == 1) ? Rewrite (rator, Primitive.IntegerSub1, rand0) : Break())
                : (rator == Primitive.IsFixnumEqual) ? (((int) rand1.Quoted >= 1)
                                                     ? StandardMake (rator, rand0, rand1)
                                                     : ((int) rand1.Quoted == 0) ? Rewrite (rator, Primitive.IsFixnumZero, rand0)
                                                     : Break())
                : (rator == Primitive.LessThanFixnum) ? (((int) rand1.Quoted == 0)
                                                          ? Rewrite (rator, Primitive.FixnumIsNegative, rand0)
                                                          : ((int) rand1.Quoted == 1) ? Rewrite1 (rand0)
                                                          :((int) rand1.Quoted == -4
                                                          || (int) rand1.Quoted >= 2) ? StandardMake (rator, rand0, rand1)
                                                          : Break ())
                : (rator == Primitive.FixnumSubtract) ? (((int) rand1.Quoted == 0) ? Simplify (rator, rand0)
                                                         : ((int) rand1.Quoted == 1) ? Rewrite (rator, Primitive.FixnumSub1, rand0)
                                                         : ((int) rand1.Quoted >= 2) ? StandardMake (rator, rand0, rand1)
                                                         :Break())
                 : (rator == Primitive.FixnumAnd) ? (((int) rand1.Quoted == 1 
                                                     || (int) rand1.Quoted == 7
                                                     || (int) rand1.Quoted == 15
                                                     || (int) rand1.Quoted == 31
                                                     || (int) rand1.Quoted == 63
                                                     || (int) rand1.Quoted == 240
                                                     || (int) rand1.Quoted == 255
                                                     || (int) rand1.Quoted == 1023
                                                     || (int) rand1.Quoted == 2097151)
                                                     ? StandardMake (rator, rand0, rand1)
                                                     : Break ())
                : (rator == Primitive.FixnumXor) ? (((int) rand1.Quoted == 1)
                                                     ? StandardMake (rator, rand0, rand1)
                                                     : Break())
                 : (rator == Primitive.FixnumOr) ? (((int) rand1.Quoted >= 1)
                                                     ? StandardMake (rator, rand0, rand1)
                                                     : Break ())
               : (rator == Primitive.FixnumLsh) ? (((int) rand1.Quoted >= 1 
                                                   || (int) rand1.Quoted <= -1)
                                                     ? StandardMake (rator, rand0, rand1)
                                                     : (int) rand1.Quoted == 0 ? Simplify (rator, rand0)
                                                     : Break ())
                 : (rator == Primitive.FlonumAdd) ? (((double) rand1.Quoted == 1.0
                                                      || (double) rand1.Quoted == 4294967291.0)
                                                     ? StandardMake (rator, rand0, rand1)
                                                     : Break())
                : (rator == Primitive.FlonumIsGreaterThan) ? (((double) rand1.Quoted ==1.0)
                                                                ? StandardMake (rator, rand0, rand1)
                                                                : ((double) rand1.Quoted == 0.0) ? Rewrite (rator, Primitive.FlonumIsPositive, rand0)
                                                                : Break())
                : (rator == Primitive.FlonumIsLessThan) ? (((double) rand1.Quoted == -1.0)
                                                                ? StandardMake (rator, rand0, rand1)
                                                                : ((double) rand1.Quoted == 0.0) ? Rewrite (rator, Primitive.FlonumIsNegative, rand0)
                                                                : Break ())
                :(rator == Primitive.GeneralCarCdr) ? (((int) rand1.Quoted >= 4)
                                                       ? StandardMake (rator, rand0, rand1)
                                                       : Break())
                : (rator == Primitive.IsEq) ? ((rand1.Quoted is string ||
                                               rand1.Quoted is char[] ||
                                               rand1.Quoted is Constant ||
                                               rand1.Quoted is Primitive ||
                                               rand1.Quoted is ReturnCode ||
                                               rand1.Quoted is Symbol)
                                             ? Rewrite (rator, Primitive.ObjectIsEq, rand0, rand1)
                                             : (rand1.Quoted is char) ? Rewrite (rator, Primitive.CharIsEq, rand0, rand1)
                                             : (rand1.Quoted is int) ? Rewrite (rator, Primitive.IntIsEq, rand0, rand1)
                                             : (rand1.Quoted is bool && (bool) rand1.Quoted) ? Rewrite (rator, Primitive.IsSharpT, rand0)
                                             : (rand1.Quoted is bool && !((bool) rand1.Quoted)) ? Rewrite (rator, Primitive.Not, rand0)
                                             : Break())
                : (rator == Primitive.IntIsEq) ? (((int) rand1.Quoted >= 1 
                                                   || (int) rand1.Quoted == -2
                                                    ) ? StandardMake (rator, rand0, rand1)
                                               : ((int) rand1.Quoted == 0) ? Rewrite (rator, Primitive.ObjectIsZero, rand0)
                                             : Break ())
                : (rator == Primitive.PlusFixnum) ? (((int) rand1.Quoted == 1) ? Rewrite (rator, Primitive.FixnumAdd1, rand0)
                                                      : ((int) rand1.Quoted >= 2) ? StandardMake (rator, rand0, rand1)
                                                          : ((int) rand1.Quoted == 0) ? Simplify (rator, rand0)
                                                      : Break())
          : (rator == Primitive.FixnumMultiply) ? (((int) rand1.Quoted == 256
                                                  || (int) rand1.Quoted == 2
                                                          ) ? StandardMake (rator, rand0, rand1)
                                                      : Break ())
                : (true 
                   && rator != Primitive.CharIsEq
                   && rator != Primitive.Cons
                   && rator != Primitive.FixnumQuotient
                   && rator != Primitive.FileAccess
                   && rator != Primitive.FixnumRemainder
                   && rator != Primitive.FloatingVectorRef
                   && rator != Primitive.FlonumIsEqual
                   && rator != Primitive.FlonumDivide
                   && rator != Primitive.FlonumSubtract
                   && rator != Primitive.GenericIsGreaterThan
                   && rator != Primitive.GenericIsEqual
                   && rator != Primitive.GenericIsLessThan
                   && rator != Primitive.GenericMultiply
                   && rator != Primitive.GenericDivide
                   && rator != Primitive.GenericAdd
                   && rator != Primitive.GenericSubtract
                   && rator != Primitive.GetServiceByName
                   && rator != Primitive.IntegerIsEqual
                   && rator != Primitive.IntegerToFlonum
                   && rator != Primitive.IntegerRemainder
                   && rator != Primitive.IntegerQuotient
                   && rator != Primitive.IntegerDivide
                   && rator != Primitive.LexicalReference
                   && rator != Primitive.LexicalUnreferenceable
                   && rator != Primitive.MakeBitString
                   && rator != Primitive.ObjectIsEq
                   && rator != Primitive.PrimitiveAddress
                   && rator != Primitive.PrimitiveObjectEq
                   && rator != Primitive.PrimitiveObjectRef
                   && rator != Primitive.Quotient
                   && rator != Primitive.RealTimerSet
                   && rator != Primitive.RecordRef
                   && rator != Primitive.Remainder
                   && rator != Primitive.StringRef
                   && rator != Primitive.SetCar
                   && rator != Primitive.SetCdr
                   && rator != Primitive.ShutdownSocket
                   && rator != Primitive.SystemPairSetCar
                   && rator != Primitive.SystemVectorRef
                   && rator != Primitive.VectorCons
                   && rator != Primitive.VectorRef
                   && rator != Primitive.Vector8BRef
                   && rator != Primitive.Win32ExpandEnvironmentStrings) ? Break ()
                : StandardMake (rator, rand0, rand1);
        }

        public static SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            else
                return 
                    (rand0 is Quotation) ? SpecialMake (rator, (Quotation) rand0, rand1)
                    : (rand1 is Quotation) ? SpecialMake (rator, rand0, (Quotation) rand1)
                    : StandardMake (rator, rand0, rand1);
        }

        public static SCode Make (object rator, object rand0, object rand1)
        {
            return Make ((Primitive2) rator, EnsureSCode (rand0), EnsureSCode (rand1));
        }

        public static SCode Make (Hunk3 init)
        {
            return Make ((Primitive2) init.Cxr0, init.Cxr1, init.Cxr2);
        }

        [SchemePrimitive ("PRIMITIVE-COMBINATION2?", 1, true)]
        public static bool IsPrimitiveCombination2 (out object answer, object arg)
        {
            answer = arg is PrimitiveCombination2;
            return false;
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optRand0 = this.rand0.Bind (ctenv);
            SCode optRand1 = this.rand1.Bind (ctenv);
            return (optRand0 == this.rand0 && optRand1 == this.rand1) 
                ? this
                : PrimitiveCombination2.Make (this.rator, optRand0, optRand1);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.rand0.CallsTheEnvironment ()
                || this.rand1.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            histogram.Note (this.histogramKey);
            ratorHistogram.Note (this.rator);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.rand0.MutatesAny (formals)
                || this.rand1.MutatesAny (formals);
        }

        public override bool Uses (Symbol formal)
        {
            return this.rand0.Uses (formal) || this.rand1.Uses (formal);
        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return UnwrapQuoted (this.rator);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            get
            {
                return UnwrapQuoted (this.rand0);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr2
        {
            get
            {
                return UnwrapQuoted (this.rand1);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (PrimitiveCombination2Deserializer));
            info.AddValue ("rator", this.rator);
            info.AddValue ("rand0", this.rand0);
            info.AddValue ("rand1", this.rand1);
        }
    }

    [Serializable]
    internal sealed class PrimitiveCombination2Deserializer : IObjectReference
    {
        // This object has no fields (although it could).
        Primitive2 rator;
        SCode rand0;
        SCode rand1;

        // GetRealObject is called after this object is deserialized.
        public Object GetRealObject (StreamingContext context)
        {
            return PrimitiveCombination2.Make (this.rator, this.rand0, this.rand1);
        }

        public void SetRator (Primitive2 rator) { this.rator = rator; }
        public void SetRand0 (SCode rand0) { this.rand0 = rand0; }
        public void SetRand1 (SCode rand1) { this.rand1 = rand1; }
    }


    [Serializable]
    sealed class PrimitiveCombination2Frame0 : SubproblemContinuation<PrimitiveCombination2>, ISystemVector
    {
        public PrimitiveCombination2Frame0 (PrimitiveCombination2 expression, Environment environment)
            : base (expression, environment)
        {
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
        {
            Control unev = this.expression.Rand0;
            Environment env = this.environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this.expression, this.environment, ev1));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.expression.Rator);
            SCode.location = this.expression.Rator.Name.ToString();
#endif
            if (this.expression.Rator.Method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    sealed class PrimitiveCombination2Frame1 : SubproblemContinuation<PrimitiveCombination2>, ISystemVector
    {
        readonly object ev1;

        internal PrimitiveCombination2Frame1 (PrimitiveCombination2 expression, Environment environment, object ev1)
            : base (expression, environment)
        {
            this.ev1 = ev1;
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev0)
        {
#if DEBUG
            Primitive.hotPrimitives.Note (this.expression.Rator);
            SCode.location = this.expression.Rator.Name.ToString();
#endif
            if (this.expression.Rator.Method (out answer, ev0, this.ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    class PrimitiveCombination2Car : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly SCode rand0rand0;

        protected PrimitiveCombination2Car (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0rand0 = rand0.Operand;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarL) ? PrimitiveCombination2CarL.Make (rator, (PrimitiveCarL) rand0, rand1) :
                (rand1 is LexicalVariable) ? PrimitiveCombination2CarSL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveCombination2CarSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2Car (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0rand0);
            noteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2Car.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            unev = this.rand0rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons)ev0).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2CarL : PrimitiveCombination2Car
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveCombination2CarL (Primitive2 rator, PrimitiveCarL rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.OperandName;
            this.rand0Depth = rand0.OperandDepth;
            this.rand0Offset = rand0.OperandOffset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarA) ? PrimitiveCombination2CarA.Make (rator, (PrimitiveCarA) rand0, rand1) :
                (rand0 is PrimitiveCarL1) ? PrimitiveCombination2CarL1.Make (rator, (PrimitiveCarL1) rand0, rand1):
                (rand1 is LexicalVariable) ? PrimitiveCombination2CarLL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveCombination2CarLQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2CarL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2CarL.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2CarA : PrimitiveCombination2CarL
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination2CarA (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarA0) ? PrimitiveCombination2CarA0.Make (rator, (PrimitiveCarA0) rand0, rand1) :
                (rand0 is PrimitiveCarA1) ? PrimitiveCombination2CarA1.Make (rator, (PrimitiveCarA1) rand0, rand1) :
                //(rand1 is LexicalVariable) ? PrimitiveCombination2CarLL.Make (rator, rand0, (LexicalVariable) rand1) :
                //(rand1 is Quotation) ? PrimitiveCombination2CarLQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2CarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2CarA.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) environment.ArgumentValue(this.rand0Offset)).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2CarA0 : PrimitiveCombination2CarA
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination2CarA0 (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveCombination2CarA0L.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveCombination2CarA0Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2CarA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2CarA0.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) environment.Argument0Value).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    class PrimitiveCombination2CarA0L : PrimitiveCombination2CarA0
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2CarA0L (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveCombination2CarA0A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is LexicalVariable1) ? PrimitiveCombination2CarA0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
                new PrimitiveCombination2CarA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarA0L.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) environment.Argument0Value).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    class PrimitiveCombination2CarA0A : PrimitiveCombination2CarA0L
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        protected PrimitiveCombination2CarA0A (Primitive2 rator, PrimitiveCarA0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented() :
                (rand1 is Argument1) ? PrimitiveCombination2CarA0A1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2CarA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString ();
#endif
            if (this.method (out answer, ((Cons) environment.Argument0Value).Car, environment.ArgumentValue(this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    sealed class PrimitiveCombination2CarA0A1 : PrimitiveCombination2CarA0A
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2CarA0A1 (Primitive2 rator, PrimitiveCarA0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveCombination2CarA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString ();
#endif
            if (this.method (out answer, ((Cons) environment.Argument0Value).Car, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    sealed class PrimitiveCombination2CarA0L1 : PrimitiveCombination2CarA0L
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2CarA0L1 (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveCombination2CarA0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarA0L1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString ();
#endif
            if (this.method (out answer, ((Cons) environment.Argument0Value).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    sealed class PrimitiveCombination2CarA0Q : PrimitiveCombination2CarA0
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object rand1Value;

        PrimitiveCombination2CarA0Q (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
        {
            return
                new PrimitiveCombination2CarA0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) environment.Argument0Value).Car, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2CarA1 : PrimitiveCombination2CarA
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination2CarA1 (Primitive2 rator, PrimitiveCarA1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA1 rand0, SCode rand1)
        {
            return
                //(rand1 is LexicalVariable) ? PrimitiveCombination2CarLL.Make (rator, rand0, (LexicalVariable) rand1) :
                //(rand1 is Quotation) ? PrimitiveCombination2CarLQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2CarA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2CarA1.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) environment.Argument1Value).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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



    class PrimitiveCombination2CarL1 : PrimitiveCombination2CarL
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination2CarL1 (Primitive2 rator, PrimitiveCarL1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL1 rand0, SCode rand1)
        {
            return
                //(rand1 is LexicalVariable) ? PrimitiveCombination2CarLL.Make (rator, rand0, (LexicalVariable) rand1) :
                //(rand1 is Quotation) ? PrimitiveCombination2CarLQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2CarL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2CarL1.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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



    class PrimitiveCombination2CarLL : PrimitiveCombination2CarL
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2CarLL (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveCombination2CarLA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is LexicalVariable1) ? PrimitiveCombination2CarLL1.Make (rator, rand0, (LexicalVariable1) rand1) :
                new PrimitiveCombination2CarLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarLL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2CarLA : PrimitiveCombination2CarLL
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        protected PrimitiveCombination2CarLA (Primitive2 rator, PrimitiveCarL rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveCombination2CarLA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveCombination2CarLA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2CarLA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarLA.EvalStep";
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, environment.ArgumentValue (this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination2CarLA0 : PrimitiveCombination2CarLA
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2CarLA0 (Primitive2 rator, PrimitiveCarL rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, Argument0 rand1)
        {
            return
                new PrimitiveCombination2CarLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarLA0.EvalStep";
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination2CarLA1 : PrimitiveCombination2CarLA
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2CarLA1 (Primitive2 rator, PrimitiveCarL rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, Argument1 rand1)
        {
            return
                new PrimitiveCombination2CarLA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarLA1.EvalStep";
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination2CarLL1 : PrimitiveCombination2CarLL
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2CarLL1 (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveCombination2CarLL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarLL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();


            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination2CarLQ : PrimitiveCombination2CarL
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object rand1Value;

        PrimitiveCombination2CarLQ (Primitive2 rator, PrimitiveCarL rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, Quotation rand1)
        {
            return
                new PrimitiveCombination2CarLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarLQ.EvalStep";
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2CarSL : PrimitiveCombination2Car
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2CarSL (Primitive2 rator, PrimitiveCar rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveCombination2CarSA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is LexicalVariable1) ? PrimitiveCombination2CarSL1.Make (rator, rand0, (LexicalVariable1) rand1) :
                new PrimitiveCombination2CarSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0rand0);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarSL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            Control unev = this.rand0rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2CarSA : PrimitiveCombination2CarSL
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        protected PrimitiveCombination2CarSA (Primitive2 rator, PrimitiveCar rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveCombination2CarSA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveCombination2CarSA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2CarSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0rand0);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarSA.EvalStep";
#endif
            Control unev = this.rand0rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, environment.ArgumentValue(this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination2CarSA0 : PrimitiveCombination2CarSA
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2CarSA0 (Primitive2 rator, PrimitiveCar rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, Argument0 rand1)
        {
            return
                new PrimitiveCombination2CarSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0rand0);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarSA0.EvalStep";
#endif
            Control unev = this.rand0rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination2CarSA1 : PrimitiveCombination2CarSA
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2CarSA1 (Primitive2 rator, PrimitiveCar rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, Argument1 rand1)
        {
            return
                new PrimitiveCombination2CarSA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0rand0);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarSA1.EvalStep";
#endif
            Control unev = this.rand0rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination2CarSL1 : PrimitiveCombination2CarSL
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2CarSL1 (Primitive2 rator, PrimitiveCar rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveCombination2CarSL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0rand0);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarSL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            Control unev = this.rand0rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    sealed class PrimitiveCombination2CarSQ : PrimitiveCombination2Car
    {
#if DEBUG
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        public readonly object rand1Value;

        PrimitiveCombination2CarSQ (Primitive2 rator, PrimitiveCar rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, Quotation rand1)
        {
            return
                new PrimitiveCombination2CarSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0rand0);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2CarSQ.EvalStep";
#endif

            Control unev = this.rand0rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ((Cons) ev0).Car, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2L : PrimitiveCombination2
    {
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected PrimitiveCombination2L (Primitive2 rator, LexicalVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Depth = rand0.Depth;
            this.rand0Offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
        {
            return
                (rand0 is Argument) ? PrimitiveCombination2A.Make (rator, (Argument) rand0, rand1)
                : (rand0 is LexicalVariable1) ? PrimitiveCombination2L1.Make (rator, (LexicalVariable1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveCombination2LL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveCombination2LQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveCombination2L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2L.EvalStep");
            noteCalls (this.rand1);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A : PrimitiveCombination2L
    {
        protected PrimitiveCombination2A (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveCombination2A0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveCombination2A1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveCombination2AL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveCombination2AQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveCombination2A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand1);
            SCode.location = "PrimitiveCombination2A.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = "-";
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A0 : PrimitiveCombination2A
    {
        protected PrimitiveCombination2A0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveCombination2A0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveCombination2A0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveCombination2A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls(this.rand1);
            SCode.location = "PrimitiveCombination2A0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
#if DEBUG
            SCode.location = "-";
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A1 : PrimitiveCombination2A
    {
        protected PrimitiveCombination2A1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveCombination2A1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveCombination2A1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveCombination2A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            noteCalls (this.rand1);
            Warm ("PrimitiveCombination2A1.EvalStep");
            Primitive.hotPrimitives.Note (this.rator);
#endif

            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (this.method (out answer, environment.Argument1Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2L1 : PrimitiveCombination2L
    {
        protected PrimitiveCombination2L1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveCombination2L1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveCombination2L1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveCombination2L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2L1.EvalStep");
            noteCalls (this.rand1);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                
            throw new NotImplementedException (); 

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    class PrimitiveCombination2Q : PrimitiveCombination2
    {
        public readonly object rand0Value;

        protected PrimitiveCombination2Q (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
                this.rand0Value = rand0.Quoted;
        }

        static SCode RewriteConditional (Primitive2 rator, Quotation rand0, Conditional rand1)
        {
            return Conditional.Make (rand1.Predicate,
                                     PrimitiveCombination2.Make (rator, rand0, rand1.Consequent),
                                     PrimitiveCombination2.Make (rator, rand0, rand1.Alternative));
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Conditional) ? RewriteConditional (rator, rand0, (Conditional) rand1) :
                (rand1 is Disjunction) ? Unimplemented() :
                (rand1 is LexicalVariable) ? PrimitiveCombination2QL.Make (rator, rand0, (LexicalVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveCombination2QQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is Sequence2) ? Unimplemented() :
                (rand1 is Sequence3) ? Unimplemented() :
                new PrimitiveCombination2Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2Q.EvalStep");
            noteCalls (this.rand1);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0 = this.rand0Value;

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2SL : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
#endif
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2SL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return 
                (rand1 is Argument) ? PrimitiveCombination2SA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveCombination2SL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveCombination2SL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination2SL.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "-";
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2SA : PrimitiveCombination2SL
    {

        protected PrimitiveCombination2SA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return 
                (rand1 is Argument0) ? PrimitiveCombination2SA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveCombination2SA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveCombination2SA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls(this.rand0);
            SCode.location = "PrimitiveCombination2SA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2SAFrame0 (this, environment, ev1));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
       

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination2SAFrame0 : SubproblemContinuation<PrimitiveCombination2SA>, ISystemVector
    {
        readonly object ev1;
        public PrimitiveCombination2SAFrame0 (PrimitiveCombination2SA expression, Environment environment, object ev1)
            : base (expression, environment)
        {
            this.ev1 = ev1;
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev0)
        {
            #if DEBUG
            Primitive.hotPrimitives.Note (this.expression.Rator);
            
            SCode.location = this.expression.Rator.Name.ToString();
#endif
            if (this.expression.Rator.Method (out answer, ev0, this.ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2SA0 : PrimitiveCombination2SA
    {

        protected PrimitiveCombination2SA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return new PrimitiveCombination2SA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2SA0.EvalStep");
            noteCalls (this.rand0);
#endif
            object ev1 = environment.Argument0Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2SA1 : PrimitiveCombination2SA
    {

        protected PrimitiveCombination2SA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return new PrimitiveCombination2SA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2SA1.EvalStep");
            noteCalls(this.rand0);
#endif
            object ev1 = environment.Argument1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2SL1 : PrimitiveCombination2SL
    {

        protected PrimitiveCombination2SL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveCombination2SL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls(this.rand0);
            SCode.location = "PrimitiveCombination2SL1.EvalStep";
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = "-";
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    sealed class PrimitiveCombination2SQ : PrimitiveCombination2
    {
        public readonly object rand1Value;

        PrimitiveCombination2SQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Value = rand1.Quoted;
        }
               
        static SCode RewriteConditional (Primitive2 rator, Conditional rand0, Quotation rand1)
        {
            return Conditional.Make (rand0.Predicate,
                                     PrimitiveCombination2.Make (rator, rand0.Consequent, rand1),
                                     PrimitiveCombination2.Make (rator, rand0.Alternative, rand1));
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return 
                //(rand0 is Conditional) ? RewriteConditional (rator, (Conditional) rand0, rand1) :
                new PrimitiveCombination2SQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls(this.rand0);
            SCode.location = "PrimitiveCombination2SQ.EvalStep";
#endif
            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2SQFrame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
#if DEBUG
            SCode.location = "-";
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination2SQFrame0 : SubproblemContinuation<PrimitiveCombination2SQ>, ISystemVector
    {
        public PrimitiveCombination2SQFrame0 (PrimitiveCombination2SQ expr, Environment env)
            : base (expr, env)
        {
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
#if DEBUG
            SCode.location = "-";
            Primitive.hotPrimitives.Note (this.expression.Rator);

            SCode.location = this.expression.Rator.Name.ToString();
#endif
            if (this.expression.Rator.Method (out answer, value, this.expression.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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


    class PrimitiveCombination2LL : PrimitiveCombination2L
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2LL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return 
                (rand1 is Argument) ? PrimitiveCombination2LA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveCombination2LL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveCombination2LL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2LL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2LA : PrimitiveCombination2LL
    {

        protected PrimitiveCombination2LA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return 
                (rand1 is Argument0) ? PrimitiveCombination2LA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveCombination2LA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveCombination2LA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2LA.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2LA0 : PrimitiveCombination2LA
    {

        protected PrimitiveCombination2LA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return new PrimitiveCombination2LA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2LA0.EvalStep");
#endif
            object ev1 = environment.Argument0Value;
            object ev0;
            if (environment.FastLexicalRef (out ev0, rand0Name, rand0Depth, rand0Offset))
               
            throw new NotImplementedException (); 

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2LA1 : PrimitiveCombination2LA
    {

        protected PrimitiveCombination2LA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return new PrimitiveCombination2LA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2LA1.EvalStep");
#endif
            object ev1 = environment.Argument1Value;
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
            throw new NotImplementedException (); 

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2LL1 : PrimitiveCombination2LL
    {

        protected PrimitiveCombination2LL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveCombination2LL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2LL1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.LexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
            throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2LQ : PrimitiveCombination2L
    {
        public readonly object rand1Value;

        protected PrimitiveCombination2LQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return new PrimitiveCombination2LQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2LQ.EvalStep");
#endif
            object ev1 = this.rand1Value;
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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


    class PrimitiveCombination2AL : PrimitiveCombination2A
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2AL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return 
                (rand1 is Argument) ? PrimitiveCombination2AA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveCombination2AL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveCombination2AL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2AL.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2AA : PrimitiveCombination2AL
    {

        protected PrimitiveCombination2AA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return 
                (rand1 is Argument0) ? PrimitiveCombination2AA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveCombination2AA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveCombination2AA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2AA.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (rand1Offset);
            object ev0 = environment.ArgumentValue (rand0Offset);

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2AA0 : PrimitiveCombination2AA
    {

        protected PrimitiveCombination2AA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return new PrimitiveCombination2AA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2AA0.EvalStep");
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2AA1 : PrimitiveCombination2AA
    {

        protected PrimitiveCombination2AA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitiveCombination2AA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2AA1.EvalStep");
#endif
            object ev1 = environment.Argument1Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);
           

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2AL1 : PrimitiveCombination2AL
    {

        protected PrimitiveCombination2AL1 (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveCombination2AL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2AL1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2AQ : PrimitiveCombination2A
    {
        public readonly object rand1Value;

        protected PrimitiveCombination2AQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return new PrimitiveCombination2AQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2AQ.EvalStep");
#endif
            object ev1 = this.rand1Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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


    class PrimitiveCombination2A0L : PrimitiveCombination2A0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2A0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return 
                (rand1 is Argument) ? PrimitiveCombination2A0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveCombination2A0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveCombination2A0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2A0L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
#if DEBUG
            SCode.location = "-";
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A0A : PrimitiveCombination2A0L
    {

        protected PrimitiveCombination2A0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return 
                (rand1 is Argument0) ? PrimitiveCombination2A0A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveCombination2A0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveCombination2A0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2A0A.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.Argument0Value;

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A0A0 : PrimitiveCombination2A0A
    {

        protected PrimitiveCombination2A0A0 (Primitive2 rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument0 rand1)
        {
            return new PrimitiveCombination2A0A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2A0A0.EvalStep");
#endif
            object ev0 = environment.Argument0Value;

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev0)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A0A1 : PrimitiveCombination2A0A
    {

        protected PrimitiveCombination2A0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return new PrimitiveCombination2A0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A0L1 : PrimitiveCombination2A0L
    {

        protected PrimitiveCombination2A0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveCombination2A0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2A0L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
              throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A0Q : PrimitiveCombination2A0
    {
        public readonly object rand1Value;

        protected PrimitiveCombination2A0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return new PrimitiveCombination2A0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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


    class PrimitiveCombination2A1L : PrimitiveCombination2A1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2A1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return 
                (rand1 is Argument) ? PrimitiveCombination2A1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveCombination2A1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveCombination2A1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2A1L.EvalStep");
            Primitive.hotPrimitives.Note (this.rator);
            
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if (this.method (out answer, environment.Argument1Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A1A : PrimitiveCombination2A1L
    {

        protected PrimitiveCombination2A1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return 
                (rand1 is Argument0) ? PrimitiveCombination2A1A0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveCombination2A1A1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2A1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2A1A.EvalStep");
            Primitive.hotPrimitives.Note (this.rator);    
#endif
            if (this.method (out answer, environment.Argument1Value, environment.ArgumentValue(this.rand1Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A1A0 : PrimitiveCombination2A1A
    {

        protected PrimitiveCombination2A1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return new PrimitiveCombination2A1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2A1A0.EvalStep");
            Primitive.hotPrimitives.Note (this.rator);       
#endif
            if (this.method (out answer, environment.Argument1Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A1A1 : PrimitiveCombination2A1A
    {

        protected PrimitiveCombination2A1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument1 rand1)
        {
            return new PrimitiveCombination2A1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException (); object ev0 = null; object ev1 = null;

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A1L1 : PrimitiveCombination2A1L
    {

        protected PrimitiveCombination2A1L1 (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveCombination2A1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2A1L1.EvalStep");
            Primitive.hotPrimitives.Note (this.rator);
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            if (this.method (out answer, environment.Argument1Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2A1Q : PrimitiveCombination2A1
    {
        public readonly object rand1Value;

        protected PrimitiveCombination2A1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return new PrimitiveCombination2A1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2A1Q.EvalStep");
            Primitive.hotPrimitives.Note (this.rator);        
#endif
            if (this.method (out answer, environment.Argument1Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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


    class PrimitiveCombination2L1L : PrimitiveCombination2L1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2L1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return 
                (rand1 is Argument) ? PrimitiveCombination2L1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveCombination2L1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveCombination2L1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2L1L.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2L1A : PrimitiveCombination2L1L
    {

        protected PrimitiveCombination2L1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return 
                (rand1 is Argument0) ? PrimitiveCombination2L1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveCombination2L1A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveCombination2L1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException (); object ev0 = null; object ev1 = null;

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2L1A0 : PrimitiveCombination2L1A
    {

        protected PrimitiveCombination2L1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return new PrimitiveCombination2L1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2L1A0.EvalStep");
#endif
            object ev1 = environment.Argument0Value;
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2L1A1 : PrimitiveCombination2L1A
    {

        protected PrimitiveCombination2L1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return new PrimitiveCombination2L1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2L1A1.EvalStep");
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException (); 
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2L1L1 : PrimitiveCombination2L1L
    {

        protected PrimitiveCombination2L1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveCombination2L1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2L1L1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2L1Q : PrimitiveCombination2L1
    {
        public readonly object rand1Value;

        protected PrimitiveCombination2L1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return new PrimitiveCombination2L1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2L1Q.EvalStep");
#endif
            object ev1 = this.rand1Value;
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

        
    
    class PrimitiveCombination2QL : PrimitiveCombination2Q
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveCombination2QL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return 
                (rand1 is Argument) ? PrimitiveCombination2QA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveCombination2QL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveCombination2QL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2QL.EvalStep");
#endif

            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = this.rand0Value;

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2QA : PrimitiveCombination2QL
    {

        protected PrimitiveCombination2QA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return 
                (rand1 is Argument0) ? PrimitiveCombination2QA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveCombination2QA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveCombination2QA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2QA.EvalStep");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = this.rand0Value;

#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2QA0 : PrimitiveCombination2QA
    {

        protected PrimitiveCombination2QA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return new PrimitiveCombination2QA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, this.rand0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2QA1 : PrimitiveCombination2QA
    {

        protected PrimitiveCombination2QA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return new PrimitiveCombination2QA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, this.rand0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    class PrimitiveCombination2QL1 : PrimitiveCombination2QL
    {

        protected PrimitiveCombination2QL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveCombination2QL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2QL1.EvalStep");
#endif
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
#if DEBUG
            SCode.location = "-";
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, this.rand0Value, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    sealed class PrimitiveCombination2QQ : PrimitiveCombination2Q
    {
        public readonly object rand1Value;

        PrimitiveCombination2QQ (Primitive2 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
              this.rand1Value = rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Quotation rand1)
        {
            if (true
                // These cons, so we can't cache the value.
                && rator != Primitive.Cons
                && rator != Primitive.IntegerMultiply // just hard
                && rator != Primitive.ObjectSetType
                && rator != Primitive.PrimitiveObjectSetType
                && rator != Primitive.VectorCons
                )
                Debugger.Break ();
            return new PrimitiveCombination2QQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            Primitive.hotPrimitives.Note (this.rator);
            
            SCode.location = this.rator.Name.ToString();
#endif
            if (this.method (out answer, this.rand0Value, this.rand1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

}
