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
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.PCOMB2; } }

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
            : base ()
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

        public SCode Operand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand0;
            }
        }

        public SCode Operand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand1;
            }
        }

        static SCode Rewrite (Primitive2 old, Primitive1 op, SCode rand)
        {
            Console.Write ("\n; Flatten {0} => {1}", old.Name, op.Name);
            return PrimitiveCombination1.Make (op, rand);
        }

        static SCode Rewrite (Primitive2 old, Primitive2 op, SCode rand0, SCode rand1)
        {
            Console.Write ("\n; Flatten {0} => {1}", old.Name, op.Name);
            return PrimitiveCombination2.Make (op, rand0, rand1);
        }

        static SCode Rewrite1 (SCode rand0) {
            Console.Write ("\n; Flatten Less-than-fixnum => not positive");
            return PrimitiveCombination1.Make (Primitive.Not, PrimitiveCombination1.Make (Primitive.PositiveFixnum, rand0));
        }

        static SCode Simplify (Primitive2 old, SCode rand)
        {
            Console.Write ("\n; Simplify {0}", old.Name);
            return rand;
        }

        static SCode InlineMake (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                //(rator == Primitive.Cons) ? PrimitiveCons.Make (rator, rand0, rand1) :
                //(rator == Primitive.IsEq) ? PrimitiveIsEq.Make (rator, rand0, rand1) :
                //(rator == Primitive.ObjectIsEq) ? PrimitiveIsObjectEq.Make (rator, rand0, rand1) :
                //(rator == Primitive.CharIsEq) ? PrimitiveIsCharEq.Make (rator, rand0, rand1) :
                //(rator == Primitive.IntIsEq) ? PrimitiveIsIntEq.Make (rator, rand0, rand1) :
                //(rator == Primitive.IsFixnumEqual) ? PrimitiveIsFixnumEqual.Make (rator, rand0, rand1) :
                //(rator == Primitive.GeneralCarCdr) ? PrimitiveGeneralCarCdr.Make (rator, rand0, rand1) :
                //(rator == Primitive.GreaterThanFixnum) ? PrimitiveGreaterThanFixnum.Make (rator, rand0, rand1) :
                //(rator == Primitive.LessThanFixnum) ? PrimitiveLessThanFixnum.Make (rator, rand0, rand1) :
                //(rator == Primitive.PlusFixnum) ? PrimitivePlusFixnum.Make (rator, rand0, rand1) :
                //(rator == Primitive.IsObjectType) ? PrimitiveIsObjectType.Make (rator, rand0, rand1) :
                //(rator == Primitive.RecordRef) ? PrimitiveRecordRef.Make (rator, rand0, rand1) :
                //(rator == Primitive.Vector8BRef) ? PrimitiveVector8BRef.Make (rator, rand0, rand1) :
                //(rator == Primitive.VectorRef) ? PrimitiveVectorRef.Make (rator, rand0, rand1) :
                 SpecializedMake (rator, rand0, rand1);
        }

        static SCode SpecializedMake (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                //(rand0 is PrimitiveCar) ? PrimitiveCombination2Car.Make (rator, (PrimitiveCar) rand0, rand1) :
                //(rand0 is PrimitiveCaar) ? PrimitiveCombination2Caar.Make (rator, (PrimitiveCaar) rand0, rand1) :
                (rand0 is Argument) ? PrimitiveCombination2A.Make (rator, (Argument) rand0, rand1) :
                (rand0 is StaticVariable) ? PrimitiveCombination2S.Make (rator, (StaticVariable) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveCombination2Q.Make (rator, (Quotation) rand0, rand1) :
                (rand1 is Argument) ? PrimitiveCombination2XA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveCombination2XS.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveCombination2XQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2 (rator, rand0, rand1);
        }

        //static SCode SpecialMake (Primitive2 rator, Quotation rand0, SCode rand1)
        //{
        //    if (rator == Primitive.BoolIsEq) {
        //        return Rewrite (rator, ((bool) rand0.Quoted) ? Primitive.IsSharpT : Primitive.Not, rand1);
        //    }
        //    else if (rator == Primitive.IntIsEq) {
        //        if ((int) rand0.Quoted == 0)
        //            return Rewrite (rator, Primitive.ObjectIsZero, rand1);
        //        else
        //            return StandardMake (rator, rand0, rand1);
        //    }
        //    else if (rator == Primitive.IsEq) {
        //        if (rand0.Quoted == null)
        //            return Rewrite (rator, Primitive.IsNull, rand1);
        //        else if (rand0.Quoted is char)
        //            return Rewrite (rator, Primitive.CharIsEq, rand0, rand1);
        //        else if (rand0.Quoted is string     ||
        //                 rand0.Quoted is Primitive  ||
        //                 rand0.Quoted is ReturnCode ||
        //                 rand0.Quoted is Symbol)
        //            return Rewrite (rator, Primitive.ObjectIsEq, rand0, rand1);
        //        else if (rand0.Quoted is bool)
        //            return Rewrite (rator, Primitive.BoolIsEq, rand0, rand1);
        //        else if (rand0.Quoted is int)
        //            return Rewrite (rator, Primitive.IntIsEq, rand0, rand1);
        //        else
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.FixnumIsGreaterThan) {
        //        if ((int) rand0.Quoted == 0) {
        //            return Rewrite (rator, Primitive.FixnumIsNegative, rand1);
        //        }
        //        else if ((int) rand0.Quoted != 2
        //            && (int) rand0.Quoted != 10)
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.FixnumMultiply) {
        //        if ((int) rand0.Quoted <= 1) {
        //            Debugger.Break ();
        //        }
        //    }
        //    else if (rator == Primitive.IsFixnumEqual) {
        //        if ((int) rand0.Quoted == 0)
        //            return Rewrite (rator, Primitive.IsFixnumZero, rand1);
        //        else if ((int) rand0.Quoted != 1
        //            && (int) rand0.Quoted != 2
        //            && (int) rand0.Quoted != 4
        //            && (int) rand0.Quoted != 5
        //            && (int) rand0.Quoted != 8
        //            && (int) rand0.Quoted != 10
        //            && (int) rand0.Quoted != 14
        //            && (int) rand0.Quoted != 15
        //            && (int) rand0.Quoted != 34
        //            && (int) rand0.Quoted != 39
        //            && (int) rand0.Quoted != 128
        //            && (int) rand0.Quoted != 55296
        //            && (int) rand0.Quoted != 56320
        //            )
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.LessThanFixnum) {
        //        if ((int) rand0.Quoted == 0) {
        //            return Rewrite (rator, Primitive.PositiveFixnum, rand1);
        //        }
        //        Debugger.Break ();
        //    }
        //    else if (rator == Primitive.FixnumAnd) {
        //        if ((int) rand0.Quoted != 1 &&
        //            (int) rand0.Quoted != 2 &&
        //            (int) rand0.Quoted != 4 &&
        //            (int) rand0.Quoted != 8 &&
        //            (int) rand0.Quoted != 12 &&
        //            (int) rand0.Quoted != 15 &&
        //            (int) rand0.Quoted != 16 &&
        //            (int) rand0.Quoted != 32 &&
        //            (int) rand0.Quoted != 64 &&
        //            (int) rand0.Quoted != 128 &&
        //            (int) rand0.Quoted != 192 &&
        //            (int) rand0.Quoted != 255 &&
        //            (int) rand0.Quoted != 63488 &&
        //            (int) rand0.Quoted != 64512 &&
        //            (int) rand0.Quoted != 65534 &&
        //            (int) rand0.Quoted != 1048576)
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.FixnumLsh) {
        //        if ((int) rand0.Quoted != 1
        //            && (int) rand0.Quoted != 255)
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.FixnumOr) {
        //        if ((int) rand0.Quoted == 0)
        //            return Simplify (rator, rand1);
        //        else if ((int) rand0.Quoted != 128
        //                 && (int) rand0.Quoted != 192
        //                 && (int) rand0.Quoted != 224
        //                 && (int) rand0.Quoted != 240
        //            && (int) rand0.Quoted != 55296
        //            && (int) rand0.Quoted != 56320
        //            )
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.FixnumSubtract) {
        //        if ((int) rand0.Quoted == 0)
        //            return Rewrite (rator, Primitive.FixnumNegate, rand1);
        //        else if ((int) rand0.Quoted != 8)
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.FlonumMultiply) {
        //        if ((double) rand0.Quoted != 2
        //            && (double) rand0.Quoted != 4.3429448190325175)

        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.IntegerAdd) {
        //        if ((int) rand0.Quoted != 2)
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.IntegerIsEqual) {
        //        if ((int) rand0.Quoted != 1
        //            && (int) rand0.Quoted != 2)
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.IntegerMultiply) {
        //        if (rand0.Quoted is int
        //            && (int) rand0.Quoted != 2
        //            && (int) rand0.Quoted != 100000)
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.IntegerShiftLeft) {
        //        if ((int) rand0.Quoted != 1)
        //            Debugger.Break ();
        //    }
        //    else if (rator == Primitive.PlusFixnum) {
        //        if ((int) rand0.Quoted == 1)
        //            return Rewrite (rator, Primitive.FixnumAdd1, rand1);
        //    }
        //    else if (true
        //        && !(rand1 is Quotation)
        //        && rator != Primitive.CharIsEq
        //        && rator != Primitive.Cons
        //        && rator != Primitive.FlonumDivide
        //        && rator != Primitive.FlonumExpt
        //        && rator != Primitive.FlonumSubtract
        //        // Don't rewrite generics because we don't know the ultimate
        //        // semantics.
        //        && rator != Primitive.GenericAdd
        //        && rator != Primitive.GenericDivide
        //        && rator != Primitive.GenericIsEqual
        //        && rator != Primitive.GenericIsLessThan
        //        && rator != Primitive.GenericMultiply
        //        && rator != Primitive.GenericSubtract
        //        && rator != Primitive.IsObjectType
        //        && rator != Primitive.MapCodeToMachineAddress
        //        && rator != Primitive.MapMachineAddressToCode
        //        && rator != Primitive.ObjectIsEq
        //        && rator != Primitive.ObjectSetType
        //        && rator != Primitive.PrimitiveIsObjectType
        //        && rator != Primitive.PrimitiveObjectSetType
        //        && rator != Primitive.StringRef
        //        && rator != Primitive.SystemListToVector
        //        && rator != Primitive.VectorCons
        //        && rator != Primitive.VectorRef
        //        && rator != Primitive.WithInterruptMask
        //        )
        //        Debugger.Break ();
        //    return StandardMake (rator, rand0, rand1);
        //}


        //static SCode SpecialMake (Primitive2 rator, SCode rand0, Quotation rand1)
        //{
        //    return
        //         (rator == Primitive.GreaterThanFixnum) ? (((int) rand1.Quoted == 0)
        //                                                  ? Rewrite (rator, Primitive.PositiveFixnum, rand0)
        //                                                  : ((int) rand1.Quoted >= 1) ? StandardMake (rator, rand0, rand1)
        //                                                  : Break())
        //        : (rator == Primitive.IntegerAdd) ? ((rand1.Quoted is long && (long) rand1.Quoted == 4294967291)
        //        ? StandardMake (rator, rand0, rand1) 
        //        : ((int) rand1.Quoted == 1) ? Rewrite (rator, Primitive.IntegerAdd1, rand0)
        //        : Break())
        //         : (rator == Primitive.IntegerIsGreater) ? ((rand1.Quoted is long && (long) rand1.Quoted == 4294967291)
        //                                                        ? StandardMake (rator, rand0, rand1) 
        //                                                        : ((int) rand1.Quoted == 0) ? Rewrite (rator, Primitive.IntegerIsPositive, rand0)
        //                                                        : Break ())
        //        : (rator == Primitive.IntegerMultiply) ? ((
        //        (rand1.Quoted is int && (int) rand1.Quoted == 2)
        //        || (rand1.Quoted is long && (long) rand1.Quoted == 4294967291))
        //        ? StandardMake (rator, rand0, rand1) 

        //        : Break ())
        //        : (rator == Primitive.IntegerSubtract) ? (((int) rand1.Quoted == 1) ? Rewrite (rator, Primitive.IntegerSub1, rand0) : Break())
        //        : (rator == Primitive.IsFixnumEqual) ? (((int) rand1.Quoted >= 1)
        //                                             ? StandardMake (rator, rand0, rand1)
        //                                             : ((int) rand1.Quoted == 0) ? Rewrite (rator, Primitive.IsFixnumZero, rand0)
        //                                             : Break())
        //        : (rator == Primitive.LessThanFixnum) ? (((int) rand1.Quoted == 0)
        //                                                  ? Rewrite (rator, Primitive.FixnumIsNegative, rand0)
        //                                                  : ((int) rand1.Quoted == 1) ? Rewrite1 (rand0)
        //                                                  :((int) rand1.Quoted == -4
        //                                                  || (int) rand1.Quoted >= 2) ? StandardMake (rator, rand0, rand1)
        //                                                  : Break ())
        //        : (rator == Primitive.FixnumSubtract) ? (((int) rand1.Quoted == 0) ? Simplify (rator, rand0)
        //                                                 : ((int) rand1.Quoted == 1) ? Rewrite (rator, Primitive.FixnumSub1, rand0)
        //                                                 : ((int) rand1.Quoted >= 2) ? StandardMake (rator, rand0, rand1)
        //                                                 :Break())
        //         : (rator == Primitive.FixnumAnd) ? (((int) rand1.Quoted == 1 
        //                                             || (int) rand1.Quoted == 7
        //                                             || (int) rand1.Quoted == 15
        //                                             || (int) rand1.Quoted == 31
        //                                             || (int) rand1.Quoted == 63
        //                                             || (int) rand1.Quoted == 240
        //                                             || (int) rand1.Quoted == 255
        //                                             || (int) rand1.Quoted == 1023
        //                                             || (int) rand1.Quoted == 2097151)
        //                                             ? StandardMake (rator, rand0, rand1)
        //                                             : Break ())
        //        : (rator == Primitive.FixnumXor) ? (((int) rand1.Quoted == 1)
        //                                             ? StandardMake (rator, rand0, rand1)
        //                                             : Break())
        //         : (rator == Primitive.FixnumOr) ? (((int) rand1.Quoted >= 1)
        //                                             ? StandardMake (rator, rand0, rand1)
        //                                             : Break ())
        //       : (rator == Primitive.FixnumLsh) ? (((int) rand1.Quoted >= 1 
        //                                           || (int) rand1.Quoted <= -1)
        //                                             ? StandardMake (rator, rand0, rand1)
        //                                             : (int) rand1.Quoted == 0 ? Simplify (rator, rand0)
        //                                             : Break ())
        //         : (rator == Primitive.FlonumAdd) ? (((double) rand1.Quoted == 1.0
        //                                              || (double) rand1.Quoted == 4294967291.0)
        //                                             ? StandardMake (rator, rand0, rand1)
        //                                             : Break())
        //        : (rator == Primitive.FlonumIsGreaterThan) ? (((double) rand1.Quoted ==1.0)
        //                                                        ? StandardMake (rator, rand0, rand1)
        //                                                        : ((double) rand1.Quoted == 0.0) ? Rewrite (rator, Primitive.FlonumIsPositive, rand0)
        //                                                        : Break())
        //        : (rator == Primitive.FlonumIsLessThan) ? (((double) rand1.Quoted == -1.0)
        //                                                        ? StandardMake (rator, rand0, rand1)
        //                                                        : ((double) rand1.Quoted == 0.0) ? Rewrite (rator, Primitive.FlonumIsNegative, rand0)
        //                                                        : Break ())
        //        :(rator == Primitive.GeneralCarCdr) ? (((int) rand1.Quoted >= 4)
        //                                               ? StandardMake (rator, rand0, rand1)
        //                                               : Break())
        //        : (rator == Primitive.IsEq) ? ((rand1.Quoted is string ||
        //                                       rand1.Quoted is char[] ||
        //                                       rand1.Quoted is Constant ||
        //                                       rand1.Quoted is Primitive ||
        //                                       rand1.Quoted is ReturnCode ||
        //                                       rand1.Quoted is Symbol)
        //                                     ? Rewrite (rator, Primitive.ObjectIsEq, rand0, rand1)
        //                                     : (rand1.Quoted is char) ? Rewrite (rator, Primitive.CharIsEq, rand0, rand1)
        //                                     : (rand1.Quoted is int) ? Rewrite (rator, Primitive.IntIsEq, rand0, rand1)
        //                                     : (rand1.Quoted is bool && (bool) rand1.Quoted) ? Rewrite (rator, Primitive.IsSharpT, rand0)
        //                                     : (rand1.Quoted is bool && !((bool) rand1.Quoted)) ? Rewrite (rator, Primitive.Not, rand0)
        //                                     : Break())
        //        : (rator == Primitive.IntIsEq) ? (((int) rand1.Quoted >= 1 
        //                                           || (int) rand1.Quoted == -2
        //                                            ) ? StandardMake (rator, rand0, rand1)
        //                                       : ((int) rand1.Quoted == 0) ? Rewrite (rator, Primitive.ObjectIsZero, rand0)
        //                                     : Break ())
        //        : (rator == Primitive.PlusFixnum) ? (((int) rand1.Quoted == 1) ? Rewrite (rator, Primitive.FixnumAdd1, rand0)
        //                                              : ((int) rand1.Quoted >= 2) ? StandardMake (rator, rand0, rand1)
        //                                                  : ((int) rand1.Quoted == 0) ? Simplify (rator, rand0)
        //                                              : Break())
        //  : (rator == Primitive.FixnumMultiply) ? (((int) rand1.Quoted == 256 ||
        //                                            (int) rand1.Quoted == 16
        //                                          || (int) rand1.Quoted == 2
        //                                                  ) ? StandardMake (rator, rand0, rand1)
        //                                              : Break ())
        //        : (true 
        //           && rator != Primitive.CharIsEq
        //           && rator != Primitive.Cons
        //           && rator != Primitive.FixnumQuotient
        //           && rator != Primitive.FileAccess
        //           && rator != Primitive.FixnumRemainder
        //           && rator != Primitive.FloatingVectorRef
        //           && rator != Primitive.FlonumIsEqual
        //           && rator != Primitive.FlonumDivide
        //           && rator != Primitive.FlonumSubtract
        //           && rator != Primitive.GenericIsGreaterThan
        //           && rator != Primitive.GenericIsEqual
        //           && rator != Primitive.GenericIsLessThan
        //           && rator != Primitive.GenericMultiply
        //           && rator != Primitive.GenericDivide
        //           && rator != Primitive.GenericAdd
        //           && rator != Primitive.GenericSubtract
        //           && rator != Primitive.GetServiceByName
        //           && rator != Primitive.IntegerIsEqual
        //           && rator != Primitive.IntegerToFlonum
        //           && rator != Primitive.IntegerRemainder
        //           && rator != Primitive.IntegerQuotient
        //           && rator != Primitive.IntegerDivide
        //           && rator != Primitive.LexicalReference
        //           && rator != Primitive.LexicalUnreferenceable
        //           && rator != Primitive.MakeBitString
        //           && rator != Primitive.ObjectIsEq
        //           && rator != Primitive.PrimitiveAddress
        //           && rator != Primitive.PrimitiveObjectEq
        //           && rator != Primitive.PrimitiveObjectRef
        //           && rator != Primitive.Quotient
        //           && rator != Primitive.RealTimerSet
        //           && rator != Primitive.RecordRef
        //           && rator != Primitive.Remainder
        //           && rator != Primitive.StringRef
        //           && rator != Primitive.SetCar
        //           && rator != Primitive.SetCdr
        //           && rator != Primitive.ShutdownSocket
        //           && rator != Primitive.SystemPairSetCar
        //           && rator != Primitive.SystemVectorRef
        //           && rator != Primitive.VectorCons
        //           && rator != Primitive.VectorRef
        //           && rator != Primitive.Vector8BRef
        //           && rator != Primitive.Win32ExpandEnvironmentStrings) ? Break ()
        //        : StandardMake (rator, rand0, rand1);
        //}

        static SCode OptimizedMake (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return Configuration.EnablePrimitive2Specialization ? SpecializedMake (rator, rand0, rand1) :
                new PrimitiveCombination2 (rator, rand0, rand1);
        }

        public static SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            else
                return
                    (!Configuration.EnablePrimitiveCombination2Optimization) ? new PrimitiveCombination2 (rator, rand0, rand1) :
                    OptimizedMake (rator, rand0, rand1);
                    ////(Configuration.EnableCodeRewriting && rand0 is Quotation) ? SpecialMake (rator, (Quotation) rand0, rand1) :
                    ////(Configuration.EnableCodeRewriting && rand1 is Quotation) ? SpecialMake (rator, rand0, (Quotation) rand1) :
                    //(!Configuration.EnablePrimitive2Specialization) ? new PrimitiveCombination2 (rator, rand0, rand1) :
                    //(Configuration.EnableInlinePrimitive2) ? InlineMake (rator, rand0, rand1) :
                    //SpecializedMake (rator, rand0, rand1);
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

        public override bool CallsTheEnvironment ()
        {
            return this.rand0.CallsTheEnvironment ()
                || this.rand1.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
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
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
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

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult r0 = this.rand0.PartialEval (environment);
            PartialResult r1 = this.rand1.PartialEval (environment);
            return new PartialResult (r0.Residual == this.rand0 &&
                r1.Residual == this.rand1 ? this : PrimitiveCombination2.Make (this.rator, r0.Residual, r1.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.Operand0.CollectFreeVariables (freeVariableSet);
            this.Operand1.CollectFreeVariables (freeVariableSet);
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
        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
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
            Control unev = this.expression.Operand0;
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
    class PrimitiveCombination2A : PrimitiveCombination2
    {
        protected readonly int rand0Offset;

#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination2A (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.Offset;
        }

        static public PrimitiveCombination2A Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveCombination2A0.Make (rator, (Argument0) rand0, rand1) :
                (rand0 is Argument1) ? PrimitiveCombination2A1.Make (rator, (Argument1) rand0, rand1) :
                (rand1 is Argument) ? PrimitiveCombination2AA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveCombination2AS.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveCombination2AQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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
    class PrimitiveCombination2A0 : PrimitiveCombination2A
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination2A0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2A0 Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveCombination2A0A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveCombination2A0S.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveCombination2A0Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
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

    [Serializable]
    class PrimitiveCombination2A0A : PrimitiveCombination2A0
    {
        readonly int rand1Offset;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        protected PrimitiveCombination2A0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        static public PrimitiveCombination2A0A Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveCombination2A0A0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveCombination2A0A1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2A0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset))) {
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
    sealed class PrimitiveCombination2A0A0 : PrimitiveCombination2A0A
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2A0A0 (Primitive2 rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2A0A Make (Primitive2 rator, Argument0 rand0, Argument0 rand1)
        {
            return
                new PrimitiveCombination2A0A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object arg = environment.Argument0Value;
            if (this.method (out answer, arg, arg)) {
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
    sealed class PrimitiveCombination2A0A1 : PrimitiveCombination2A0A
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2A0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2A0A1 Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveCombination2A0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
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

    [Serializable]
    sealed class PrimitiveCombination2A0S : PrimitiveCombination2A0
    {
        readonly Symbol rand1Name;
        readonly int rand1Offset;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2A0S (Primitive2 rator, Argument0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        static public PrimitiveCombination2A0S Make (Primitive2 rator, Argument0 rand0, StaticVariable rand1)
        {
            return
                new PrimitiveCombination2A0S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
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

    [Serializable]
    sealed class PrimitiveCombination2A0Q : PrimitiveCombination2A0
    {
        readonly object rand1Value;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2A0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static public PrimitiveCombination2A0Q Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveCombination2A0Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

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

    [Serializable]
    class PrimitiveCombination2A1 : PrimitiveCombination2A
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination2A1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2A1 Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveCombination2A1A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveCombination2A1S.Make (rator, rand0, (StaticVariable) rand1):
                (rand1 is Quotation) ? PrimitiveCombination2A1Q.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

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

    [Serializable]
    class PrimitiveCombination2A1A : PrimitiveCombination2A1
    {
        readonly int rand1Offset;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        protected PrimitiveCombination2A1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        static public PrimitiveCombination2A1A Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveCombination2A1A0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveCombination2A1A1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2A1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

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

    [Serializable]
    sealed class PrimitiveCombination2A1A0 : PrimitiveCombination2A1A
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2A1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2A1A Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                new PrimitiveCombination2A1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
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

    [Serializable]
    sealed class PrimitiveCombination2A1A1 : PrimitiveCombination2A1A
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2A1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2A1A1 Make (Primitive2 rator, Argument1 rand0, Argument1 rand1)
        {
            return
                new PrimitiveCombination2A1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object arg = environment.Argument1Value;
            if (this.method (out answer, arg, arg)) {
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
    sealed class PrimitiveCombination2A1S : PrimitiveCombination2A1
    {
        readonly Symbol rand1Name;
        readonly int rand1Offset;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2A1S (Primitive2 rator, Argument1 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        static public PrimitiveCombination2A1S Make (Primitive2 rator, Argument1 rand0, StaticVariable rand1)
        {
            return
                new PrimitiveCombination2A1S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
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

    [Serializable]
    sealed class PrimitiveCombination2A1Q : PrimitiveCombination2A1
    {
        readonly object rand1Value;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2A1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static public PrimitiveCombination2A1Q Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveCombination2A1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

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

    [Serializable]
    class PrimitiveCombination2AA : PrimitiveCombination2A
    {
        readonly int rand1Offset;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        protected PrimitiveCombination2AA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        static public PrimitiveCombination2AA Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveCombination2AA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveCombination2AA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2AA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

            if (this.method (out answer, environment.ArgumentValue(this.rand0Offset), environment.ArgumentValue (this.rand1Offset))) {
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
    sealed class PrimitiveCombination2AA0 : PrimitiveCombination2AA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2AA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2AA Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return
                new PrimitiveCombination2AA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, environment.ArgumentValue(this.rand0Offset), environment.Argument0Value)) {
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
    sealed class PrimitiveCombination2AA1 : PrimitiveCombination2AA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2AA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2AA1 Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return
                new PrimitiveCombination2AA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, environment.ArgumentValue(this.rand0Offset), environment.Argument1Value)) {
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
    sealed class PrimitiveCombination2AS : PrimitiveCombination2A
    {
        readonly Symbol rand1Name;
        readonly int rand1Offset;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2AS (Primitive2 rator, Argument rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        static public PrimitiveCombination2AS Make (Primitive2 rator, Argument rand0, StaticVariable rand1)
        {
            return
                new PrimitiveCombination2AS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), ev1)) {
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
    sealed class PrimitiveCombination2AQ : PrimitiveCombination2A
    {
        readonly object rand1Value;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2AQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static public PrimitiveCombination2AQ Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveCombination2AQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

            if (this.method (out answer, environment.ArgumentValue(this.rand0Offset), this.rand1Value)) {
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
    class PrimitiveCombination2S : PrimitiveCombination2
    {
        protected readonly Symbol rand0Name;
        protected readonly int rand0Offset;

#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination2S (Primitive2 rator, StaticVariable rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.Name;
            this.rand0Offset = rand0.Offset;
        }

        static public PrimitiveCombination2S Make (Primitive2 rator, StaticVariable rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveCombination2SA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveCombination2SS.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveCombination2SQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2S (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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
    class PrimitiveCombination2SA : PrimitiveCombination2S
    {
        protected readonly int rand1Offset;

#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        protected PrimitiveCombination2SA (Primitive2 rator, StaticVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        static public PrimitiveCombination2SA Make (Primitive2 rator, StaticVariable rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveCombination2SA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveCombination2SA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2SA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif


            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            if (this.method (out answer, ev0, environment.ArgumentValue(this.rand1Offset))) {
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
    sealed class PrimitiveCombination2SA0 : PrimitiveCombination2SA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2SA0 (Primitive2 rator, StaticVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2SA0 Make (Primitive2 rator, StaticVariable rand0, Argument0 rand1)
        {
            return
                new PrimitiveCombination2SA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            if (this.method (out answer, ev0, environment.Argument0Value)) {
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
    sealed class PrimitiveCombination2SA1 : PrimitiveCombination2SA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2SA1 (Primitive2 rator, StaticVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2SA1 Make (Primitive2 rator, StaticVariable rand0, Argument1 rand1)
        {
            return
                new PrimitiveCombination2SA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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

    [Serializable]
    sealed class PrimitiveCombination2SS : PrimitiveCombination2S
    {
        readonly Symbol rand1Name;
        readonly int rand1Offset;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2SS (Primitive2 rator, StaticVariable rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
        this.rand1Offset = rand1.Offset;
        }

        static public PrimitiveCombination2SS Make (Primitive2 rator, StaticVariable rand0, StaticVariable rand1)
        {
            return
                new PrimitiveCombination2SS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif


            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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
    sealed class PrimitiveCombination2SQ : PrimitiveCombination2S
    {
        readonly object rand1Value;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2SQ (Primitive2 rator, StaticVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static public PrimitiveCombination2SQ Make (Primitive2 rator, StaticVariable rand0, Quotation rand1)
        {
            return
                new PrimitiveCombination2SQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
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

    [Serializable]
    class PrimitiveCombination2Q : PrimitiveCombination2
    {
        protected readonly object rand0Value;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination2Q (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        static public SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveCombination2QA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is StaticVariable) ? PrimitiveCombination2QS.Make (rator, rand0, (StaticVariable) rand1) :
                (rand1 is Quotation) ? PrimitiveCombination2QQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveCombination2Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            ratorHistogram.Note (this.rator);
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

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
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
    class PrimitiveCombination2QA : PrimitiveCombination2Q
    {
        protected readonly int rand1Offset;

#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        protected PrimitiveCombination2QA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        static public SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveCombination2QA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveCombination2QA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2QA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, this.rand0Value, environment.ArgumentValue (this.rand1Offset))) {
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
    sealed class PrimitiveCombination2QA0 : PrimitiveCombination2QA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2QA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                new PrimitiveCombination2QA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
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

    [Serializable]
    sealed class PrimitiveCombination2QA1 : PrimitiveCombination2QA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2QA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public PrimitiveCombination2QA1 Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                new PrimitiveCombination2QA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
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

    [Serializable]
    sealed class PrimitiveCombination2QS : PrimitiveCombination2Q
    {
        readonly Symbol rand1Name;
        readonly int rand1Offset;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2QS (Primitive2 rator, Quotation rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        static public SCode Make (Primitive2 rator, Quotation rand0, StaticVariable rand1)
        {
            return
                new PrimitiveCombination2QS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
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
        readonly object rand1Value;
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2QQ (Primitive2 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static public SCode Make (Primitive2 rator, Quotation rand0, Quotation rand1)
        {
            return
                new PrimitiveCombination2QQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
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

    [Serializable]
    class PrimitiveCombination2XA : PrimitiveCombination2
    {
        protected readonly int rand1Offset;

#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        protected PrimitiveCombination2XA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        static public SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveCombination2XA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveCombination2XA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveCombination2XA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
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
    sealed class PrimitiveCombination2XA0 : PrimitiveCombination2XA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2XA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return
                new PrimitiveCombination2XA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            object ev1 = environment.Argument0Value;

            Control unev = this.rand0;
            Environment env = environment;
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
    sealed class PrimitiveCombination2XA1 : PrimitiveCombination2XA
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2XA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        static public SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return
                new PrimitiveCombination2XA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            object ev1 = environment.Argument1Value;

            Control unev = this.rand0;
            Environment env = environment;
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
    sealed class PrimitiveCombination2XS : PrimitiveCombination2
    {
        readonly Symbol rand1Name;
        readonly int rand1Offset;

#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif

        PrimitiveCombination2XS (Primitive2 rator, SCode rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        static public SCode Make (Primitive2 rator, SCode rand0, StaticVariable rand1)
        {
            return
                new PrimitiveCombination2XS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
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
    sealed class PrimitiveCombination2XQ : PrimitiveCombination2
    {
        readonly object rand1Value;
#if DEBUG
        [NonSerialized]
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif
        PrimitiveCombination2XQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted; ;
        }

        static public SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveCombination2XQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, this.rand1Value));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
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

////    [Serializable]
////    class PrimitiveCombination2Car : PrimitiveCombination2
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif
////        public readonly SCode rand0rand0;

////        protected PrimitiveCombination2Car (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand0rand0 = rand0.Operand;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
////        {
////            return
////                (rand0 is PrimitiveCarL) ? PrimitiveCombination2CarL.Make (rator, (PrimitiveCarL) rand0, rand1) :
////                (rand1 is LexicalVariable) ? PrimitiveCombination2CarSL.Make (rator, rand0, (LexicalVariable) rand1) :
////                (rand1 is Quotation) ? PrimitiveCombination2CarSQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2Car (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2Car.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            unev = this.rand0rand0;
////            env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarL : PrimitiveCombination2Car
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif
////        public readonly object rand0Name;
////        public readonly uint rand0Depth;
////        public readonly uint rand0Offset;

////        protected PrimitiveCombination2CarL (Primitive2 rator, PrimitiveCarL rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand0Name = rand0.OperandName;
////            this.rand0Depth = rand0.OperandDepth;
////            this.rand0Offset = rand0.OperandOffset;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, SCode rand1)
////        {
////            return
////                (rand0 is PrimitiveCarA) ? PrimitiveCombination2CarA.Make (rator, (PrimitiveCarA) rand0, rand1) :
////                (rand0 is PrimitiveCarL1) ? PrimitiveCombination2CarL1.Make (rator, (PrimitiveCarL1) rand0, rand1) :
////                (rand1 is LexicalVariable) ? PrimitiveCombination2CarLL.Make (rator, rand0, (LexicalVariable) rand1) :
////                (rand1 is Quotation) ? PrimitiveCombination2CarLQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CarL (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CarL.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarA : PrimitiveCombination2CarL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveCombination2CarA (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
////        {
////            return
////                (rand0 is PrimitiveCarA0) ? PrimitiveCombination2CarA0.Make (rator, (PrimitiveCarA0) rand0, rand1) :
////                (rand0 is PrimitiveCarA1) ? PrimitiveCombination2CarA1.Make (rator, (PrimitiveCarA1) rand0, rand1) :
////                //(rand1 is LexicalVariable) ? PrimitiveCombination2CarLL.Make (rator, rand0, (LexicalVariable) rand1) :
////                //(rand1 is Quotation) ? PrimitiveCombination2CarLQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CarA (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CarA.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) closureEnvironment.ArgumentValue (this.rand0Offset)).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarA0 : PrimitiveCombination2CarA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveCombination2CarA0 (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
////        {
////            return
////                (rand1 is LexicalVariable) ? PrimitiveCombination2CarA0L.Make (rator, rand0, (LexicalVariable) rand1) :
////                (rand1 is Quotation) ? PrimitiveCombination2CarA0Q.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CarA0 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CarA0.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) closureEnvironment.Argument0Value).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarA0L : PrimitiveCombination2CarA0
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Name;
////        public readonly int rand1Depth;
////        public readonly int rand1Offset;

////        protected PrimitiveCombination2CarA0L (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Name = rand1.Name;
////            this.rand1Depth = rand1.Depth;
////            this.rand1Offset = rand1.Offset;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable rand1)
////        {
////            return
////                (rand1 is Argument) ? PrimitiveCombination2CarA0A.Make (rator, rand0, (Argument) rand1) :
////                (rand1 is LexicalVariable1) ? PrimitiveCombination2CarA0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
////                new PrimitiveCombination2CarA0L (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarA0L.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) closureEnvironment.Argument0Value).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarA0A : PrimitiveCombination2CarA0L
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        protected PrimitiveCombination2CarA0A (Primitive2 rator, PrimitiveCarA0 rand0, Argument rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Argument rand1)
////        {
////            return
////                (rand1 is Argument0) ? Unimplemented () :
////                (rand1 is Argument1) ? PrimitiveCombination2CarA0A1.Make (rator, rand0, (Argument1) rand1) :
////                new PrimitiveCombination2CarA0A (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) closureEnvironment.Argument0Value).Car, closureEnvironment.ArgumentValue (this.rand1Offset))) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarA0A1 : PrimitiveCombination2CarA0A
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CarA0A1 (Primitive2 rator, PrimitiveCarA0 rand0, Argument1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Argument1 rand1)
////        {
////            return
////                new PrimitiveCombination2CarA0A1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) closureEnvironment.Argument0Value).Car, closureEnvironment.Argument1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarA0L1 : PrimitiveCombination2CarA0L
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CarA0L1 (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, LexicalVariable1 rand1)
////        {
////            return
////                new PrimitiveCombination2CarA0L1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarA0L1.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) closureEnvironment.Argument0Value).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarA0Q : PrimitiveCombination2CarA0
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Value;

////        PrimitiveCombination2CarA0Q (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Value = rand1.Quoted;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
////        {
////            return
////                new PrimitiveCombination2CarA0Q (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) closureEnvironment.Argument0Value).Car, this.rand1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarA1 : PrimitiveCombination2CarA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveCombination2CarA1 (Primitive2 rator, PrimitiveCarA1 rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarA1 rand0, SCode rand1)
////        {
////            return
////                //(rand1 is LexicalVariable) ? PrimitiveCombination2CarLL.Make (rator, rand0, (LexicalVariable) rand1) :
////                //(rand1 is Quotation) ? PrimitiveCombination2CarLQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CarA1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CarA1.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) closureEnvironment.Argument1Value).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarL1 : PrimitiveCombination2CarL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveCombination2CarL1 (Primitive2 rator, PrimitiveCarL1 rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarL1 rand0, SCode rand1)
////        {
////            return
////                //(rand1 is LexicalVariable) ? PrimitiveCombination2CarLL.Make (rator, rand0, (LexicalVariable) rand1) :
////                //(rand1 is Quotation) ? PrimitiveCombination2CarLQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CarL1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CarL1.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            object ev0;
////            if (closureEnvironment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarLL : PrimitiveCombination2CarL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Name;
////        public readonly int rand1Depth;
////        public readonly int rand1Offset;

////        protected PrimitiveCombination2CarLL (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Name = rand1.Name;
////            this.rand1Depth = rand1.Depth;
////            this.rand1Offset = rand1.Offset;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable rand1)
////        {
////            return
////                (rand1 is Argument) ? PrimitiveCombination2CarLA.Make (rator, rand0, (Argument) rand1) :
////                (rand1 is LexicalVariable1) ? PrimitiveCombination2CarLL1.Make (rator, rand0, (LexicalVariable1) rand1) :
////                new PrimitiveCombination2CarLL (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarLL.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
////                throw new NotImplementedException ();

////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarLA : PrimitiveCombination2CarLL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        protected PrimitiveCombination2CarLA (Primitive2 rator, PrimitiveCarL rand0, Argument rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, Argument rand1)
////        {
////            return
////                (rand1 is Argument0) ? PrimitiveCombination2CarLA0.Make (rator, rand0, (Argument0) rand1) :
////                (rand1 is Argument1) ? PrimitiveCombination2CarLA1.Make (rator, rand0, (Argument1) rand1) :
////                new PrimitiveCombination2CarLA (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarLA.EvalStep";
////#endif
////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, closureEnvironment.ArgumentValue (this.rand1Offset))) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarLA0 : PrimitiveCombination2CarLA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CarLA0 (Primitive2 rator, PrimitiveCarL rand0, Argument0 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, Argument0 rand1)
////        {
////            return
////                new PrimitiveCombination2CarLA0 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarLA0.EvalStep";
////#endif
////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, closureEnvironment.Argument0Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarLA1 : PrimitiveCombination2CarLA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CarLA1 (Primitive2 rator, PrimitiveCarL rand0, Argument1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, Argument1 rand1)
////        {
////            return
////                new PrimitiveCombination2CarLA1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarLA1.EvalStep";
////#endif
////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, closureEnvironment.Argument1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarLL1 : PrimitiveCombination2CarLL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif

////        PrimitiveCombination2CarLL1 (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, LexicalVariable1 rand1)
////        {
////            return
////                new PrimitiveCombination2CarLL1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarLL1.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
////                throw new NotImplementedException ();

////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();


////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarLQ : PrimitiveCombination2CarL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Value;

////        PrimitiveCombination2CarLQ (Primitive2 rator, PrimitiveCarL rand0, Quotation rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Value = rand1.Quoted;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCarL rand0, Quotation rand1)
////        {
////            return
////                new PrimitiveCombination2CarLQ (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarLQ.EvalStep";
////#endif
////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, this.rand1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarSL : PrimitiveCombination2Car
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Name;
////        public readonly int rand1Depth;
////        public readonly int rand1Offset;

////        protected PrimitiveCombination2CarSL (Primitive2 rator, PrimitiveCar rand0, LexicalVariable rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Name = rand1.Name;
////            this.rand1Depth = rand1.Depth;
////            this.rand1Offset = rand1.Offset;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, LexicalVariable rand1)
////        {
////            return
////                (rand1 is Argument) ? PrimitiveCombination2CarSA.Make (rator, rand0, (Argument) rand1) :
////                (rand1 is LexicalVariable1) ? PrimitiveCombination2CarSL1.Make (rator, rand0, (LexicalVariable1) rand1) :
////                new PrimitiveCombination2CarSL (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarSL.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
////                throw new NotImplementedException ();
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CarSA : PrimitiveCombination2CarSL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        protected PrimitiveCombination2CarSA (Primitive2 rator, PrimitiveCar rand0, Argument rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, Argument rand1)
////        {
////            return
////                (rand1 is Argument0) ? PrimitiveCombination2CarSA0.Make (rator, rand0, (Argument0) rand1) :
////                (rand1 is Argument1) ? PrimitiveCombination2CarSA1.Make (rator, rand0, (Argument1) rand1) :
////                new PrimitiveCombination2CarSA (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarSA.EvalStep";
////#endif
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, closureEnvironment.ArgumentValue (this.rand1Offset))) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarSA0 : PrimitiveCombination2CarSA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CarSA0 (Primitive2 rator, PrimitiveCar rand0, Argument0 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, Argument0 rand1)
////        {
////            return
////                new PrimitiveCombination2CarSA0 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarSA0.EvalStep";
////#endif
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, closureEnvironment.Argument0Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarSA1 : PrimitiveCombination2CarSA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CarSA1 (Primitive2 rator, PrimitiveCar rand0, Argument1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, Argument1 rand1)
////        {
////            return
////                new PrimitiveCombination2CarSA1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarSA1.EvalStep";
////#endif
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, closureEnvironment.Argument1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarSL1 : PrimitiveCombination2CarSL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif

////        PrimitiveCombination2CarSL1 (Primitive2 rator, PrimitiveCar rand0, LexicalVariable1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, LexicalVariable1 rand1)
////        {
////            return
////                new PrimitiveCombination2CarSL1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarSL1.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
////                throw new NotImplementedException ();
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CarSQ : PrimitiveCombination2Car
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Value;

////        PrimitiveCombination2CarSQ (Primitive2 rator, PrimitiveCar rand0, Quotation rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Value = rand1.Quoted;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, Quotation rand1)
////        {
////            return
////                new PrimitiveCombination2CarSQ (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CarSQ.EvalStep";
////#endif

////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            if (this.method (out answer, ((Cons) ev0).Car, this.rand1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2Caar : PrimitiveCombination2
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif
////        public readonly SCode rand0rand0;

////        protected PrimitiveCombination2Caar (Primitive2 rator, PrimitiveCaar rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand0rand0 = rand0.Operand;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaar rand0, SCode rand1)
////        {
////            return
////                (rand0 is PrimitiveCaarL) ? PrimitiveCombination2CaarL.Make (rator, (PrimitiveCaarL) rand0, rand1) :
////                (rand1 is LexicalVariable) ? PrimitiveCombination2CaarSL.Make (rator, rand0, (LexicalVariable) rand1) :
////                (rand1 is Quotation) ? PrimitiveCombination2CaarSQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2Caar (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2Caar.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            unev = this.rand0rand0;
////            env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarL : PrimitiveCombination2Caar
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif
////        public readonly object rand0Name;
////        public readonly int rand0Depth;
////        public readonly int rand0Offset;

////        protected PrimitiveCombination2CaarL (Primitive2 rator, PrimitiveCaarL rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand0Name = rand0.OperandName;
////            this.rand0Depth = rand0.OperandDepth;
////            this.rand0Offset = rand0.OperandOffset;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarL rand0, SCode rand1)
////        {
////            return
////                (rand0 is PrimitiveCaarA) ? PrimitiveCombination2CaarA.Make (rator, (PrimitiveCaarA) rand0, rand1) :
////                (rand0 is PrimitiveCaarL1) ? PrimitiveCombination2CaarL1.Make (rator, (PrimitiveCaarL1) rand0, rand1):
////                (rand1 is LexicalVariable) ? PrimitiveCombination2CaarLL.Make (rator, rand0, (LexicalVariable) rand1) :
////                (rand1 is Quotation) ? PrimitiveCombination2CaarLQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CaarL (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CaarL.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarA : PrimitiveCombination2CaarL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveCombination2CaarA (Primitive2 rator, PrimitiveCaarA rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarA rand0, SCode rand1)
////        {
////            return
////                (rand0 is PrimitiveCaarA0) ? PrimitiveCombination2CaarA0.Make (rator, (PrimitiveCaarA0) rand0, rand1) :
////                (rand0 is PrimitiveCaarA1) ? PrimitiveCombination2CaarA1.Make (rator, (PrimitiveCaarA1) rand0, rand1) :
////                //(rand1 is LexicalVariable) ? PrimitiveCombination2CaarLL.Make (rator, rand0, (LexicalVariable) rand1) :
////                //(rand1 is Quotation) ? PrimitiveCombination2CaarLQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CaarA (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CaarA.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////            Cons temp = ((Cons) closureEnvironment.ArgumentValue (this.rand0Offset)).Car as Cons;
////            if (temp == null) throw new NotImplementedException ();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarA0 : PrimitiveCombination2CaarA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveCombination2CaarA0 (Primitive2 rator, PrimitiveCaarA0 rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarA0 rand0, SCode rand1)
////        {
////            return
////                (rand1 is LexicalVariable) ? PrimitiveCombination2CaarA0L.Make (rator, rand0, (LexicalVariable) rand1) :
////                (rand1 is Quotation) ? PrimitiveCombination2CaarA0Q.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CaarA0 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CaarA0.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////            Cons temp = ((Cons) closureEnvironment.Argument0Value).Car as Cons;
////            if (temp == null) throw new NotImplementedException ();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarA0L : PrimitiveCombination2CaarA0
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Name;
////        public readonly int rand1Depth;
////        public readonly int rand1Offset;

////        protected PrimitiveCombination2CaarA0L (Primitive2 rator, PrimitiveCaarA0 rand0, LexicalVariable rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Name = rand1.Name;
////            this.rand1Depth = rand1.Depth;
////            this.rand1Offset = rand1.Offset;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarA0 rand0, LexicalVariable rand1)
////        {
////            return
////                (rand1 is Argument) ? PrimitiveCombination2CaarA0A.Make (rator, rand0, (Argument) rand1) :
////                (rand1 is LexicalVariable1) ? PrimitiveCombination2CaarA0L1.Make (rator, rand0, (LexicalVariable1) rand1) :
////                new PrimitiveCombination2CaarA0L (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarA0L.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
////                throw new NotImplementedException ();

////            Cons temp = ((Cons) closureEnvironment.Argument0Value).Car as Cons;
////            if (temp == null) throw new NotImplementedException ();
////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarA0A : PrimitiveCombination2CaarA0L
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        protected PrimitiveCombination2CaarA0A (Primitive2 rator, PrimitiveCaarA0 rand0, Argument rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarA0 rand0, Argument rand1)
////        {
////            return
////                (rand1 is Argument0) ? Unimplemented() :
////                (rand1 is Argument1) ? PrimitiveCombination2CaarA0A1.Make (rator, rand0, (Argument1) rand1) :
////                new PrimitiveCombination2CaarA0A (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            Cons temp = ((Cons) closureEnvironment.Argument0Value).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, closureEnvironment.ArgumentValue(this.rand1Offset))) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarA0A1 : PrimitiveCombination2CaarA0A
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CaarA0A1 (Primitive2 rator, PrimitiveCaarA0 rand0, Argument1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarA0 rand0, Argument1 rand1)
////        {
////            return
////                new PrimitiveCombination2CaarA0A1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            Cons temp = ((Cons) closureEnvironment.Argument0Value).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, closureEnvironment.Argument1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarA0L1 : PrimitiveCombination2CaarA0L
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CaarA0L1 (Primitive2 rator, PrimitiveCaarA0 rand0, LexicalVariable1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarA0 rand0, LexicalVariable1 rand1)
////        {
////            return
////                new PrimitiveCombination2CaarA0L1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarA0L1.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString ();
////#endif
////            Cons temp = ((Cons) closureEnvironment.Argument0Value).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarA0Q : PrimitiveCombination2CaarA0
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Value;

////        PrimitiveCombination2CaarA0Q (Primitive2 rator, PrimitiveCaarA0 rand0, Quotation rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Value = rand1.Quoted;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarA0 rand0, Quotation rand1)
////        {
////            return
////                new PrimitiveCombination2CaarA0Q (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////            Cons temp = ((Cons) closureEnvironment.Argument0Value).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, this.rand1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarA1 : PrimitiveCombination2CaarA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveCombination2CaarA1 (Primitive2 rator, PrimitiveCaarA1 rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarA1 rand0, SCode rand1)
////        {
////            return
////                //(rand1 is LexicalVariable) ? PrimitiveCombination2CaarLL.Make (rator, rand0, (LexicalVariable) rand1) :
////                //(rand1 is Quotation) ? PrimitiveCombination2CaarLQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CaarA1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CaarA1.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////            Cons temp = ((Cons) closureEnvironment.Argument1Value).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarL1 : PrimitiveCombination2CaarL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
////#endif

////        protected PrimitiveCombination2CaarL1 (Primitive2 rator, PrimitiveCaarL1 rand0, SCode rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarL1 rand0, SCode rand1)
////        {
////            return
////                //(rand1 is LexicalVariable) ? PrimitiveCombination2CaarLL.Make (rator, rand0, (LexicalVariable) rand1) :
////                //(rand1 is Quotation) ? PrimitiveCombination2CaarLQ.Make (rator, rand0, (Quotation) rand1) :
////                new PrimitiveCombination2CaarL1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand1);
////            ratorHistogram.Note (this.rator);
////            rand1TypeHistogram.Note (this.rand1Type);
////            SCode.location = "PrimitiveCombination2CaarL1.EvalStep";
////#endif
////            Control unev = this.rand1;
////            Environment env = closureEnvironment;
////            object ev1;
////            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
////            if (ev1 == Interpreter.Unwind) {
////                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
////                answer = Interpreter.Unwind;
////                closureEnvironment = env;
////                return false;
////            }

////            object ev0;
////            if (closureEnvironment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarLL : PrimitiveCombination2CaarL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Name;
////        public readonly int rand1Depth;
////        public readonly int rand1Offset;

////        protected PrimitiveCombination2CaarLL (Primitive2 rator, PrimitiveCaarL rand0, LexicalVariable rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Name = rand1.Name;
////            this.rand1Depth = rand1.Depth;
////            this.rand1Offset = rand1.Offset;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarL rand0, LexicalVariable rand1)
////        {
////            return
////                (rand1 is Argument) ? PrimitiveCombination2CaarLA.Make (rator, rand0, (Argument) rand1) :
////                (rand1 is LexicalVariable1) ? PrimitiveCombination2CaarLL1.Make (rator, rand0, (LexicalVariable1) rand1) :
////                new PrimitiveCombination2CaarLL (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarLL.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
////                throw new NotImplementedException ();

////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarLA : PrimitiveCombination2CaarLL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        protected PrimitiveCombination2CaarLA (Primitive2 rator, PrimitiveCaarL rand0, Argument rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarL rand0, Argument rand1)
////        {
////            return
////                (rand1 is Argument0) ? PrimitiveCombination2CaarLA0.Make (rator, rand0, (Argument0) rand1) :
////                (rand1 is Argument1) ? PrimitiveCombination2CaarLA1.Make (rator, rand0, (Argument1) rand1) :
////                new PrimitiveCombination2CaarLA (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarLA.EvalStep";
////#endif
////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, closureEnvironment.ArgumentValue (this.rand1Offset))) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarLA0 : PrimitiveCombination2CaarLA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CaarLA0 (Primitive2 rator, PrimitiveCaarL rand0, Argument0 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarL rand0, Argument0 rand1)
////        {
////            return
////                new PrimitiveCombination2CaarLA0 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarLA0.EvalStep";
////#endif
////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, closureEnvironment.Argument0Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarLA1 : PrimitiveCombination2CaarLA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CaarLA1 (Primitive2 rator, PrimitiveCaarL rand0, Argument1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarL rand0, Argument1 rand1)
////        {
////            return
////                new PrimitiveCombination2CaarLA1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarLA1.EvalStep";
////#endif
////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, closureEnvironment.Argument1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarLL1 : PrimitiveCombination2CaarLL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif

////        PrimitiveCombination2CaarLL1 (Primitive2 rator, PrimitiveCaarL rand0, LexicalVariable1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarL rand0, LexicalVariable1 rand1)
////        {
////            return
////                new PrimitiveCombination2CaarLL1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarLL1.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
////                throw new NotImplementedException ();

////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();


////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarLQ : PrimitiveCombination2CaarL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Value;

////        PrimitiveCombination2CaarLQ (Primitive2 rator, PrimitiveCaarL rand0, Quotation rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Value = rand1.Quoted;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaarL rand0, Quotation rand1)
////        {
////            return
////                new PrimitiveCombination2CaarLQ (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarLQ.EvalStep";
////#endif
////            object ev0;
////            if (closureEnvironment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
////                throw new NotImplementedException ();

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, this.rand1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarSL : PrimitiveCombination2Caar
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Name;
////        public readonly int rand1Depth;
////        public readonly int rand1Offset;

////        protected PrimitiveCombination2CaarSL (Primitive2 rator, PrimitiveCaar rand0, LexicalVariable rand1)
////            : base (rator, rand0, rand1)
////        {
////            this.rand1Name = rand1.Name;
////            this.rand1Depth = rand1.Depth;
////            this.rand1Offset = rand1.Offset;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaar rand0, LexicalVariable rand1)
////        {
////            return
////                (rand1 is Argument) ? PrimitiveCombination2CaarSA.Make (rator, rand0, (Argument) rand1) :
////                (rand1 is LexicalVariable1) ? PrimitiveCombination2CaarSL1.Make (rator, rand0, (LexicalVariable1) rand1) :
////                new PrimitiveCombination2CaarSL (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarSL.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
////                throw new NotImplementedException ();
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    class PrimitiveCombination2CaarSA : PrimitiveCombination2CaarSL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        protected PrimitiveCombination2CaarSA (Primitive2 rator, PrimitiveCaar rand0, Argument rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaar rand0, Argument rand1)
////        {
////            return
////                (rand1 is Argument0) ? PrimitiveCombination2CaarSA0.Make (rator, rand0, (Argument0) rand1) :
////                (rand1 is Argument1) ? PrimitiveCombination2CaarSA1.Make (rator, rand0, (Argument1) rand1) :
////                new PrimitiveCombination2CaarSA (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarSA.EvalStep";
////#endif
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, closureEnvironment.ArgumentValue(this.rand1Offset))) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarSA0 : PrimitiveCombination2CaarSA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CaarSA0 (Primitive2 rator, PrimitiveCaar rand0, Argument0 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaar rand0, Argument0 rand1)
////        {
////            return
////                new PrimitiveCombination2CaarSA0 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarSA0.EvalStep";
////#endif
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, closureEnvironment.Argument0Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarSA1 : PrimitiveCombination2CaarSA
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        PrimitiveCombination2CaarSA1 (Primitive2 rator, PrimitiveCaar rand0, Argument1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaar rand0, Argument1 rand1)
////        {
////            return
////                new PrimitiveCombination2CaarSA1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarSA1.EvalStep";
////#endif
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, closureEnvironment.Argument1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarSL1 : PrimitiveCombination2CaarSL
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif

////        PrimitiveCombination2CaarSL1 (Primitive2 rator, PrimitiveCaar rand0, LexicalVariable1 rand1)
////            : base (rator, rand0, rand1)
////        {
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaar rand0, LexicalVariable1 rand1)
////        {
////            return
////                new PrimitiveCombination2CaarSL1 (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarSL1.EvalStep";
////#endif
////            object ev1;
////            if (closureEnvironment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
////                throw new NotImplementedException ();
////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, ev1)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }

////    [Serializable]
////    sealed class PrimitiveCombination2CaarSQ : PrimitiveCombination2Caar
////    {
////#if DEBUG
////        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
////#endif
////        public readonly object rand1Value;

////        PrimitiveCombination2CaarSQ (Primitive2 rator, PrimitiveCaar rand0, Quotation rand1)
////            : base (rator, rand0, rand1)
////        {
////              this.rand1Value = rand1.Quoted;
////        }

////        public static SCode Make (Primitive2 rator, PrimitiveCaar rand0, Quotation rand1)
////        {
////            return
////                new PrimitiveCombination2CaarSQ (rator, rand0, rand1);
////        }

////        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
////        {
////#if DEBUG
////            Warm ("-");
////            NoteCalls (this.rand0rand0);
////            ratorHistogram.Note (this.rator);
////            SCode.location = "PrimitiveCombination2CaarSQ.EvalStep";
////#endif

////            Control unev = this.rand0rand0;
////            Environment env = closureEnvironment;
////            object ev0;
////            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
////            if (ev0 == Interpreter.Unwind) {
////                throw new NotImplementedException ();
////                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, closureEnvironment, ev1));
////                //answer = Interpreter.Unwind;
////                //closureEnvironment = env;
////                //return false;
////            }

////            // It is expensive to bounce down to invoke the procedure
////            // we invoke it directly and pass along the ref args.
////#if DEBUG
////            Primitive.hotPrimitives.Note (this.rator);
////            SCode.location = this.rator.Name.ToString();
////#endif
////        Cons temp = ((Cons)ev0).Car as Cons;
////            if (temp == null) throw new NotImplementedException();
////            if (this.method (out answer, temp.Car, this.rand1Value)) {
////                TailCallInterpreter tci = answer as TailCallInterpreter;
////                if (tci != null) {
////                    answer = null;
////                    expression = tci.Expression;
////                    closureEnvironment = tci.Environment;
////                    return true;
////                }
////                else
////                    throw new NotImplementedException ();
////            }
////            else return false;
////        }
////    }


//    [Serializable]
//    class PrimitiveCombination2SL : PrimitiveCombination2
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
//#endif
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected PrimitiveCombination2SL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
//        {
//            return 
//                (rand1 is Argument) ? PrimitiveCombination2SA.Make (rator, rand0, (Argument) rand1) :
//                (rand1 is LexicalVariable1) ? PrimitiveCombination2SL1.Make (rator, rand0, (LexicalVariable1) rand1) :
//                new PrimitiveCombination2SL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.rand0);
//            rand0TypeHistogram.Note (this.rand0Type);
//            SCode.location = "PrimitiveCombination2SL.EvalStep";
//#endif
//            object ev1;
//            if (closureEnvironment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();

//            Control unev = this.rand0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev1 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//#if DEBUG
//            SCode.location = "-";
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;

//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2SA : PrimitiveCombination2SL
//    {

//        protected PrimitiveCombination2SA (Primitive2 rator, SCode rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
//        {
//            return 
//                (rand1 is Argument0) ? PrimitiveCombination2SA0.Make (rator, rand0, (Argument0) rand1) :
//                (rand1 is Argument1) ? PrimitiveCombination2SA1.Make (rator, rand0, (Argument1) rand1) :
//                new PrimitiveCombination2SA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls(this.rand0);
//            SCode.location = "PrimitiveCombination2SA.EvalStep";
//#endif
//            object ev1 = closureEnvironment.ArgumentValue (this.rand1Offset);

//            Control unev = this.rand0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new PrimitiveCombination2SAFrame0 (this, closureEnvironment, ev1));
//                answer = Interpreter.Unwind;
//                closureEnvironment = env;
//                return false;
//            }
       

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCombination2SAFrame0 : SubproblemContinuation<PrimitiveCombination2SA>, ISystemVector
//    {
//        readonly object ev1;
//        public PrimitiveCombination2SAFrame0 (PrimitiveCombination2SA expression, Environment closureEnvironment, object ev1)
//            : base (expression, closureEnvironment)
//        {
//            this.ev1 = ev1;
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion

//        public override bool Continue (out object answer, ref Control expression, ref Environment closureEnvironment, object ev0)
//        {
//            #if DEBUG
//            Primitive.hotPrimitives.Note (this.expression.Rator);
            
//            SCode.location = this.expression.Rator.Name.ToString();
//#endif
//            if (this.expression.Rator.Method (out answer, ev0, this.ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }

//    }

//    [Serializable]
//    sealed class PrimitiveCombination2SA0 : PrimitiveCombination2SA
//    {

//        PrimitiveCombination2SA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
//        {
//            return new PrimitiveCombination2SA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCombination2SA0.EvalStep");
//            NoteCalls (this.rand0);
//#endif
//            object ev1 = closureEnvironment.Argument0Value;

//            Control unev = this.rand0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCombination2SA1 : PrimitiveCombination2SA
//    {

//        PrimitiveCombination2SA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
//        {
//            return new PrimitiveCombination2SA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCombination2SA1.EvalStep");
//            NoteCalls(this.rand0);
//#endif
//            object ev1 = closureEnvironment.Argument1Value;

//            Control unev = this.rand0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2SL1 : PrimitiveCombination2SL
//    {

//        protected PrimitiveCombination2SL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
//        {
//            return new PrimitiveCombination2SL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls(this.rand0);
//            SCode.location = "PrimitiveCombination2SL1.EvalStep";
//#endif
//            object ev1;
//            if (closureEnvironment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();

//            Control unev = this.rand0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, closureEnvironment));
//                //answer = Interpreter.Unwind;
//                //closureEnvironment = env;
//                //return false;
//            }

//#if DEBUG
//            SCode.location = "-";
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCombination2SQ : PrimitiveCombination2
//    {
//        public readonly object rand1Value;

//        PrimitiveCombination2SQ (Primitive2 rator, SCode rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//              this.rand1Value = rand1.Quoted;
//        }
               
//        static SCode RewriteConditional (Primitive2 rator, Conditional rand0, Quotation rand1)
//        {
//            return Conditional.Make (rand0.Predicate,
//                                     PrimitiveCombination2.Make (rator, rand0.Consequent, rand1),
//                                     PrimitiveCombination2.Make (rator, rand0.Alternative, rand1));
//        }

//        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
//        {
//            return 
//                //(rand0 is Conditional) ? RewriteConditional (rator, (Conditional) rand0, rand1) :
//                new PrimitiveCombination2SQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls(this.rand0);
//            SCode.location = "PrimitiveCombination2SQ.EvalStep";
//#endif
//            Control unev = this.rand0;
//            Environment env = closureEnvironment;
//            object ev0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.Unwind) {
//                ((UnwinderState) env).AddFrame (new PrimitiveCombination2SQFrame0 (this, closureEnvironment));
//                answer = Interpreter.Unwind;
//                closureEnvironment = env;
//                return false;
//            }
//#if DEBUG
//            SCode.location = "-";
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, ev0, this.rand1Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCombination2SQFrame0 : SubproblemContinuation<PrimitiveCombination2SQ>, ISystemVector
//    {
//        public PrimitiveCombination2SQFrame0 (PrimitiveCombination2SQ expr, Environment env)
//            : base (expr, env)
//        {
//        }


//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion

//        public override bool Continue (out object answer, ref Control expression, ref Environment closureEnvironment, object value)
//        {
//#if DEBUG
//            SCode.location = "-";
//            Primitive.hotPrimitives.Note (this.expression.Rator);

//            SCode.location = this.expression.Rator.Name.ToString();
//#endif
//            if (this.expression.Rator.Method (out answer, value, this.expression.rand1Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

        
    
//    [Serializable]
//    class PrimitiveCombination2QL : PrimitiveCombination2Q
//    {
//        public readonly object rand1Name;
//        public readonly int rand1Depth;
//        public readonly int rand1Offset;

//        protected PrimitiveCombination2QL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//              this.rand1Name = rand1.Name;
//            this.rand1Depth = rand1.Depth;
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
//        {
//            return 
//                (rand1 is Argument) ? PrimitiveCombination2QA.Make (rator, rand0, (Argument) rand1)
//                : (rand1 is LexicalVariable1) ? PrimitiveCombination2QL1.Make (rator, rand0, (LexicalVariable1) rand1)
//                : new PrimitiveCombination2QL (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCombination2QL.EvalStep");
//#endif

//            object ev1;
//            if (closureEnvironment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
//                throw new NotImplementedException ();
//            object ev0 = this.rand0Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2QA : PrimitiveCombination2QL
//    {

//        protected PrimitiveCombination2QA (Primitive2 rator, Quotation rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
//        {
//            return 
//                (rand1 is Argument0) ? PrimitiveCombination2QA0.Make (rator, rand0, (Argument0) rand1)
//                : (rand1 is Argument1) ? PrimitiveCombination2QA1.Make (rator, rand0, (Argument1) rand1)
//                : new PrimitiveCombination2QA (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCombination2QA.EvalStep");
//#endif
//            object ev1 = closureEnvironment.ArgumentValue (this.rand1Offset);
//            object ev0 = this.rand0Value;

//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2QA0 : PrimitiveCombination2QA
//    {

//        protected PrimitiveCombination2QA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
//        {
//            return new PrimitiveCombination2QA0 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, this.rand0Value, closureEnvironment.Argument0Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2QA1 : PrimitiveCombination2QA
//    {

//        protected PrimitiveCombination2QA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
//        {
//            return new PrimitiveCombination2QA1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, this.rand0Value, closureEnvironment.Argument1Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2QL1 : PrimitiveCombination2QL
//    {

//        protected PrimitiveCombination2QL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
//        {
//            return new PrimitiveCombination2QL1 (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("PrimitiveCombination2QL1.EvalStep");
//#endif
//            object ev1;
//            if (closureEnvironment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
//                throw new NotImplementedException ();
//#if DEBUG
//            SCode.location = "-";
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, this.rand0Value, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCombination2QQ : PrimitiveCombination2Q
//    {
//        public readonly object rand1Value;

//        PrimitiveCombination2QQ (Primitive2 rator, Quotation rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//              this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, Quotation rand1)
//        {
//            if (true
//                // These cons, so we can't cache the value.
//                && rator != Primitive.Cons
//                && rator != Primitive.IntegerMultiply // just hard
//                && rator != Primitive.MakeBitString
//                && rator != Primitive.ObjectSetType
//                && rator != Primitive.PrimitiveObjectSetType
//                && rator != Primitive.VectorCons
//                )
//                Debugger.Break ();
//            return new PrimitiveCombination2QQ (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("-");
//            Primitive.hotPrimitives.Note (this.rator);
            
//            SCode.location = this.rator.Name.ToString();
//#endif
//            if (this.method (out answer, this.rand0Value, this.rand1Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    closureEnvironment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }
}
