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
                (rator == Primitive.IsEq) ? PrimitiveIsEq.Make (rator, rand0, rand1) :
                //(rator == Primitive.ObjectIsEq) ? PrimitiveIsObjectEq.Make (rator, rand0, rand1) :
                //(rator == Primitive.CharIsEq) ? PrimitiveIsCharEq.Make (rator, rand0, rand1) :
                //(rator == Primitive.IntIsEq) ? PrimitiveIsIntEq.Make (rator, rand0, rand1) :
                //(rator == Primitive.IsFixnumEqual) ? PrimitiveIsFixnumEqual.Make (rator, rand0, rand1) :
                //(rator == Primitive.GeneralCarCdr) ? PrimitiveGeneralCarCdr.Make (rator, rand0, rand1) :
                //(rator == Primitive.GreaterThanFixnum) ? PrimitiveGreaterThanFixnum.Make (rator, rand0, rand1) :
               // (rator == Primitive.GenericIsLessThan) ? PrimitiveLessThan.Make (rator, rand0, rand1) :
                //(rator == Primitive.LessThanFixnum) ? PrimitiveLessThanFixnum.Make (rator, rand0, rand1) :
                //(rator == Primitive.PlusFixnum) ? PrimitivePlusFixnum.Make (rator, rand0, rand1) :
                (rator == Primitive.IsObjectType) ? PrimitiveIsObjectType.Make (rator, rand0, rand1) :
                (rator == Primitive.RecordRef) ? PrimitiveRecordRef.Make (rator, rand0, rand1) :
                (rator == Primitive.Vector8BRef) ? PrimitiveVector8BRef.Make (rator, rand0, rand1) :
                //(rator == Primitive.VectorRef) ? PrimitiveVectorRef.Make (rator, rand0, rand1) :
                 SpecializedMake (rator, rand0, rand1);
        }

        static SCode SpecializedMake (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCar) ? PrimitiveCombination2Car.Make (rator, (PrimitiveCar) rand0, rand1) :
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
            return 
                Configuration.EnableInlinePrimitive2 ? InlineMake(rator, rand0, rand1) :
                Configuration.EnablePrimitive2Specialization ? SpecializedMake (rator, rand0, rand1) :
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
            SCode.location = "PrimitiveCombination2";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2";
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
            SCode.location = "PrimitiveCombination2";
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
    class PrimitiveCombination2Car : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        public readonly Type rand0InnerType;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand0inner;

        protected PrimitiveCombination2Car (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0inner = rand0.Operand;
#if DEBUG
            this.rand0InnerType = rand0.Operand.GetType ();
#endif
        }

        public static SCode Make (Primitive2 rator, PrimitiveCar rand0, SCode rand1)
        {
            return 
                (rand0 is PrimitiveCarA) ? PrimitiveCombination2CarA.Make (rator, (PrimitiveCarA) rand0, rand1) :
                (rand0 is PrimitiveCarS) ? PrimitiveCombination2CarS.Make (rator, (PrimitiveCarS) rand0, rand1) :
                (rand1 is Quotation) ? new PrimitiveCombination2CarXQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new PrimitiveCombination2CarXS (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveCombination2Car (rator, rand0, rand1);
        }
            
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0inner);
            NoteCalls (this.rand1);

            rand0TypeHistogram.Note (this.rand0InnerType);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2Car";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2Car";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            unev = this.rand0inner;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2Car";
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
            Cons ev0pair = ev0 as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
    class PrimitiveCombination2CarA : PrimitiveCombination2Car
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected PrimitiveCombination2CarA (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Offset = rand0.offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveCarA0) ? PrimitiveCombination2CarA0.Make (rator, (PrimitiveCarA0) rand0, rand1) :
                 (rand1 is Argument) ? PrimitiveCombination2CarAA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new PrimitiveCombination2CarAQ (rator, rand0, (Quotation) rand1):
                (rand1 is StaticVariable) ? new PrimitiveCombination2CarAS (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveCombination2CarA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2CarA";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2CarA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }


#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = environment.ArgumentValue(this.rand0Offset) as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
    class PrimitiveCombination2CarA0 : PrimitiveCombination2CarA
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveCombination2CarA0 (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveCombination2CarA0A.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new PrimitiveCombination2CarA0Q (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new PrimitiveCombination2CarA0S (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveCombination2CarA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2CarA0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2CarA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }


#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = environment.Argument0Value as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
    class PrimitiveCombination2CarA0A : PrimitiveCombination2CarA0
    {
        public readonly int rand1Offset;
        protected PrimitiveCombination2CarA0A (Primitive2 rator, PrimitiveCarA0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarA0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? new PrimitiveCombination2CarA0A0 (rator, rand0, (Argument0) rand1) :
                new PrimitiveCombination2CarA0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2CarA0A");
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = environment.Argument0Value as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
    sealed class PrimitiveCombination2CarA0A0 : PrimitiveCombination2CarA0A
    {
        internal PrimitiveCombination2CarA0A0 (Primitive2 rator, PrimitiveCarA0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2CarA0A0");
#endif
            object ev1 = environment.Argument0Value;
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = ev1 as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
            public readonly object rand1Value;
        internal PrimitiveCombination2CarA0Q (Primitive2 rator, PrimitiveCarA0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2CarA0Q");
#endif
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = environment.Argument0Value as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, this.rand1Value)) {
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
    sealed class PrimitiveCombination2CarA0S : PrimitiveCombination2CarA0
    {
            public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        internal PrimitiveCombination2CarA0S (Primitive2 rator, PrimitiveCarA0 rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2CarA0S");
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException();
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = environment.Argument0Value as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
            class PrimitiveCombination2CarAA : PrimitiveCombination2CarA
            {

                public readonly int rand1Offset;
                protected PrimitiveCombination2CarAA (Primitive2 rator, PrimitiveCarA rand0, Argument rand1)
                    : base (rator, rand0, rand1)
                {
                    this.rand1Offset = rand1.Offset;
                }

                public static SCode Make (Primitive2 rator, PrimitiveCarA rand0, Argument rand1)
                {
                    return
                        (rand1 is Argument0) ? new PrimitiveCombination2CarAA0 (rator, rand0, (Argument0) rand1) :
                        new PrimitiveCombination2CarAA (rator, rand0, rand1);
                }

                public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
                {
#if DEBUG
                    Warm ("-");
                    SCode.location = "PrimitiveCombination2CarAA";
#endif



#if DEBUG
                    SCode.location = this.rator.Name.ToString ();
                    Primitive.hotPrimitives.Note (this.rator);
#endif
                    Cons ev0pair = environment.ArgumentValue(this.rand0Offset) as Cons;
                    if (ev0pair == null) throw new NotImplementedException ();

                    // It is expensive to bounce down to invoke the procedure
                    // we invoke it directly and pass along the ref args.
                    if (this.method (out answer, ev0pair.Car, environment.ArgumentValue (this.rand1Offset))) {
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
            sealed class PrimitiveCombination2CarAA0 : PrimitiveCombination2CarAA
            {
                internal PrimitiveCombination2CarAA0 (Primitive2 rator, PrimitiveCarA rand0, Argument0 rand1)
                    : base (rator, rand0, rand1)
                {
                }

                public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
                {
#if DEBUG
                    Warm ("-");
                    SCode.location = "PrimitiveCombination2CarAA0";
#endif



#if DEBUG
                    SCode.location = this.rator.Name.ToString ();
                    Primitive.hotPrimitives.Note (this.rator);
#endif
                    Cons ev0pair = environment.ArgumentValue(this.rand0Offset) as Cons;
                    if (ev0pair == null) throw new NotImplementedException ();

                    // It is expensive to bounce down to invoke the procedure
                    // we invoke it directly and pass along the ref args.
                    if (this.method (out answer, ev0pair.Car, environment.Argument0Value)) {
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
            sealed class PrimitiveCombination2CarAQ : PrimitiveCombination2CarA
            {
                public readonly object rand1Value;
                internal PrimitiveCombination2CarAQ (Primitive2 rator, PrimitiveCarA rand0, Quotation rand1)
                    : base (rator, rand0, rand1)
                {
                    this.rand1Value = rand1.Quoted;
                }

                public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
                {
#if DEBUG
                    Warm ("PrimitiveCombination2CarAQ");
#endif
#if DEBUG
                    SCode.location = this.rator.Name.ToString ();
                    Primitive.hotPrimitives.Note (this.rator);
#endif
                    Cons ev0pair = environment.ArgumentValue(this.rand0Offset) as Cons;
                    if (ev0pair == null) throw new NotImplementedException ();

                    // It is expensive to bounce down to invoke the procedure
                    // we invoke it directly and pass along the ref args.
                    if (this.method (out answer, ev0pair.Car, this.rand1Value)) {
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
            sealed class PrimitiveCombination2CarAS : PrimitiveCombination2CarA
            {
                public readonly Symbol rand1Name;
                public readonly int rand1Offset;
                internal PrimitiveCombination2CarAS (Primitive2 rator, PrimitiveCarA rand0, StaticVariable rand1)
                    : base (rator, rand0, rand1)
                {
                    this.rand1Name = rand1.Name;
                    this.rand1Offset = rand1.Offset;
                }

                public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
                {
#if DEBUG
                    Warm ("PrimitiveCombination2CarAS");
#endif
                    object ev1;
                    if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                        throw new NotImplementedException ();
#if DEBUG
                    SCode.location = this.rator.Name.ToString ();
                    Primitive.hotPrimitives.Note (this.rator);
#endif
                    Cons ev0pair = environment.ArgumentValue(this.rand0Offset) as Cons;
                    if (ev0pair == null) throw new NotImplementedException ();

                    // It is expensive to bounce down to invoke the procedure
                    // we invoke it directly and pass along the ref args.
                    if (this.method (out answer, ev0pair.Car, ev1)) {
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
    class PrimitiveCombination2CarS : PrimitiveCombination2Car
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PrimitiveCombination2CarS (Primitive2 rator, PrimitiveCarS rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Name = rand0.name;
            this.rand0Offset = rand0.offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarS rand0, SCode rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveCombination2CarSA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is Quotation) ? new PrimitiveCombination2CarSQ (rator, rand0, (Quotation) rand1) :
                (rand1 is StaticVariable) ? new PrimitiveCombination2CarSS (rator, rand0, (StaticVariable) rand1) :
                new PrimitiveCombination2CarS (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination2CarS";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2CarS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = ev0 as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
    class PrimitiveCombination2CarSA : PrimitiveCombination2CarS
    {
        public readonly int rand1Offset;

        protected PrimitiveCombination2CarSA (Primitive2 rator, PrimitiveCarS rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, PrimitiveCarS rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? new PrimitiveCombination2CarSA0 (rator, rand0, (Argument0) rand1) :
                new PrimitiveCombination2CarSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2CarSA");
#endif

            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = ev0 as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
    sealed class PrimitiveCombination2CarSA0 : PrimitiveCombination2CarSA
    {
        internal PrimitiveCombination2CarSA0 (Primitive2 rator, PrimitiveCarS rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2CarSA0");
#endif

            object ev1 = environment.Argument0Value;
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = ev0 as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
    sealed class PrimitiveCombination2CarSQ : PrimitiveCombination2CarS
    {
        public readonly object rand1Value;

        internal PrimitiveCombination2CarSQ (Primitive2 rator, PrimitiveCarS rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2CarSQ");
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = ev0 as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, this.rand1Value)) {
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
    sealed class PrimitiveCombination2CarSS : PrimitiveCombination2CarS
    {
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        internal PrimitiveCombination2CarSS (Primitive2 rator, PrimitiveCarS rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination2CarSS");
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = ev0 as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
    sealed class PrimitiveCombination2CarXQ : PrimitiveCombination2Car
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        internal PrimitiveCombination2CarXQ (Primitive2 rator, PrimitiveCar rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0inner);

            rand0TypeHistogram.Note (this.rand0InnerType);
            SCode.location = "PrimitiveCombination2CarXQ";
#endif
            Control unev = this.rand0inner;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2CarXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, this.rand1Value));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
#if DEBUG
            SCode.location = this.rator.Name.ToString ();
            Primitive.hotPrimitives.Note (this.rator);
#endif
            Cons ev0pair = ev0 as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, this.rand1Value)) {
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
    sealed class PrimitiveCombination2CarXS : PrimitiveCombination2Car
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
            public readonly int rand1Offset;

        internal PrimitiveCombination2CarXS (Primitive2 rator, PrimitiveCar rand0, StaticVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Offset = rand1.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0inner);

            rand0TypeHistogram.Note (this.rand0InnerType);
            SCode.location = "PrimitiveCombination2CarXS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException();

            Control unev = this.rand0inner;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2CarXS";
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
            Cons ev0pair = ev0 as Cons;
            if (ev0pair == null) throw new NotImplementedException ();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0pair.Car, ev1)) {
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
        public readonly int rand0Offset;

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
            SCode.location = "PrimitiveCombination2A";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2A";
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
            SCode.location = "PrimitiveCombination2A0";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2A0";
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
               new PrimitiveCombination2A0A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2A0A";
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
            SCode.location = "PrimitiveCombination2A0A0";
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
            SCode.location = "PrimitiveCombination2A0S";
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
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
#endif        
        public readonly object rand1Value;

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
            SCode.location = "PrimitiveCombination2A0Q";
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
               new PrimitiveCombination2AA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2AA";
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
            SCode.location = "PrimitiveCombination2AA0";
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
            SCode.location = "PrimitiveCombination2AS";
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
            SCode.location = "PrimitiveCombination2AQ";
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
            SCode.location = "PrimitiveCombination2S";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2S";
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
                new PrimitiveCombination2SA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2SA";
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
            SCode.location = "PrimitiveCombination2SA0";
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
            SCode.location = "PrimitiveCombination2SS";
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
            SCode.location = "PrimitiveCombination2SQ";
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
        public readonly object rand0Value;
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
            SCode.location = "PrimitiveCombination2Q";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2Q";
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
               new PrimitiveCombination2QA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2QA";
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
            SCode.location = "PrimitiveCombination2QA0";
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
            SCode.location = "PrimitiveCombination2QS";
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
            SCode.location = "PrimitiveCombination2QQ";
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
                new PrimitiveCombination2XA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            ratorHistogram.Note (this.rator);
            SCode.location = "PrimitiveCombination2XA";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2XA";
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
            SCode.location = "PrimitiveCombination2XA0";
#endif
            object ev1 = environment.Argument0Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2XA0";
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
            SCode.location = "PrimitiveCombination2XS";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2XS";
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
            SCode.location = "PrimitiveCombination2XQ";
#endif

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination2XQ";
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
}
