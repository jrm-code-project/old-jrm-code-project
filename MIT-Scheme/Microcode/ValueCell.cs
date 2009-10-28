using System;
using System.Diagnostics;

namespace Microcode
{
    [Serializable]
    public sealed class ValueCell
    {
        //static object unassigned = Constant.Unassigned;
#if DEBUG
        object name;
#endif
        bool trapOnReference;
        object val;

        int readCount;
        int writeCount;

        ValueCell ()
        {
            this.trapOnReference = true;
            this.val = ReferenceTrap.Unassigned;
        }

        static bool ValidValue (object value)
        {
            if (value == Interpreter.UnwindStack)
                throw new NotImplementedException ();
            return (value == null)
                || (value is char)
                || (value is char [])
                || (value is double)
                || (value is double [])
                || (value is int)
                || (value is long)
                || (value is object [])
                || (value is string)
                || (value is Access)
                || (value is Assignment)
                || (value is Bignum)
                || (value is Boolean)
                || (value is SimpleClosure)     
                || (value is StandardClosure)
                || (value is StaticClosure)
                || (value is Combination)
                || (value is Combination0)
                || (value is Combination1)
                || (value is Combination2)
                || (value is Comment)
                || (value is Complex)
                || (value is Conditional)
                || (value is Cons)
                || (value is Constant)
                || (value is ControlPoint)
                || (value is Definition)
                || (value is Delay)
                || (value is Disjunction)
                || (value is Entity)
                || (value is StandardExtendedClosure)
                || (value is ExtendedLambda)
                || (value is Hunk3)
                || (value is Lambda)
                || (value is NonMarkedVector)
                || (value is NullEnvironment)
                || (value is Primitive)
                || (value is PrimitiveCombination0)
                || (value is PrimitiveCombination1)
                || (value is PrimitiveCombination2)
                || (value is PrimitiveCombination3)
                || (value is Promise)
                || (value is Quotation)
                || (value is Ratnum)
                || (value is Record)
                || (value is ReferenceTrap)
                || (value is ReturnAddress)
                || (value is ReturnCode)
                || (value is Sequence2)
                || (value is Sequence3)
                //|| (value is SimpleClosure)
                || (value is SimpleEnvironment)
                //|| (value is SimpleLambda)
                || (value is StandardEnvironment<StandardLambda, StandardClosure>)
                || (value is StaticEnvironment)
                || (value is TheEnvironment)
                || (value is Symbol)
                || (value is UnmarkedHistory)
                || (value is Variable)
                || (value is WeakCons)
                ;
        }

        public ValueCell (object name, object initialValue)
        {
#if DEBUG   
            this.name = name;
            if (!ValidValue(initialValue)) throw new ArgumentException ("Illegal value cell contents.", "initialValue");
#endif
            this.val = (initialValue == Constant.ExternalUnassigned)
                        ? ReferenceTrap.Unassigned
                        : initialValue; 
            this.trapOnReference = this.val is ReferenceTrap;
        }

#if DEBUG
        public override String ToString ()
        {
            return "{ValueCell for " + this.name + "}";
        }
#endif

        public bool Unreferenceable ()
        {
            return this.trapOnReference;
            // throw new NotImplementedException ();
            //return val is ReferenceTrap || this.trapOnReference;
        }

        public bool Unbound ()
        {
            if (!this.trapOnReference)
                return false;
            throw new NotImplementedException ();
        }

        public bool Assign (out object oldValue, object newValue)
        {
            this.writeCount += 1;
#if DEBUG
            if (!ValidValue(newValue)) throw new ArgumentException ("Illegal value cell contents.", "newValue");
#endif
            oldValue = (this.trapOnReference && this.val == ReferenceTrap.Unassigned)
                ? Constant.ExternalUnassigned
                : this.val;

            this.val = (newValue == Constant.ExternalUnassigned)
                ? ReferenceTrap.Unassigned
                : newValue;

            this.trapOnReference = this.val is ReferenceTrap;
            return false;
        }

        internal bool GetValue (out object value)
        {
            this.readCount += 1;
            //if (this.readCount == 1000)
            //    Debugger.Break ();
            value = this.val;
            return this.trapOnReference;
        }

        internal bool SafeGetValue (out object value)
        {
            if (this.trapOnReference) {
                switch (ReferenceTrap.GetTrapKind (this.val)) {
                    case TrapKind.TRAP_MACRO:
                        value = this.val;
                        break;
                    default:
                        throw new NotImplementedException ();
                }
                return false;
            }
            else {
                value = this.val;
                return false;
            }
        }

        internal bool GetType (out object value)
        {
            if (this.trapOnReference) {
                switch (ReferenceTrap.GetTrapKind (this.val)) {
                    case TrapKind.TRAP_UNBOUND:
                        value = 0;
                        break;

                    case TrapKind.TRAP_UNASSIGNED:
                        value = 1;
                        break;

                    case TrapKind.TRAP_MACRO:
                        value = 3;
                        break;

                    default:
                        throw new NotImplementedException ();
                }
            }
            else
                value = 2; // NORMAL
            return false;
        }
    }
}
