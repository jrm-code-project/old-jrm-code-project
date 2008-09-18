using System;
using System.Diagnostics;

namespace Microcode
{
    public class FixedObjectsVector
    {
        const int SystemInterruptVector = 0x01;
        const int SystemCallNames = 0x09;
        const int SystemErrorNames = 0x0A;
        const int DummyHistory = 0x13;
        const int TerminationVector = 0x16;
        const int TerminationProcVector  = 0x17;
        const int MeMyself = 0x18;
        const int GenericTrampolineZeroP = 0x24;
        const int GenericTrampolinePositiveP = 0x25;
        const int GenericTrampolineNegativeP = 0x26;
        const int GenericTrampolineSuccessor = 0x27;
        const int GenericTrampolinePredecessor = 0x28;
        const int GenericTrampolineEqualP = 0x29;
        const int GenericTrampolineLessP = 0x2a;
        const int GenericTrampolineGreaterP = 0x2b;
        const int GenericTrampolineAdd = 0x2c;
        const int GenericTrampolineSubtract = 0x2d;
        const int GenericTrampolineMultiply = 0x2e;
        const int GenericTrampolineDivide = 0x2f;
        const int GenericTrampolineQuotient = 0x30;
        const int GenericTrampolineRemainder = 0x31;
        const int GenericTrampolineModulo = 0x32;
        const int nFixedObjects = 0x45;

        static object [] theFixedObjectsVector;

        public static void Initialize ()
        {
            theFixedObjectsVector = new object [nFixedObjects];
            theFixedObjectsVector [DummyHistory] = History.DummyHistory ();
            theFixedObjectsVector [TerminationVector] = new object [0];
            theFixedObjectsVector [TerminationProcVector] = new object [0];
            theFixedObjectsVector [MeMyself] = theFixedObjectsVector;
            theFixedObjectsVector [SystemCallNames] = new object [0];
            theFixedObjectsVector [SystemErrorNames] = new object [0];
            theFixedObjectsVector [SystemInterruptVector] = new object [15];
            theFixedObjectsVector [GenericTrampolineZeroP] = Primitive.Find ("INTEGER-ZERO?", 1);
            theFixedObjectsVector [GenericTrampolinePositiveP] = Primitive.Find ("INTEGER-POSITIVE?", 1);
            theFixedObjectsVector [GenericTrampolineNegativeP] = Primitive.Find ("INTEGER-NEGATIVE?", 1);
            theFixedObjectsVector [GenericTrampolineSuccessor] = Primitive.Find ("INTEGER-ADD-1", 1);
            theFixedObjectsVector [GenericTrampolinePredecessor] = Primitive.Find ("INTEGER-SUBTRACT-1", 1);
            theFixedObjectsVector [GenericTrampolineEqualP] = Primitive.Find ("INTEGER-EQUAL?", 2);
            theFixedObjectsVector [GenericTrampolineLessP] = Primitive.Find ("INTEGER-LESS?", 2);
            theFixedObjectsVector [GenericTrampolineGreaterP] = Primitive.Find ("INTEGER-GREATER?", 2);
            theFixedObjectsVector [GenericTrampolineAdd] = Primitive.Find ("INTEGER-ADD", 2);
            theFixedObjectsVector [GenericTrampolineSubtract] = Primitive.Find ("INTEGER-SUBTRACT", 2);
            theFixedObjectsVector [GenericTrampolineMultiply] = Primitive.Find ("INTEGER-MULTIPLY", 2);
            theFixedObjectsVector [GenericTrampolineDivide] = false;
            theFixedObjectsVector [GenericTrampolineQuotient] = false;
            theFixedObjectsVector [GenericTrampolineRemainder] = false;
            theFixedObjectsVector [GenericTrampolineModulo] = false;
        }

        internal static History TheDummyHistory
        {
            [DebuggerStepThrough]
            get
            {
                return (History) (theFixedObjectsVector [DummyHistory]);
            }
        }

        public static object SyscallNames
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [SystemCallNames];
            }
        }

        public static object SyserrNames
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [SystemErrorNames];
            }
        }

        public static object GenericZeroP
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineZeroP];
            }
        }

        public static object GenericPositiveP
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolinePositiveP];
            }
        }

        public static object GenericNegativeP
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineNegativeP];
            }
        }

        public static object GenericSuccessor
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineSuccessor];
            }
        }

        public static object GenericPredecessor
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolinePredecessor];
            }
        }

        public static object GenericEqualP
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineEqualP];
            }
        }

        public static object GenericLessP
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineLessP];
            }
        }

        public static object GenericGreaterP
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineGreaterP];
            }
        }

        public static object GenericAdd
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineAdd];
            }
        }

        public static object GenericSubtract
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineSubtract];
            }
        }

        public static object GenericMultiply
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineMultiply];
            }
        }

        public static object GenericDivide
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineDivide];
            }
        }

        public static object GenericQuotient
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineQuotient];
            }
        }

        public static object GenericRemainder
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineRemainder];
            }
        }

        public static object GenericModulo
        {
            [DebuggerStepThrough]
            get
            {
                return theFixedObjectsVector [GenericTrampolineModulo];
            }
        }

        [SchemePrimitive ("GET-FIXED-OBJECTS-VECTOR", 0, true)]
        public static bool GetFixedObjectsVector (out object answer)
        {
            answer = theFixedObjectsVector;
            return false;
        }

        [SchemePrimitive ("SET-FIXED-OBJECTS-VECTOR!", 1, false)]
        public static bool SetFixedObjectsVector (out object answer, object arg0)
        {
            answer = theFixedObjectsVector;
            theFixedObjectsVector = (object []) arg0;
            theFixedObjectsVector [MeMyself] = theFixedObjectsVector;
            return false;
        } 
    }
}
