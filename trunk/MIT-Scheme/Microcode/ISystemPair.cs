
using System;
using System.Collections.Generic;
using System.Linq;

namespace Microcode
{
    public interface ISystemPair
    {
        object SystemPairCar
        {
            get;
            set;
        }

        object SystemPairCdr
        {
            get;
            set;
        }
    }

    static class SystemPair
    {
        [SchemePrimitive ("SYSTEM-PAIR-CONS", 3, false)]
        public static bool SystemPairCons (out object answer, object acode, object car, object cdr)
        {
            //TC typeCode = (TC) (int) acode; // for debugging porpoises

            switch ((TC) acode) {
                case TC.ACCESS:
                    answer = Access.Make (car, (Symbol) cdr);
                    break;

                case TC.ASSIGNMENT:
                    answer = Assignment.Make (car, cdr);
                    break;

                case TC.COMBINATION_1:
                    answer = Combination1.Make (car, cdr);
                    break;

                case TC.COMMENT:
                    answer = Comment.Make (car, cdr);
                    break;

                case TC.COMPLEX:
                    answer = new Complex (car, cdr);
                    break;

                case TC.DEFINITION:
                    answer = Definition.Make ((Symbol)car, cdr);
                    break;

                case TC.DELAY:
                    answer = new Delay (car, cdr);
                    break;

                case TC.DISJUNCTION:
                    answer = Disjunction.Make (car, cdr);
                    break;

                case TC.ENTITY:
                    answer = new Entity (car, cdr);
                    break;

                case TC.LAMBDA:
                    // passed in backwards.
                    object [] names = (object []) cdr;
                    object [] formals = new object [names.Length - 1];
                    Array.Copy (names, 1, formals, 0, formals.Length);
                    SCode body = SCode.EnsureSCode (car);
                    answer = Lambda.Make (names[0], formals, car);
                    break;

                case TC.PCOMB1:
                    answer = PrimitiveCombination1.Make ((Primitive1) car, cdr);
                    break;

                case TC.PROCEDURE:
                    // Lambda had better be a `StandardLambda' because we are
                    // constructing an closureEnvironment that needs to be first-class.

                    Environment env = Environment.ToEnvironment (cdr);
                    Lambda ulam = (Lambda) car;
                    Lambda plam = (Lambda) ulam.PartialEval (PartialEnvironment.Make((ITopLevelEnvironment) env)).Residual;
                    StandardLambda slam = (StandardLambda) new StandardLambda (plam.Name, 
                        plam.Formals, 
                        plam.Body,
                        ulam.FreeVariables,
                        plam.GetStaticMapping(env)
                        );
                    answer = new StandardClosure (slam, env);
                    break;

                case TC.RATNUM:
                    answer = new Ratnum (car, cdr);
                    break;

                case TC.SCODE_QUOTE:
                    answer = Quotation.Make (car);
                    break;

                case TC.SEQUENCE_2:
                    answer = Sequence2.Make (car, cdr);
                    break;

                case TC.UNINTERNED_SYMBOL:
                    // What gives?  Uninterned strings are squirrely on the CLR.
                    // We put them in a class object to have more control.
                    answer = Symbol.MakeUninterned (new String ((char []) car));
                    break;

                case TC.WEAK_CONS:
                    answer = new WeakCons (car, cdr);
                    break;

                default:
                    throw new NotImplementedException ();
            }
            return false;
        }

        [SchemePrimitive ("SYSTEM-PAIR-CAR", 1, false)]
        public static bool SystemPairCar (out object answer, object arg)
        {
            ISystemPair systemPair = arg as ISystemPair;
            if (systemPair != null)
                answer = systemPair.SystemPairCar;
            else {
                // special case strings to act like symbols
                // with a print ratorName in the system pair car.
                string sarg = arg as string;
                if (sarg == null)
                    throw new NotImplementedException ();
                else
                    answer = sarg.ToCharArray ();
            }
            return false;
        }

        [SchemePrimitive ("SYSTEM-PAIR-CDR", 1, false)]
        public static bool SystemPairCdr (out object answer, object arg)
        {
            ISystemPair systemPair = arg as ISystemPair;
            if (systemPair == null)
                throw new NotImplementedException ();
            else {
                answer = systemPair.SystemPairCdr;
                return false;
            }
        }

        [SchemePrimitive ("SYSTEM-PAIR-SET-CAR!", 2, false)]
        public static bool SystemPairSetCar (out object answer, object arg, object newValue)
        {
            ISystemPair systemPair = arg as ISystemPair;
            answer = systemPair.SystemPairCar;
            systemPair.SystemPairCar = newValue;
            return false;
        }

        [SchemePrimitive ("SYSTEM-PAIR-SET-CDR!", 2, false)]
        public static bool SystemPairSetCdr (out object answer, object arg, object newValue)
        {
            ISystemPair systemPair = arg as ISystemPair;
            answer = systemPair.SystemPairCdr;
            systemPair.SystemPairCdr = newValue;
            return false;
        }
    }
}
