using System;

namespace Microcode
{
    interface ISystemPair
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
        [SchemePrimitive ("SYSTEM-PAIR-CONS", 3)]
        public static bool SystemPairCons (out object answer, object acode, object car, object cdr)
        {
            TC typeCode = (TC) (int) acode; // for debugging porpoises

            switch ((TC) acode) {
                case TC.ACCESS:
                    answer = new Access (car, (string) cdr);
                    break;

                case TC.ASSIGNMENT:
                    answer = new Assignment (car, cdr);
                    break;

                case TC.COMBINATION_1:
                    answer = new Combination1 (car, cdr);
                    break;

                case TC.DEFINITION:
                    answer = new Definition (car, cdr);
                    break;

                case TC.DISJUNCTION:
                    answer = new Disjunction (car, cdr);
                    break;

                case TC.ENTITY:
                    answer = new Entity (car, cdr);
                    break;

                case TC.LAMBDA:
                    // passed in backwards.
                    answer = new Lambda (cdr, car);
                    break;

                case TC.PROCEDURE:
                    answer = new Closure ((Lambda) car, (cdr is bool && (bool) cdr == false) ? Environment.Global : (Environment) cdr);
                    break;

                case TC.RATNUM:
                    answer = new Ratnum (car, cdr);
                    break;

                case TC.SEQUENCE_2:
                    answer = new Sequence2 (car, cdr);
                    break;

                case TC.UNINTERNED_SYMBOL:
                    answer = new String ((char []) car);
                    break;

                case TC.WEAK_CONS:
                    answer = new WeakCons (car, cdr);
                    break;

                default:
                    throw new NotImplementedException ();
            }
            return false;
        }

        [SchemePrimitive ("SYSTEM-PAIR-CAR", 1)]
        public static bool SystemPairCar (out object answer, object arg)
        {
            ISystemPair systemPair = arg as ISystemPair;
            if (systemPair == null) {
                if (arg is string) {
                    // symbol name is stored in the system-pair-car
                    answer = ((string) arg).ToCharArray ();
                    return false;
                }
                else
                    throw new NotImplementedException ();
            }
            answer = systemPair.SystemPairCar;
            return false;
        }

        [SchemePrimitive ("SYSTEM-PAIR-CDR", 1)]
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

        [SchemePrimitive ("SYSTEM-PAIR-SET-CAR!", 2)]
        public static bool SystemPairSetCar (out object answer, object arg, object newValue)
        {
            ISystemPair systemPair = arg as ISystemPair;
            answer = systemPair.SystemPairCar;
            systemPair.SystemPairCar = newValue;
            return false;
        }

        [SchemePrimitive ("SYSTEM-PAIR-SET-CDR!", 2)]
        public static bool SystemPairSetCdr (out object answer, object arg, object newValue)
        {
            ISystemPair systemPair = arg as ISystemPair;
            answer = systemPair.SystemPairCdr;
            systemPair.SystemPairCdr = newValue;
            return false;
        }
    }
}
