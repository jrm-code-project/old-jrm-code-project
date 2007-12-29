using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;

namespace Lisp
{
    internal class FuncallableWrapper
    {
        Delegate del;

        FuncallableWrapper (Delegate del)
        {
            this.del = del;
        }

        object Funcall1r (StandardInstance self, object [] arguments)
        {
            return del.DynamicInvoke (null, arguments);
        }

        object Funcall2 (StandardInstance self, object [] arguments)
        {
            if (arguments.Length != 1)
                throw new NotImplementedException ("funcall2");
            return del.DynamicInvoke (null, arguments [0]);
        }

        object Funcall2r (StandardInstance self, object [] arguments)
        {
            object [] remaining = new object [arguments.Length - 1];
            Array.Copy (arguments, 1, remaining, 0, arguments.Length - 1);
            return del.DynamicInvoke (null, arguments [0], remaining);
        }

        object Funcall3 (StandardInstance self, object [] arguments)
        {
            if (arguments.Length != 2)
                throw new NotImplementedException ("funcall3");
            return del.DynamicInvoke (null, arguments [0], arguments[1]);
        }

        object Funcall (StandardInstance self, object [] arguments)
        {
            ParameterInfo [] parameters = del.Method.GetParameters ();
            ParameterInfo lastParameter = parameters [parameters.Length - 1];
            object [] attr = lastParameter.GetCustomAttributes (typeof (System.ParamArrayAttribute), false);
            switch (parameters.Length) {
                case 2:
                    if (attr.Length > 0) {
                        // 2 or more arguments
                        return del.DynamicInvoke (null, arguments);
                    }
                    else {
                        // exactly 2 arguments
                        throw new NotImplementedException ();
                    }
                default:
                    throw new NotImplementedException ();
            }
            // return del.DynamicInvoke (null, arguments);
        }

        static MethodInfo GetFuncallMethod (Delegate del)
        {
            ParameterInfo [] parameters = del.Method.GetParameters ();
            ParameterInfo lastParameter = parameters [parameters.Length - 1];
            object [] attr = lastParameter.GetCustomAttributes (typeof (System.ParamArrayAttribute), false);
            string funcall = "Funcall" + (parameters.Length - ((attr.Length > 0) ? 1 : 0)).ToString() + ((attr.Length > 0) ? "r" : "");
            MethodInfo mi = 
            typeof (FuncallableWrapper)
                .GetMethod (funcall, BindingFlags.NonPublic | BindingFlags.Instance);
            if (mi == null)
                throw new NotImplementedException (funcall);
            return mi;
        }

        public static FuncallHandler Create (Delegate del)
        {
            return (FuncallHandler)
                Delegate.CreateDelegate (typeof (FuncallHandler),
                                            new FuncallableWrapper (del),
                                            GetFuncallMethod(del)
                                            );
        }
    }
}

