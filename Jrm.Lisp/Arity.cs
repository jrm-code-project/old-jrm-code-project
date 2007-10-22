using System;
using System.Collections.Generic;
using System.Reflection;
using System.Linq;
using System.Text;

namespace Lisp
{
    class Arity
    {
        bool isNary;
        int minimumArgumentCount;
        int maximumArgumentCount;

        static Symbol QuoteAndRest = Package.CommonLisp.FindSymbol ("&REST");

        public Arity (Delegate del)
        {
            ParameterInfo [] parameters = del.Method.GetParameters ();
            ParameterInfo lastParameter = parameters [parameters.Length - 1];
            object [] attr = lastParameter.GetCustomAttributes (typeof (System.ParamArrayAttribute), false);
            this.isNary = attr.Length > 0;
            this.minimumArgumentCount = (this.isNary) ? parameters.Length - 1 : parameters.Length;
            this.maximumArgumentCount = (this.isNary) ? -1 : parameters.Length;
        }

        public Arity (Cons lambdaList)
        {
            int count = 0;
            while (lambdaList != null) {
                lambdaList = (Cons) lambdaList.Cdr;
                count += 1;
            }
            this.minimumArgumentCount = count;
            this.maximumArgumentCount = count;
        }

        public int MinimumArgumentCount
        {
            get
            {
                return minimumArgumentCount;
            }
        }

        public bool IsNary
        {
            get
            {
                return isNary;
            }
        }

        public override bool Equals (object obj)
        {
            Arity that = obj as Arity;

            return (that != null)
                && this.minimumArgumentCount == that.minimumArgumentCount
                && this.maximumArgumentCount == that.maximumArgumentCount;
        }

        public override int GetHashCode ()
        {
            return this.minimumArgumentCount;
        }
        
    }
}
