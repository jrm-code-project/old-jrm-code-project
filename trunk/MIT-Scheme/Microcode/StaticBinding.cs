using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    /// <summary>
    /// Represents the formal parameter of a lambda expression.
    /// </summary>
    class StaticBinding
    {
        // Name is an object because it might
        // be a gensym.
        object name;
        LambdaBase lambda; // the lambda expression that binds it

        public StaticBinding (LambdaBase lambda, object name, int offset)
        {
            this.lambda = lambda;
            this.name = name;
        }

        public object Name
        {
            get
            {
                return this.name;
            }
        }
    }
}
