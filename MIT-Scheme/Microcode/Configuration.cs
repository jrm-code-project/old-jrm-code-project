using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Configuration
    {
        public const bool EnableArgumentBinding = true;
        public const bool EnableGlobalBinding = true;
        public const bool EnableLexicalAddressing = true;
        public const bool EnableLexical1 = true;
        public const bool EnableSimpleLambda = true;
        public const bool EnableStaticBinding = true;
        public const bool EnableSuperOperators = true;

        /// <summary>
        /// Enable replacement of chains of car/cdr with call to GeneralCarCdr.
        /// </summary>
        public const bool EnableFoldCarCdr = false;

        /// <summary>
        /// Enable conversion of (if (not ...)  to if.
        /// </summary>
        public const bool EnableInvertConditional = true;

        /// <summary>
        /// Enable conversion of conditional to sequence2 if both branches
        /// produce the same constant answer.
        /// </summary>
        public const bool EnableMergeConditionalResult = true;

        // If true, literal unspecific values may be replaced
        // by arbitrary convenient values.
        public const bool EnableTrueUnspecific = true;
    }
}
