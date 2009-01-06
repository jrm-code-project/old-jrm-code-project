using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Configuration
    {
        /// <summary>
        /// If true, allow the interpreter to figure out where the variable
        /// lives prior to interpretation.
        /// </summary>
        public const bool EnableVariableBinding = true;
        public const bool EnableArgumentBinding = true;
        public const bool EnableGlobalBinding = true;
        public const bool EnableLexicalAddressing = true;
        public const bool EnableLexical1 = true;
        public const bool EnableSimpleLambda = true;
        public const bool EnableStaticBinding = true;

        /// <summary>
        /// Cache the parent frames in a vector in the lexical frame.
        /// </summary>
        public const bool EnableLexicalCache = true;

        public const bool EnableSuperOperators = true;

        public const bool EnableCombination1Specialization = true;
        public const bool EnableCombination2Specialization = true;
        public const bool EnableCombination3 = true;
        public const bool EnableCombination3Specialization = false;
        public const bool EnableConditionalSpecialization = true;
        public const bool EnablePrimitive1Specialization = true;
        public const bool EnablePrimitive2Specialization = true;
        public const bool EnablePrimitiveConditional1 = true;
        public const bool EnablePrimitiveConditional2 = true;
        public const bool EnableSequenceSpecialization = true;

        public const bool EnableInlinePrimitive1 = true;
        public const bool EnableInlinePrimitive2 = true;

        /// <summary>
        /// Special case combination1.
        /// </summary>
        public const bool EnableLet1 = true;

        /// <summary>
        /// If true, allow interpreter to rewrite code to improve performance.
        /// </summary>
        public const bool EnableCodeRewriting = true;

        /// <summary>
        /// If true, allow interpreter to flatten sequences by rewriting.
        /// </summary>
        public const bool EnableFlattenSequence = true;

        /// <summary>
        /// If true, allow interpreter to elide non-side-effects from sequences.
        /// </summary>
        public const bool EnableSequenceSimplification = true;

        /// <summary>
        /// If true, sequences that begin with conditionals may be
        /// distributed to inside the conditional.
        /// </summary>
        public const bool EnableSequenceConditionalSwap = true;

        /// <summary>
        /// Enable replacement of chains of car/cdr with call to GeneralCarCdr.
        /// </summary>
        public const bool EnableFoldCarCdr = false;

        /// <summary>
        /// Enable conversion of (if (not ...)  to if.
        /// </summary>
        public const bool EnableInvertConditional = true;

        /// <summary>
        /// Enable conversion of conditionals to disjunctions if predicate
        /// and consequent are the same.
        /// </summary>
        public const bool EnableDisjunctionConversion = true;

        /// <summary>
        /// Enable conditional folding if predicate is compile-time constant.
        /// </summary>
        public const bool EnableFoldConditional = true;

        /// <summary>
        /// Enable conditional distribution if predicate is conditional.
        /// </summary>
        public const bool EnableConditionalDistribution = true;

        /// <summary>
        /// Enable conversion of conditional to sequence2 if both branches
        /// produce the same constant answer.
        /// </summary>
        public const bool EnableMergeConditionalResult = true;

        // If true, literal unspecific values may be replaced
        // by arbitrary convenient values.
        public const bool EnableTrueUnspecific = false;
    }
}
