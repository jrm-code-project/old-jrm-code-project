using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Configuration
    {
        /// <summary>
        /// If true, allow Access to be optimized.
        /// </summary>
        public const bool EnableAccessOptimization = false;
        public const bool EnableAssignmentOptimization = false;
        public const bool EnableCombinationOptimization = false;
        public const bool EnableCombination0Optimization = false;
        public const bool EnableCombination1Optimization = false;
        public const bool EnableCombination2Optimization = false;
        public const bool EnableCommentOptimization = false;
        public const bool EnableConditionalOptimization = false;
        public const bool EnableDefinitionOptimization = false;
        public const bool EnableDelayOptimization = false;
        public const bool EnableDisjunctionOptimization = false;
        /// <summary>
        /// If true, allow analysis of Lambdas.
        /// No effect, currently.
        /// </summary>
        public const bool EnableLambdaOptimization = false;
        public const bool EnablePrimitiveCombination0Optimization = false;
        /// <summary>
        /// If true, allow analysis of PrimitiveCombination1.
        /// </summary>
        public const bool EnablePrimitiveCombination1Optimization = false;
        /// <summary>
        /// If true, allow analysis of PrimitiveCombination2.
        /// </summary>
        public const bool EnablePrimitiveCombination2Optimization = false;
        public const bool EnablePrimitiveCombination3Optimization = false;
        public const bool EnableQuotationOptimization = false;
        public const bool EnableSequence2Optimization = false;
        public const bool EnableSequence3Optimization = false;
        public const bool EnableTheEnvironmentOptimization = false;
        /// <summary>
        /// If true, allow the interpreter to figure out where the variable
        /// lives prior to interpretation.
        /// </summary>
        public const bool EnableVariableOptimization = false;

        public const bool EnableArgumentBinding = true;
        public const bool EnableGlobalBinding = true;
        public const bool EnableSimpleLambda = true;
        public const bool EnableStaticBinding = true;

        /// <summary>
        /// Cache the parent frames in a vector in the lexical frame.
        /// </summary>
        public const bool EnableLexicalCache = false;

        public const bool EnableSuperOperators = false;

        public const bool EnableCombination1Specialization = false;
        public const bool EnableCombination2Specialization = false;
        public const bool EnableCombination3 = false;
        public const bool EnableCombination3Specialization = false;
        public const bool EnableConditionalSpecialization = false;
        public const bool EnableDisjunctionSpecialization = false;
        /// <summary>
        /// Allow Primitive1Combinations to specialize on the type of argument.
        /// </summary>
        public const bool EnablePrimitive1Specialization = true;
        public const bool EnablePrimitive2Specialization = true;
        /// <summary>
        /// If true, certain primitive predicates are handled directly
        /// by the conditional.
        /// </summary>
        public const bool EnablePrimitiveConditional1 = false;
        public const bool EnablePrimitiveConditional2 = false;
        public const bool EnableSequenceSpecialization = false;

        public const bool EnableInlinePrimitive1 = true;
        public const bool EnableInlinePrimitive2 = false;

        /// <summary>
        /// Special case combination1.
        /// </summary>
        public const bool EnableLet1 = false;

        /// <summary>
        /// If true, allow interpreter to rewrite code to improve performance.
        /// </summary>
        public const bool EnableCodeRewriting = false;

        /// <summary>
        /// If true, allow interpreter to flatten sequences by rewriting.
        /// </summary>
        public const bool EnableFlattenSequence = false;

        /// <summary>
        /// If true, allow interpreter to elide non-side-effects from sequences.
        /// </summary>
        public const bool EnableSequenceSimplification = false;

        /// <summary>
        /// If true, sequences that begin with conditionals may be
        /// distributed to inside the conditional.
        /// </summary>
        public const bool EnableSequenceConditionalSwap = false;

        /// <summary>
        /// Enable replacement of chains of car/cdr with call to GeneralCarCdr.
        /// </summary>
        public const bool EnableFoldCarCdr = false;

        /// <summary>
        /// Enable conversion of (if (not ...)  to if.
        /// </summary>
        public const bool EnableInvertConditional = false;

        /// <summary>
        /// Enable conversion of conditionals to disjunctions if predicate
        /// and consequent are the same.
        /// </summary>
        public const bool EnableDisjunctionConversion = false;

        /// <summary>
        /// Enable conditional folding if predicate is compile-time constant.
        /// </summary>
        public const bool EnableFoldConditional = false;

        /// <summary>
        /// Enable conditional distribution if predicate is conditional.
        /// </summary>
        public const bool EnableConditionalDistribution = false;

        /// <summary>
        /// Enable conversion of conditional to sequence2 if both branches
        /// produce the same constant answer.
        /// </summary>
        public const bool EnableMergeConditionalResult = false;

        // If true, literal unspecific values may be replaced
        // by arbitrary convenient values.
        public const bool EnableTrueUnspecific = false;
    }
}
