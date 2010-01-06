using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class Configuration
    {
        #region Access
        /// <summary>
        /// If true, allow Access to be optimized.
        /// </summary>
        public const bool EnableAccessOptimization = true;
        public const bool EnableAccessSpecialization = true;
        #endregion

        #region Assignment
        public const bool EnableAssignmentOptimization = true;
        #endregion

        #region Combination
        public const bool EnableCombinationOptimization = true;
        public const bool EnableCombination0 = true;
        public const bool EnableCombination3 = false;
        #endregion

        #region Combination1
        public const bool EnableCombination1Optimization = true;
        public const bool EnableCombination1Specialization = true;
        /// <summary>
        /// Special case combination1.
        /// </summary>
        public const bool EnableLet1 = true;
        #endregion

        #region Combination2
        public const bool EnableCombination2Optimization = true;
        public const bool EnableCombination2Specialization = true;
        /// <summary>
        /// Special case combination1.
        /// </summary>
        public const bool EnableLet2 = false;
        #endregion

        public const bool EnableCommentOptimization = false;

        #region Conditional
        public const bool EnableConditionalOptimization = true;
        public const bool EnableConditionalSpecialization = true;
        /// <summary>
        /// If true, certain primitive predicates are handled directly
        /// by the conditional.
        /// </summary>
        public const bool EnablePrimitiveConditional1 = true;
        public const bool EnableInlinePCond1 = true;

        public const bool EnablePrimitiveConditional2 = false;
        #endregion

        public const bool EnableDefinitionOptimization = false;
        public const bool EnableDelayOptimization = false;

        #region Disjunction
        public const bool EnableDisjunctionOptimization = false;
        public const bool EnablePrimitiveDisjunction1 = false;
        public const bool EnablePrimitiveDisjunction2 = false;
        public const bool EnableDisjunctionSpecialization = true;
        #endregion

        #region Lambda
        /// <summary>
        /// If true, allow analysis of Lambdas.
        /// </summary>
        public const bool EnableLambdaOptimization = true;
        public const bool EnableStaticLambda = true;
        public const bool EnableSimpleLambda = true;
        #endregion

        public const bool EnablePrimitiveCombination0Optimization = false;

        #region PrimitiveCombination1
        /// <summary>
        /// If true, allow analysis of PrimitiveCombination1.
        /// </summary>
        public const bool EnablePrimitiveCombination1Optimization = true;
        /// <summary>
        /// Allow PrimitiveCombination1 to specialize on the type of argument.
        /// </summary>
        public const bool EnablePrimitive1Specialization = true;
        /// <summary>
        /// Allow selected PrimitiveCombination1 to directly execute on
        /// the arguments.
        /// </summary>
        public const bool EnableInlinePrimitive1 = true;
        #endregion

        #region PrimitiveCombination2
        /// <summary>
        /// If true, allow analysis of PrimitiveCombination2.
        /// </summary>
        public const bool EnablePrimitiveCombination2Optimization = true;
        public const bool EnablePrimitive2Specialization = true;
        public const bool EnableInlinePrimitive2 = true;
        #endregion

        #region PrimitiveCombination3
        public const bool EnablePrimitiveCombination3Optimization = true;
        public const bool EnablePrimitive3Specialization = true;
        public const bool EnableInlinePrimitive3 = false;
        #endregion

        public const bool EnableQuotationOptimization = false;

        #region Sequence2
        public const bool EnableSequence2Optimization = true;
        public const bool EnableSequence2Specialization = true;
        #endregion

        #region Sequence3
        public const bool EnableSequence3Optimization = true;
        public const bool EnableSequence3Specialization = true;
        #endregion

        public const bool EnableTheEnvironmentOptimization = false;

        #region Variable
        /// <summary>
        /// If true, allow the interpreter to figure out where the variable
        /// lives prior to interpretation.
        /// </summary>
        public const bool EnableVariableOptimization = true;
        public const bool EnableArgumentBinding = true;
        public const bool EnableArgument0And1 = true;
        public const bool EnableStaticBinding = true;
        #endregion

        /// <summary>
        /// If true, calls to OBJECT-TYPE? with a constant fixnum are rewritten
        /// as the appropriate calls to the one-argument primitives.
        /// </summary>
        public const bool EnableObjectTypePrimitives = true;

        public const bool EnableCombination3Specialization = false;

        // When closure call count reaches this, walk the code
        // and substitute the static variables.  Set to -1 to disable
        // optimization.
        public const int OptimizeThreshold = 8;


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
