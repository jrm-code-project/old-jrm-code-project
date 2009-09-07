using System;
using System.Diagnostics;

namespace Microcode
{
    public static class FixedObjectsVector
    {
        const int SystemInterruptVector = 0x01;
        const int SystemErrorVector = 0x02;
        const int Obarray = 0x03;
        const int TypesVector = 0x04;
        const int ReturnsVector = 0x05;
        const int InterruptMaskVector = 0x06;
        const int ErrorsVector = 0x07;
        const int IdentificationVector = 0x08;
        const int SystemCallNames = 0x09;
        const int SystemErrorNames = 0x0A;
        const int FixedObjectSlots = 0x0F;
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
            theFixedObjectsVector [TypesVector] =
                new object [] {
		                Symbol.Make("false"),  // 0x00
		                Symbol.Make("pair"),  // 0x01
		                Symbol.Make("character"),  // 0x02
		                Symbol.Make("quotation"),  // 0x03
		                Symbol.Make("primitive-combination-2"),  // 0x04
		                Symbol.Make("uninterned-symbol"),  // 0x05
		                Symbol.Make("flonum"),  // 0x06
		                Symbol.Make("combination-1"),  // 0x07
		                Symbol.Make("constant"),  // 0x08
		                Symbol.Make("extended-procedure"),  // 0x09
		                Symbol.Make("vector"),  // 0x0A
		                Symbol.Make("return-code"),  // 0x0B
		                Symbol.Make("combination-2"),  // 0x0C
		                Symbol.Make("manifest-closure"),  // 0x0D
		                Symbol.Make("bignum"),  // 0x0E
		                Symbol.Make("procedure"),  // 0x0F
		                Symbol.Make("entity"),  // 0x10
		                Symbol.Make("delay"),  // 0x11
		                Symbol.Make("environment"),  // 0x12
		                Symbol.Make("promise"),  // 0x13
		                Symbol.Make("extended-lambda"),  // 0x14
		                Symbol.Make("comment"),  // 0x15
		                Symbol.Make("non-marked-vector"),  // 0x16
		                Symbol.Make("lambda"),  // 0x17
		                Symbol.Make("primitive"),  // 0x18
		                Symbol.Make("sequence-2"),  // 0x19
		                Symbol.Make("fixnum"),  // 0x1A
		                Symbol.Make("primitive-combination-1"),  // 0x1B
		                Symbol.Make("control-point"),  // 0x1C
		                Symbol.Make("interned-symbol"),  // 0x1D
		                Symbol.Make("string"),  // 0x1e
		                Symbol.Make("access"),  // 0x1f
		                Symbol.Make("hunk3-a"),  // 0x20
		                Symbol.Make("definition"),  // 0x21
		                Symbol.Make("broken-heart"),  // 0x22
		                Symbol.Make("assignment"),  // 0x23
		                Symbol.Make("triple"),  // 0x24
		                Symbol.Make("in-package"),  // 0x25
		                Symbol.Make("combination"),  // 0x26
		                Symbol.Make("manifest-nm-vector"),  // 0x27
		                Symbol.Make("compiled-entry"),  // 0x28
		                Symbol.Make("lexpr"),  // 0x29
		                Symbol.Make("primitive-combination-3"),  // 0x2a
			            null,  // 0x2b
		                Symbol.Make("variable"),  // 0x2c
		                Symbol.Make("the-environment"),  // 0x2d
			            null,  // 0x2e
		                Symbol.Make("vector-1b"),  // 0x2f
		                Symbol.Make("primitive-combination-0"),  // 0x30
		                Symbol.Make("vector-16b"),  // 0x31
		                Symbol.Make("reference-trap"),  // 0x32
		                Symbol.Make("sequence-3"),  // 0x33
		                Symbol.Make("conditional"),  // 0x34
		                Symbol.Make("disjunction"),  // 0x35
		                Symbol.Make("cell"),  // 0x36
		                Symbol.Make("weak-cons"),  // 0x37
		                Symbol.Make("quad"),  // 0x38
		                Symbol.Make("linkage-section"),  // 0x39
		                Symbol.Make("ratnum"),  // 0x3a
		                Symbol.Make("stack-environment"),  // 0x3b
		                Symbol.Make("recnum"),  // 0x3c
		                Symbol.Make("compiled-code-block"),  // 0x3d
		                Symbol.Make("record"),  // 0x3e
			            null  // 0x3f
                };
            theFixedObjectsVector [ReturnsVector] =
                new object [] {
	                    Symbol.Make("non-existent-continuation"),  // 0x00
	                    Symbol.Make("join-stacklets"),  // 0x01
		                null,  // 0x02
	                    Symbol.Make("internal-apply"),  // 0x03
		                null,  // 0x04
	                    Symbol.Make("restore-history"),  // 0x05
	                    Symbol.Make("invoke-stack-thread"),  // 0x06
		                null,  // 0x07
	                    Symbol.Make("assignment-continue"),  // 0x08
	                    Symbol.Make("definition-continue"),  // 0x09
	                    Symbol.Make("access-continue"),  // 0x0a
	                    Symbol.Make("in-package-continue"),  // 0x0b
	                    Symbol.Make("sequence-2-second"),  // 0x0c
	                    Symbol.Make("sequence-3-second"),  // 0x0d
	                    Symbol.Make("sequence-3-third"),  // 0x0e
	                    Symbol.Make("conditional-decide"),  // 0x0f
	                    Symbol.Make("disjunction-decide"),  // 0x10
	                    Symbol.Make("combination-1-procedure"),  // 0x11
	                    Symbol.Make("combination-apply"),  // 0x12
	                    Symbol.Make("combination-2-first-operand"),  // 0x13
	                    Symbol.Make("combination-2-procedure"),  // 0x14
	                    Symbol.Make("combination-save-value"),  // 0x15
	                    Symbol.Make("primitive-combination-1-apply"),  // 0x16
	                    Symbol.Make("primitive-combination-2-first-operand"),  // 0x17
	                    Symbol.Make("primitive-combination-2-apply"),  // 0x18
	                    Symbol.Make("primitive-combination-3-second-operand"),  // 0x19
	                    Symbol.Make("primitive-combination-3-first-operand"),  // 0x1a
	                    Symbol.Make("primitive-combination-3-apply"),  // 0x1b
	                    Symbol.Make("force-snap-thunk"),  // 0x1c
	                    Symbol.Make("reenter-compiled-code"),  // 0x1d
		                null,  // 0x1e
		                null,  // 0x1f
	                    Symbol.Make("normal-garbage-collect-done"),  // 0x20
		                null,  // 0x21
	                    Symbol.Make("purify-after-first-gc"),  // 0x22
	                    Symbol.Make("purify-after-second-gc"),  // 0x23
		                null,  // 0x24
		                null,  // 0x25
		                null,  // 0x26
		                null,  // 0x27
		                null,  // 0x28
	                    Symbol.Make("pop-from-compiled-code"),  // 0x29
	                    Symbol.Make("return-trap-point"),  // 0x2a
		                null,  // 0x2b
	                    Symbol.Make("restore-to-state-point"),  // 0x2c
	                    Symbol.Make("move-to-adjacent-point"),  // 0x2d
	                    Symbol.Make("restore-value"),  // 0x2e
	                    Symbol.Make("restore-dont-copy-history"),  // 0x2f
		                null,  // 0x30
		                null,  // 0x31
		                null,  // 0x32
		                null,  // 0x33
		                null,  // 0x34
		                null,  // 0x35
		                null,  // 0x36
		                null,  // 0x37
		                null,  // 0x38
		                null,  // 0x39
		                null,  // 0x3a
		                null,  // 0x3b
		                null,  // 0x3c
		                null,  // 0x3d
		                null,  // 0x3e
		                null,  // 0x3f
	                    Symbol.Make("pop-return-error"),  // 0x40
	                    Symbol.Make("eval-error"),  // 0x41
	                    Symbol.Make("stack-marker"),  // 0x42
	                    Symbol.Make("compiler-interrupt-restart"),  // 0x43
		                null,  // 0x44
	                    Symbol.Make("restore-interrupt-mask"),  // 0x45
	                    Symbol.Make("halt"),  // 0x46
		                null,  // 0x47
	                    Symbol.Make("repeat-dispatch"),  // 0x48
	                    Symbol.Make("gc-check"),  // 0x49
		                null,  // 0x4a
		                null,  // 0x4b
		                null,  // 0x4c
		                null,  // 0x4d
		                null,  // 0x4e
		                null,  // 0x4f
		                null,  // 0x50
		                null,  // 0x51
		                null,  // 0x52
	                    Symbol.Make("compiler-reference-trap-restart"),  // 0x53
	                    Symbol.Make("compiler-assignment-trap-restart"),  // 0x54
		                null,  // 0x55
	                    Symbol.Make("compiler-operator-lookup-trap-restart"),  // 0x56
	                    Symbol.Make("compiler-lookup-apply-trap-restart"),  // 0x57
	                    Symbol.Make("compiler-safe-reference-trap-restart"),  // 0x58
	                    Symbol.Make("compiler-unassigned?-trap-restart"),  // 0x59
		                null,  // 0x5a
	                    Symbol.Make("compiler-link-caches-restart"),  // 0x5b
	                    Symbol.Make("hardware-trap"),  // 0x5c
	                    Symbol.Make("internal-apply-val"),  // 0x5d
	                    Symbol.Make("compiler-error-restart"),  // 0x5e
	                    Symbol.Make("primitive-continue")  // 0x5f

                };
            theFixedObjectsVector [ErrorsVector] =
                new object [] {
                    	Symbol.Make("bad-error-code"),  // 0x00
	                    Symbol.Make("unbound-variable"),  // 0x01
	                    Symbol.Make("unassigned-variable"),  // 0x02
	                    Symbol.Make("undefined-procedure"),  // 0x03
	                    Symbol.Make("system-call"),  // 0x04
	                    Symbol.Make("error-with-argument"),  // 0x05
	                    Symbol.Make("bad-frame"),  // 0x06
	                    Symbol.Make("broken-compiled-variable"),  // 0x07
	                    Symbol.Make("undefined-user-type"),  // 0x08
	                    Symbol.Make("undefined-primitive-operation"),  // 0x09
	                    Symbol.Make("external-return"),  // 0x0a
	                    Symbol.Make("execute-manifest-vector"),  // 0x0b
	                    Symbol.Make("wrong-number-of-arguments"),  // 0x0c
	                    Symbol.Make("wrong-type-argument-0"),  // 0x0d
	                    Symbol.Make("wrong-type-argument-1"),  // 0x0e
	                    Symbol.Make("wrong-type-argument-2"),  // 0x0f
	                    Symbol.Make("bad-range-argument-0"),  // 0x10
	                    Symbol.Make("bad-range-argument-1"),  // 0x11
	                    Symbol.Make("bad-range-argument-2"),  // 0x12
	                    Symbol.Make("macro-binding"),  // 0x13
	                    Symbol.Make("fasdump-object-too-large"),  // 0x14
		                null,  // 0x15
		                null,  // 0x16
	                    Symbol.Make("fasl-file-too-big"),  // 0x17
	                    Symbol.Make("fasl-file-bad-data"),  // 0x18
		                null,  // 0x19
		                null,  // 0x1a
		                null,  // 0x1b
		                null,  // 0x1c
	                    Symbol.Make("bad-assignment"),  // 0x1d
	                    Symbol.Make("failed-arg-1-coercion"),  // 0x1e
	                    Symbol.Make("failed-arg-2-coercion"),  // 0x1f
	                    Symbol.Make("out-of-file-handles"),  // 0x20
		                null,  // 0x21
	                    Symbol.Make("bad-range-argument-3"),  // 0x22
	                    Symbol.Make("bad-range-argument-4"),  // 0x23
	                    Symbol.Make("bad-range-argument-5"),  // 0x24
	                    Symbol.Make("bad-range-argument-6"),  // 0x25
	                    Symbol.Make("bad-range-argument-7"),  // 0x26
	                    Symbol.Make("bad-range-argument-8"),  // 0x27
	                    Symbol.Make("bad-range-argument-9"),  // 0x28
	                    Symbol.Make("wrong-type-argument-3"),  // 0x29
	                    Symbol.Make("wrong-type-argument-4"),  // 0x2a
	                    Symbol.Make("wrong-type-argument-5"),  // 0x2b
	                    Symbol.Make("wrong-type-argument-6"),  // 0x2c
	                    Symbol.Make("wrong-type-argument-7"),  // 0x2d
	                    Symbol.Make("wrong-type-argument-8"),  // 0x2e
	                    Symbol.Make("wrong-type-argument-9"),  // 0x2f
	                    Symbol.Make("inapplicable-continuation"),  // 0x30
	                    Symbol.Make("compiled-code-error"),  // 0x31
	                    Symbol.Make("floating-overflow"),  // 0x32
	                    Symbol.Make("unimplemented-primitive"),  // 0x33
	                    Symbol.Make("illegal-reference-trap"),  // 0x34
	                    Symbol.Make("broken-variable-cache"),  // 0x35
	                    Symbol.Make("wrong-arity-primitives"),  // 0x36
	                    Symbol.Make("io-error"),  // 0x37
	                    Symbol.Make("fasdump-environment"),  // 0x38
	                    Symbol.Make("fasload-band"),  // 0x39
	                    Symbol.Make("fasload-compiled-mismatch"),  // 0x3a
	                    Symbol.Make("unknown-primitive-continuation"),  // 0x3b
	                    Symbol.Make("illegal-continuation"),  // 0x3c
	                    Symbol.Make("stack-has-slipped"),  // 0x3d
	                    Symbol.Make("cannot-recurse")  // 0x3e
                        };

            theFixedObjectsVector [IdentificationVector] =
                new object [] {
                    Symbol.Make ("system-release-string"), // 0x00
                    Symbol.Make ("microcode-version"), // 0x01
                    null,
                    Symbol.Make ("console-width"), // 0x03
                    Symbol.Make ("console-height"), // 0x04
                    Symbol.Make ("newline-char"), //0x05
                    Symbol.Make ("flonum-mantissa-length"), // 0x06
                    Symbol.Make ("flonum-epsilon"), // 0x07
                    Symbol.Make ("os-name-string"), // 0x08
                    Symbol.Make ("os-variant-string"), // 0x09
                    Symbol.Make ("stack-type-string"), // 0x0A
                    Symbol.Make ("machine-type-string"), // 0x0B
                    Symbol.Make ("cc-arch-string") // 0x0C
                };
            theFixedObjectsVector [SystemCallNames] = new object [0];
            theFixedObjectsVector [SystemErrorNames] = new object [0];
            theFixedObjectsVector [FixedObjectSlots] = 
                new object [] {
                            Symbol.Make ("non-object"), // 0x00
                            Symbol.Make ("system-interrupt-vector"), // 0x01
                            Symbol.Make("system-error-vector"),  // 0x02
                            Symbol.Make("obarray"),  // 0x03
                            Symbol.Make("microcode-types-vector"),  // 0x04
                            Symbol.Make("microcode-returns-vector"),  // 0x05
                            Symbol.Make("interrupt-mask-vector"),  // 0x06
                            Symbol.Make("microcode-errors-vector"),  // 0x07
                            Symbol.Make("microcode-identification-vector"),  // 0x08
                            Symbol.Make("system-call-names"),  // 0x09
                            Symbol.Make("system-call-errors"),  // 0x0A
                            Symbol.Make("gc-daemon"),  // 0x0B
                            Symbol.Make("trap-handler"),  // 0x0C
                            Symbol.Make("edwin-auto-save"),  // 0x0D
                            Symbol.Make("stepper-state"),  // 0x0E
                            Symbol.Make("microcode-fixed-objects-slots"),  // 0x0F
                            Symbol.Make("files-to-delete"),  // 0x10
                            Symbol.Make("state-space-tag"),  // 0x11
                            Symbol.Make("state-point-tag"),  // 0x12
                            Symbol.Make("dummy-history"),  // 0x13
                            Symbol.Make("bignum-one"),  // 0x14
                            null,  // 0x15
                            Symbol.Make("microcode-terminations-vector"),  // 0x16
                            Symbol.Make("microcode-terminations-procedures"),  // 0x17
                            null,  // 0x18
                            null,  // 0x19
                            null,  // 0x1A
                            null,  // 0x1B
                            null,  // 0x1C
                            Symbol.Make("error-procedure"),  // 0x1D
                            null,  // 0x1E
                            null,  // 0x1F
                            Symbol.Make("compiler-error-procedure"),  // 0x20
                            null,  // 0x21
                            Symbol.Make("state-space-root"),  // 0x22
                            Symbol.Make("primitive-profiling-table"),  // 0x23
                            Symbol.Make("generic-trampoline-zero?"),  // 0x24
                            Symbol.Make("generic-trampoline-positive?"),  // 0x25
                            Symbol.Make("generic-trampoline-negative?"),  // 0x26
                            Symbol.Make("generic-trampoline-add-1"),  // 0x27
                            Symbol.Make("generic-trampoline-subtract-1"),  // 0x28
                            Symbol.Make("generic-trampoline-equal?"),  // 0x29
                            Symbol.Make("generic-trampoline-less?"),  // 0x2A
                            Symbol.Make("generic-trampoline-greater?"),  // 0x2B
                            Symbol.Make("generic-trampoline-add"),  // 0x2C
                            Symbol.Make("generic-trampoline-subtract"),  // 0x2D
                            Symbol.Make("generic-trampoline-multiply"),  // 0x2E
                            Symbol.Make("generic-trampoline-divide"),  // 0x2F
                            Symbol.Make("generic-trampoline-quotient"),  // 0x30
                            Symbol.Make("generic-trampoline-remainder"),  // 0x31
                            Symbol.Make("generic-trampoline-modulo"),  // 0x32
                            Symbol.Make("arity-dispatcher-tag"),  // 0x33
                            Symbol.Make("pc-sample/builtin-table"),  // 0x34
                            Symbol.Make("pc-sample/utility-table"),  // 0x35
                            Symbol.Make("pc-sample/primitive-table"),  // 0x36
                            Symbol.Make("pc-sample/code-block-table"),  // 0x37
                            Symbol.Make("pc-sample/purified-code-block-block-buffer"),  // 0x38
                            Symbol.Make("pc-sample/purified-code-block-offset-buffer"),  // 0x39
                            Symbol.Make("pc-sample/heathen-code-block-block-buffer"),  // 0x3A
                            Symbol.Make("pc-sample/heathen-code-block-offset-buffer"),  // 0x3B
                            Symbol.Make("pc-sample/interp-proc-buffer"),  // 0x3C
                            Symbol.Make("pc-sample/prob-comp-table"),  // 0x3D
                            Symbol.Make("pc-sample/ufo-table"),  // 0x3E
                            Symbol.Make("compiled-code-bkpt-handler"),  // 0x3F
                            Symbol.Make("gc-wabbit-descwiptor"),  // 0x40
                            null,  // 0x41
                            null,  // 0x42
                            null,  // 0x43
                            null,  // 0x44
                            null  // 0x45
        };
            theFixedObjectsVector [SystemInterruptVector] = new object [15];
            theFixedObjectsVector [DummyHistory] = History.DummyHistory ();
            theFixedObjectsVector [TerminationVector] = new object [] {
                	        Symbol.Make("halt"),  // 0x00
	                        Symbol.Make("disk-restore"),  // 0x01
	                        Symbol.Make("broken-heart"),  // 0x02
	                        Symbol.Make("non-pointer-relocation"),  // 0x03
	                        Symbol.Make("bad-root"),  // 0x04
	                        Symbol.Make("non-existent-continuation"),  // 0x05
	                        Symbol.Make("bad-stack"),  // 0x06
	                        Symbol.Make("stack-overflow"),  // 0x07
	                        Symbol.Make("stack-allocation-failed"),  // 0x08
	                        Symbol.Make("no-error-handler"),  // 0x09
	                        Symbol.Make("no-interrupt-handler"),  // 0x0a
	                        Symbol.Make("unimplemented-continuation"),  // 0x0b
	                        Symbol.Make("exit"),  // 0x0c
	                        Symbol.Make("bad-primitive-during-error"),  // 0x0d
	                        Symbol.Make("eof"),  // 0x0e
	                        Symbol.Make("bad-primitive"),  // 0x0f
	                        Symbol.Make("termination-handler"),  // 0x10
	                        Symbol.Make("end-of-computation"),  // 0x11
	                        Symbol.Make("invalid-type-code"),  // 0x12
	                        Symbol.Make("compiler-death"),  // 0x13
	                        Symbol.Make("gc-out-of-space"),  // 0x14
	                        Symbol.Make("no-space"),  // 0x15
	                        Symbol.Make("signal"),  // 0x16
		                    null,  // 0x17
	                        Symbol.Make("save-and-exit"),  // 0x18
	                        Symbol.Make("trap"),  // 0x19
	                        Symbol.Make("bad-back-out")  // 0x1a
            };
            theFixedObjectsVector [TerminationProcVector] = new object [0];
            theFixedObjectsVector [MeMyself] = theFixedObjectsVector;
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

        public static object [] ErrorVector
        {
            get
            {
                return (object []) theFixedObjectsVector [SystemErrorVector];
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
            theFixedObjectsVector [Obarray] = Symbol.GetObarray ();
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
