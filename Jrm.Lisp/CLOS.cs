using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;

namespace Lisp
{

    public class MethodCache
    {
        Cons table;

        static bool Equal (Cons left, Cons right) {
            if ((left == null) && (right == null))
                return true;
            if (left == null)
                return false;
            if (right == null)
                return false;
            if (left.Car.Equals(right.Car))
                return Equal ((Cons) left.Cdr, (Cons) right.Cdr);
            else
                return false;
        }

        static Cons Assoc (Cons key, Cons table) {
            if (table == null)
                return null;
            Cons entry = (Cons) table.Car;
            if (Equal (key, (Cons) entry.Car))
                return entry;
            else
        {

                return Assoc (key, (Cons) table.Cdr);
        }
        }

        static Cons standinForNull = new Cons (null, null);
        internal bool TryGetValue (Cons key, out FuncallHandler value)
        {
            Cons probe = Assoc (key == null ? standinForNull : key, table);
            if (probe == null) {
                value = null;
                return false;
            }
            else {
                value = (FuncallHandler) probe.Cdr;
                return true;
            }
        }

        internal void Add (object key, FuncallHandler value)
        {
            // will shadow
                table = new Cons (new Cons (key, value), table);
        }

        internal bool ContainsKey (object key)
        {
            throw new NotImplementedException ("MethodCache::ContainsKey");
        }
    }

    public delegate object NextMethodFunction (params object [] arguments);
    public delegate object MethodStep (NextMethodFunction callNextMethod, params object [] arguments);

    public class MethodStepWrapper
    {

        Delegate del;

        public MethodStepWrapper (Delegate del)
        {
            this.del = del;
        }

        object Funcall2 (NextMethodFunction nmf, object [] arguments)
        {
            return this.del.DynamicInvoke (nmf, arguments [0]);
        }

        object Funcall2r (NextMethodFunction nmf, object [] arguments)
        {
            object [] remaining = new object [arguments.Length - 1];
            Array.Copy(arguments,1,remaining,0,arguments.Length-1);
            return this.del.DynamicInvoke (nmf, arguments [0], remaining);
        }

        object Funcall3 (NextMethodFunction nmf, object [] arguments)
        {
            if (arguments.Length != 2)
                throw new NotImplementedException ("wrong number of arguments");
            return this.del.DynamicInvoke (nmf, arguments [0], arguments [1]);
        }

        static MethodInfo GetFuncallMethod (Delegate del)
        {
            ParameterInfo [] parameters = del.Method.GetParameters ();
            ParameterInfo lastParameter = parameters [parameters.Length - 1];
            object [] attr = lastParameter.GetCustomAttributes (typeof (System.ParamArrayAttribute), false);
            string funcall = "Funcall" + (parameters.Length - ((attr.Length > 0) ? 1 : 0)).ToString () + ((attr.Length > 0) ? "r" : "");
            MethodInfo mi =
            typeof (MethodStepWrapper)
                .GetMethod (funcall, BindingFlags.NonPublic | BindingFlags.Instance);
            if (mi == null)
                throw new NotImplementedException (funcall);
            return mi;
        }

        public static MethodStep Create (Delegate del)
        {
            return (MethodStep)
                Delegate.CreateDelegate (typeof (MethodStep),
                                            new MethodStepWrapper (del),
                                            GetFuncallMethod (del)
                                            );
        }
    }

    public static class CLOS
    {
        static object UnboundSlotValue = UnboundSlot.Value;

        public static object Init ()
        {
            return UnboundSlotValue;
        }

        static StandardObject Class (this StandardObject obj)
        {
            return ((ManifestInstance) obj.Target).Class;
        }

        // Extension methods for StandardObject
        static object InstanceRef (this StandardObject obj, int index)
        {
            object result = ((ManifestInstance) obj.Target).Slots [index];
            if (result == UnboundSlotValue)
                throw new NotImplementedException ("UnboundSlot");
            return result;
        }

        static object InstanceSet (this StandardObject obj, int index, object newValue)
        {
            ((ManifestInstance) obj.Target).Slots [index] = newValue;
            return newValue;
        }

        static object InstanceChange (this StandardObject obj, int index, SlotChangingFunction change)
        {
            object [] slots = ((ManifestInstance) obj.Target).Slots;
            object newValue = change.Invoke (slots [index]);
            slots [index] = newValue;
            return newValue;
        }

        static int SerialNumber (this StandardObject obj)
        {
            return ((ManifestInstance) obj.Target).SerialNumber;
        }

        //// End of extension methods

        // Not an extension, but Instance-specific.
        static void setFuncallableInstanceFunction (StandardObject funcallableInstance, FuncallHandler handler)
        {
            ((ManifestInstance) funcallableInstance.Target).OnFuncall = handler;
        }

        static readonly Package closPackage = Package.Clos;

        #region Symbols
        static readonly Symbol QuoteAndAllowOtherKeys = Package.CommonLisp.FindSymbol ("&ALLOW-OTHER-KEYS");
        static readonly Symbol QuoteAndKey = Package.CommonLisp.FindSymbol ("&KEY");
        static readonly Symbol QuoteAndRest = Package.CommonLisp.FindSymbol ("&REST");
        static readonly Symbol QuoteAnonymous = Package.CommonLisp.FindSymbol ("[unnamed]");
        static readonly Symbol QuoteAddMethod = Package.CommonLisp.FindSymbol ("ADD-METHOD");
        static readonly Symbol QuoteAllocateInstance = Package.CommonLisp.FindSymbol ("ALLOCATE-INSTANCE");
        static readonly Symbol QuoteApplicationCache = closSymbol ("APPLICATION-CACHE");
        static readonly Symbol QuoteArguments = closSymbol ("ARGUMENTS");
        static readonly Symbol QuoteArgumentPrecedenceOrder = closSymbol ("ARGUMENT-PRECEDENCE-ORDER");
        static readonly Symbol QuoteBuiltInClass = closSymbol ("BUILT-IN-CLASS");
        static readonly Symbol QuoteClassPrecedenceList = closSymbol ("CLASS-PRECEDENCE-LIST");
        static readonly Symbol QuoteClassPrototype = closSymbol ("CLASS-PROTOTYPE");
        static readonly Symbol QuoteClassSlots = closSymbol ("CLASS-SLOTS");
        static readonly Symbol QuoteClass = closSymbol ("CLASS");
        static readonly Symbol QuoteComputeApplicableMethods = closSymbol ("COMPUTE-APPLICABLE-METHODS");
        static readonly Symbol QuoteComputeDiscriminatingFunction = closSymbol ("COMPUTE-DISCRIMINATING-FUNCTION");
        static readonly Symbol QuoteComputeEffectiveMethod = closSymbol ("COMPUTE-EFFECTIVE-METHOD");
        static readonly Symbol QuoteComputeMethodMoreSpecific = closSymbol ("COMPUTE-METHOD-MORE-SPECIFIC");
        static readonly Symbol QuoteDeclarations = closSymbol ("DECLARATIONS");
        static readonly Symbol QuoteDefaultInitargs = closSymbol ("DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectDefaultInitargs = closSymbol ("DIRECT-DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectSlots = closSymbol ("DIRECT-SLOTS");
        static readonly Symbol QuoteDirectSubclasses = closSymbol ("DIRECT-SUBCLASSES");
        static readonly Symbol QuoteDirectSuperclasses = closSymbol ("DIRECT-SUPERCLASSES");
        static readonly Symbol QuoteDocumentation = closSymbol ("DOCUMENTATION");
        static readonly Symbol QuoteEnsureGenericFunction = Package.CommonLisp.FindSymbol ("ENSURE-GENERIC-FUNCTION");
        static readonly Symbol QuoteFinalizedP = closSymbol ("FINALIZEDP");
        static readonly Symbol QuoteFuncallableStandardClass = closSymbol ("FUNCALLABLE-STANDARD-CLASS");
        static readonly Symbol QuoteFuncallableStandardObject = closSymbol ("FUNCALLABLE-STANDARD-OBJECT");
        static readonly Symbol QuoteFunction = closSymbol ("FUNCTION");
        static readonly Symbol QuoteFunctionName = closSymbol ("FUNCTION-NAME");
        static readonly Symbol QuoteGenericFunction = closSymbol ("GENERIC-FUNCTION");
        static readonly Symbol QuoteGenericFunctionClass = closSymbol ("GENERIC-FUNCTION-CLASS");
        static readonly Symbol QuoteGenericFunctionMethodClass = closSymbol ("GENERIC-FUNCTION-METHOD-CLASS");
        static readonly Symbol QuoteGettersAndSetters = closSymbol ("GETTERS-AND-SETTERS");
        static readonly Symbol QuoteInstance = closSymbol ("INSTANCE");
        static readonly Symbol QuoteInitargs = closSymbol ("INITARGS");
        static readonly Symbol QuoteInitializeInstance = closSymbol ("INITIALIZE-INSTANCE");
        static readonly Symbol QuoteLambdaList = closSymbol ("LAMBDA-LIST");
        static readonly Symbol QuoteList = Package.CommonLisp.FindSymbol ("LIST");
        static readonly Symbol QuoteMakeInstance = Package.CommonLisp.FindSymbol ("MAKE-INSTANCE");
        static readonly Symbol QuoteMetaobject = closSymbol ("METAOBJECT");
        static readonly Symbol QuoteMethod = closSymbol ("METHOD");
        static readonly Symbol QuoteMethodAllocateInstance = closSymbol ("METHOD:allocate-instance");
        static readonly Symbol QuoteMethodDefaultInitargs = closSymbol ("METHOD:default-initargs");
        static readonly Symbol QuoteMethodComputeApplicableMethods = closSymbol ("METHOD:compute-applicable-methods");
        static readonly Symbol QuoteMethodComputeDiscriminatingFunction = closSymbol ("METHOD:compute-discriminating-function");
        static readonly Symbol QuoteMethodComputeEffectiveMethod = closSymbol ("METHOD:compute-effective-method");
        static readonly Symbol QuoteMethodComputeMethodMoreSpecific = closSymbol ("METHOD:compute-method-more-specific");
        static readonly Symbol QuoteMethods = closSymbol ("METHODS");
        static readonly Symbol QuoteMethodClass = closSymbol ("METHOD-CLASS");
        static readonly Symbol QuoteMethodCombination = closSymbol ("METHOD-COMBINATION");
        static readonly Symbol QuoteMethodMethodQualifier = closSymbol ("METHOD:method-qualifier");
        static readonly Symbol QuoteMethodMethodSpecializers = closSymbol ("METHOD:method-specializers");
        static readonly Symbol QuoteMethodQualifier = closSymbol ("METHOD-QUALIFIER");
        static readonly Symbol QuoteMethodSpecializers = closSymbol ("METHOD-SPECIALIZERS");
        static readonly Symbol QuoteName = closSymbol ("NAME");
        static readonly Symbol QuoteNewMakeInstance = closSymbol ("NEW-MAKE-INSTANCE");
        static readonly Symbol QuoteNewMakeInstanceMethod = closSymbol ("NEW-MAKE-INSTANCE-METHOD");
        static readonly Symbol QuoteProcedure = closSymbol ("PROCEDURE");
        static readonly Symbol QuoteQualifier = closSymbol ("QUALIFIER");
        static readonly Symbol QuoteSingletonsList = closSymbol ("SINGLETONS-LIST");
        static readonly Symbol QuoteSlotNames = closSymbol ("SLOT-NAMES");
        static readonly Symbol QuoteSpecializer = closSymbol ("SPECIALIZER");
        static readonly Symbol QuoteSpecializers = closSymbol ("SPECIALIZERS");
        static readonly Symbol QuoteStandardAccessorMethod = closSymbol ("STANDARD-ACCESSOR-METHOD");
        static readonly Symbol QuoteStandardReaderMethod = closSymbol ("STANDARD-READER-METHOD");
        static readonly Symbol QuoteStandardWriterMethod = closSymbol ("STANDARD-WRITER-METHOD");
        static readonly Symbol QuoteStandardClass = closSymbol ("STANDARD-CLASS");   
        static readonly Symbol QuoteStandardGenericFunction = closSymbol ("STANDARD-GENERIC-FUNCTION");
        static readonly Symbol QuoteStandardMethod = Package.CommonLisp.FindSymbol ("STANDARD-METHOD");
        static readonly Symbol QuoteStandardObject = closSymbol ("STANDARD-OBJECT");
        static readonly Symbol QuoteStudlyName = closSymbol ("StudlyName");
        static readonly Symbol QuoteSuppliedInitargs = closSymbol ("SUPPLIED-INITARGS");
        static readonly Symbol QuoteTop = closSymbol ("TOP");
        #endregion Symbols

        public delegate object SlotReader (StandardObject obj);
        delegate object SlotWriter (StandardObject obj, object newValue);
        delegate object SlotChangingFunction (object currentValue);
        delegate object SlotChanger (StandardObject obj, SlotChangingFunction func);


                                   

        // ClassOf
        delegate StandardObject ClassOfFunction (object obj);

        static ClassOfFunction classOf =
            new ClassOfFunction (delegate (object instance)
        {
            StandardObject probe = instance as StandardObject;
            return (probe == null) ? Top : probe.Class();
        });

        static ClassOfFunction ClassOf
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return classOf;
            }
        }

        delegate Cons amf (Object obj);
        static Cons AppendMap (amf del, Cons list)
        {
            Cons answer = null;
            while (true) {
                if (list == null)
                    break;
                answer = (Cons) CL.Append (del (list.Car), answer);
                list = (Cons) list.Cdr;
            }

            return answer;

        }

        static Cons bootstrapComputeClassPrecedenceList (Cons superclasses, Cons soFar)
        {
            if (superclasses == null)
                return (Cons) CL.Reverse (soFar);
            return bootstrapComputeClassPrecedenceList ((Cons) CL.Append (superclasses.Cdr, InternalClassDirectSuperclasses ((StandardObject) superclasses.Car)),
                                                        CL.Memq (superclasses.Car, soFar)
                                                           ? soFar
                                                           : CL.Cons (superclasses.Car, soFar));
        }

        delegate Cons Allocator (object initargs);

        static object unspecificInitializer ()
        {
            throw new NotImplementedException ();
        }

        static object bootstrapMakeInstance (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            Cons initargs = Cons.SubvectorToList (arguments, 1, arguments.Length);
            if (object.ReferenceEquals (closClass,StandardClass)
                || object.ReferenceEquals (closClass,FuncallableStandardClass)) {
                StandardObject instance = ManifestInstance.CreateInstance (closClass, CL.Length (SLOTS_OF_STANDARD_CLASS));

                Cons directDefaultInitargs = (Cons) Utility.GetArg (initargs, KW.DirectDefaultInitargs, null);
                Cons directSlots = (Cons) Utility.GetArg (initargs, KW.DirectSlots, null);
                Cons directSuperclasses = (Cons) Utility.GetArg (initargs, KW.DirectSuperclasses, null);
                Symbol name = (Symbol) Utility.GetArg (initargs, KW.Name, QuoteAnonymous);
                string studlyName = (string) Utility.GetArg (initargs, KW.StudlyName, "AnonymousClass");
                Cons classPrecedenceList = bootstrapComputeClassPrecedenceList (directSuperclasses, CL.List (instance));
                Cons inheritedSlots = AppendMap (new amf (delegate (object superclass)
                {
                    return InternalClassDirectSlots ((StandardObject) superclass);
                }), (Cons) CL.Cdr (classPrecedenceList));
                Cons slots = (Cons) CL.Append (directSlots, inheritedSlots);

                int nfields = 0;
                Cons fieldInitializers = null;



                Delegate allocator = new Allocator (delegate (object init)
                {
                    int f = nfields;
                    nfields += 1;
                    fieldInitializers = new Cons (init, fieldInitializers);
                    return CL.List (
                         new SlotReader (delegate (StandardObject o)
                             {
                                 return StandardInstanceAccess (o, f);
                             }),
                             new SlotWriter (delegate (StandardObject o, object newValue)
                             {
                                 return SetStandardInstanceAccess (o, f, newValue);
                             }),
                             new SlotChanger (delegate (StandardObject o, SlotChangingFunction modify)
                             {
                                 return StandardInstanceChange (o, f, modify);
                             }));
                });

                Cons gettersAndSetters =
                  (Cons) CL.Map (QuoteList, new MapFunction (
                                      delegate (Cons s)
                                      {
                                          return CL.Cons (s.Car, allocator.DynamicInvoke (Utility.GetArg (s.Cdr, KW.Initializer, null)));
                                      }), slots);
                if (CL.Length (slots) < CL.Length (directSlots))
                    throw new NotImplementedException ();
                if (CL.Length (slots) != CL.Length (gettersAndSetters))
                    throw new NotImplementedException ();
                internalSetClassDefaultInitargs (instance, null); // don't install yet
                internalSetClassDirectDefaultInitargs (instance, directDefaultInitargs);
                internalSetClassDirectSlots (instance, directSlots);
                internalSetClassDirectSubclasses (instance, null);
                internalSetClassDirectSuperclasses (instance, directSuperclasses);
                internalSetClassIsFinalized (instance, true);
                internalSetClassName (instance, name);
                internalSetClassStudlyName (instance, studlyName);
                internalSetClassPrecedenceList (instance, classPrecedenceList);
                internalSetClassPrototype (instance, null);
                internalSetClassGettersAndSetters (instance, gettersAndSetters);
                internalSetClassSlots (instance, null);
                return instance;
            }
            else if (object.ReferenceEquals (closClass, StandardGenericFunction)) {
                Cons sentinel = new Cons (null, null);
                Cons lambdaList = (Cons) Utility.GetArg (initargs, KW.LambdaList, sentinel);
                if (object.ReferenceEquals (sentinel, lambdaList))
                    throw new NotImplementedException ("missing lambda list");
                StandardObject answer = ManifestInstance.CreateFuncallableInstance (closClass, CL.Length (internalClassDirectSlots (closClass)));
                internalSetGenericApplicationCache (answer, new Cons (GenericApplicationCacheTag, new MethodCache()));
                internalSetGenericMethodClass (answer, Utility.GetArg (initargs, KW.MethodClass, StandardMethod));
                internalSetGenericMethods (answer, null);
                internalSetGenericName (answer, Utility.GetArg (initargs, KW.Name, QuoteAnonymous));
                internalSetGenericStudlyName (answer, Utility.GetArg (initargs, KW.StudlyName, "AnonymousGeneric"));
                internalSetGenericLambdaList (answer, lambdaList);
                internalSetGenericSingletonsList (answer, null);
                return answer;
            }
            else if (object.ReferenceEquals (closClass, StandardMethod)
                || object.ReferenceEquals (closClass, StandardReaderMethod)) {
                StandardObject answer = ManifestInstance.CreateFuncallableInstance (closClass, CL.Length (InternalClassGettersAndSetters (closClass)));
                    internalSetMethodName (answer, Utility.GetArg (initargs, KW.Name, QuoteAnonymous));
                internalSetMethodStudlyName (answer, Utility.GetArg (initargs, KW.StudlyName, "AnonymousMethod"));
                InternalSetMethodProcedure (answer, (Delegate) Utility.GetArg (initargs, KW.Procedure, null));
                internalSetMethodQualifier (answer, Utility.GetArg (initargs, KW.Qualifier, KW.Primary));
                internalSetMethodSpecializers (answer, Utility.GetArg (initargs, KW.Specializers, null));
                return answer;
            }
            else

            throw new NotImplementedException ("something else");
        }

        static readonly StandardObject makeInstance = ManifestInstance.CreateFuncallableInstance (null, 0, bootstrapMakeInstance);

        static object bootstrapClassName (StandardObject className, object [] arguments)
        {
            return InternalClassName ((StandardObject) arguments [0]);
        }

        static object bootstrapGenericFunctionName (StandardObject gfn, object [] arguments)
        {
            return InternalGenericName ((StandardObject) arguments [0]);
        }        


        static readonly StandardObject className = ManifestInstance.CreateFuncallableInstance (null, 0, bootstrapClassName);
        static readonly StandardObject genericFunctionName = ManifestInstance.CreateFuncallableInstance (null, 0, bootstrapGenericFunctionName);

        static Cons lookupSlotInfo (StandardObject closClass, Symbol slotName)
        {
            Symbol className = null;
            object [] classSlots;
            Cons sng =  InternalClassGettersAndSetters (closClass);
            Cons probe = CL.Assq (slotName, sng);
            if (probe == null) {
                className =  InternalClassName (closClass);
                classSlots = ((ManifestInstance) closClass.Target).Slots;
                throw new NotImplementedException ("Slot not found");
            }
            return probe;
        }

        static object internalSlotRef (StandardObject obj, Symbol slotName)
        {
            return ((SlotReader) (CL.Cadr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj);
        }

        static internal object StandardObjectName (StandardObject obj)
        {
            try {
                // Try getting the StudlyName slot, if it exists.
                return internalSlotRef (obj, QuoteStudlyName);
            }
            catch (Exception) {
                try {
                    // Try getting the name slot, if it exists.
                    return internalSlotRef (obj, QuoteName);
                }
                catch (Exception) {

                    return null;
                }
            }
        }

        static object internalSlotSet (StandardObject obj, Symbol slotName, object value)
        {
            return ((SlotWriter) (CL.Caddr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj, value);
        }

        static object internalSlotChange (StandardObject obj, Symbol slotName, SlotChangingFunction value)
        {
            return ((SlotChanger) (CL.Cadddr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj, value);
        }

        #region InternalClassAccessors

        static StandardObject GuaranteeClass (StandardObject putativeClass)
        {
            if ((putativeClass.Class () != StandardClass)
                && (putativeClass.Class () != FuncallableStandardClass))
                throw new NotImplementedException ("GuaranteeClass");
            return putativeClass;
        }

        static SlotWriter internalSetClassDefaultInitargs =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDefaultInitargs, newValue);
        });

        static SlotWriter internalSetClassDirectDefaultInitargs =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDirectDefaultInitargs, newValue);
        });

        static SlotReader internalClassDirectSlots =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteDirectSlots);
        });

        static Cons InternalClassDirectSlots (StandardObject obj)
        {
            return (Cons) internalClassDirectSlots (GuaranteeClass (obj));
        }

        static SlotWriter internalSetClassDirectSlots =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDirectSlots, newValue);
        });

        static SlotWriter internalSetClassDirectSubclasses =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDirectSubclasses, newValue);
        });

        static SlotReader internalClassDirectSuperclasses =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteDirectSuperclasses);
        });

        static SlotWriter internalSetClassDirectSuperclasses =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDirectSuperclasses, newValue);
        });

        static Cons InternalClassDirectSuperclasses (StandardObject obj)
        {
            return (Cons) internalClassDirectSuperclasses (GuaranteeClass (obj));
        }

        static SlotWriter internalSetClassIsFinalized =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteFinalizedP, newValue);
        });

        static public SlotReader internalClassName =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteName);
        });

        static Symbol InternalClassName (StandardObject obj)
        {
            return (Symbol) internalClassName (GuaranteeClass (obj));
        }

        static SlotWriter internalSetClassName =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteName, newValue);
        });

        static SlotWriter internalSetClassStudlyName =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteStudlyName, newValue);
        });

        static SlotReader internalClassPrecedenceList =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteClassPrecedenceList);
        });

        static SlotWriter internalSetClassPrecedenceList =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteClassPrecedenceList, newValue);
        });

        static SlotWriter internalSetClassPrototype =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteClassPrototype, newValue);
            throw new NotImplementedException ();
        });

        static SlotWriter internalSetClassGettersAndSetters =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            //object [] slots = obj.Slots;
            return internalSlotSet (obj, QuoteGettersAndSetters, newValue);
        });

        static SlotReader internalClassSlots =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteClassSlots);
        });

        static SlotWriter internalSetClassSlots =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteClassSlots, newValue);
        });

        #endregion InternalClassAccessors

        #region InternalGenericAccessors

        static StandardObject GuaranteeGeneric (StandardObject putativeGeneric)
        {
            if (putativeGeneric.Class() != StandardGenericFunction)
                throw new NotImplementedException ("GuaranteeGeneric");
            return putativeGeneric;
        }

        static SlotReader internalGenericApplicationCache =
            new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteApplicationCache);
        });

        static SlotWriter internalSetGenericApplicationCache =
            new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (generic, QuoteApplicationCache, newValue);
        });

        static void InternalSetGenericApplicationCache (StandardObject generic, Cons newValue)
        {
           internalSetGenericApplicationCache (GuaranteeGeneric (generic), newValue);
        }

        static Cons InternalGenericApplicationCache (StandardObject generic)
        {
            return (Cons) internalGenericApplicationCache (GuaranteeGeneric (generic));
        }

        static SlotReader internalGenericLambdaList =
            new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteLambdaList);
        });

        static SlotWriter internalSetGenericLambdaList =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteLambdaList, newValue);
        });

        static Cons InternalGenericLambdaList (StandardObject generic)
        {
            return (Cons) internalGenericLambdaList (GuaranteeGeneric (generic));
        }

        static SlotReader internalGenericMethods =
            new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteMethods);
        });

        static Cons InternalGenericMethods (StandardObject generic)
        {
            return (Cons) internalGenericMethods (GuaranteeGeneric (generic));
        }

        static SlotWriter internalSetGenericMethods =
        new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (generic, QuoteMethods, (Cons) newValue);
        });

        static SlotChanger internalChangeGenericMethods =
            new SlotChanger (delegate (StandardObject generic, SlotChangingFunction func)
        {
            return internalSlotChange (generic, QuoteMethods, func);
        });

        static SlotReader internalGenericMethodClass =
            new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteMethodClass);
        });

        static StandardObject InternalGenericMethodClass (StandardObject generic)
        {
            return (StandardObject) internalGenericMethodClass (GuaranteeGeneric (generic));
        }

        static Cons InternalChangeGenericMethods (StandardObject generic, SlotChangingFunction func)
        {
            return (Cons) internalChangeGenericMethods (GuaranteeGeneric (generic), func);
        }

        static SlotWriter internalSetGenericMethodClass =
          new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (generic, QuoteMethodClass, newValue);
        });

        static SlotReader internalGenericName =
          new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteName);
        });

        static Symbol InternalGenericName (StandardObject obj)
        {
            return (Symbol) internalGenericName (GuaranteeGeneric (obj));
        }

        static SlotWriter internalSetGenericName =
          new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (generic, QuoteName, newValue);
        });

        static SlotWriter internalSetGenericStudlyName =
          new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (GuaranteeGeneric (generic), QuoteStudlyName, newValue);
        });

        static SlotReader internalGenericSingletonsList =
            new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteSingletonsList);
        });

        static SlotWriter internalSetGenericSingletonsList =
          new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            internalSlotSet (generic, QuoteSingletonsList, newValue);
            return newValue;
        });

        static Cons InternalGenericSingletonsList (StandardObject generic)
        {
           return (Cons) internalGenericSingletonsList (GuaranteeGeneric (generic));
        }

        static void InternalSetGenericSingletonsList (StandardObject generic, Cons newValue)
        {
           internalSetGenericSingletonsList (GuaranteeGeneric (generic), newValue);
        }

        #endregion InternalGenericAccessors

        #region InternalMethodAccessors

        // Method accessors
        static StandardObject GuaranteeMethod (StandardObject putativeMethod)
        {
            if (IsInstanceOf (putativeMethod, StandardMethod))
                return putativeMethod;
            else
                throw new NotImplementedException ("GuaranteeMethod");
        }

        static SlotReader internalMethodName = 
            new SlotReader (delegate (StandardObject method)
        {
            return internalSlotRef (method, QuoteName);
        });

        static Symbol InternalMethodName (StandardObject method)
        {
            return (Symbol) internalMethodName (GuaranteeMethod (method));
        }

        static SlotWriter internalSetMethodName = 
            new SlotWriter (delegate (StandardObject method, object newValue)
        {
            return internalSlotSet (method, QuoteName, newValue);
        });

        static SlotWriter internalSetMethodStudlyName =
          new SlotWriter (delegate (StandardObject method, object newValue)
        {
            return internalSlotSet (GuaranteeMethod (method), QuoteStudlyName, newValue);
        });

        static SlotReader internalMethodProcedure = 
          new SlotReader (delegate (StandardObject method)
        {
            return internalSlotRef (method, QuoteProcedure);
        });

        static Delegate InternalMethodProcedure (StandardObject method)
        {
            return (Delegate) internalMethodProcedure (GuaranteeMethod (method));
        }

        static SlotWriter internalSetMethodProcedure = 
          new SlotWriter (delegate (StandardObject method, object newValue)
        {
            return internalSlotSet (method, QuoteProcedure, newValue);
        });

        static void InternalSetMethodProcedure (StandardObject method, Delegate proc)
        {
            internalSetMethodProcedure (GuaranteeMethod (method), proc);
        }

        static SlotReader internalMethodQualifier =
          new SlotReader (delegate (StandardObject method)
        {
            return internalSlotRef (method, QuoteQualifier);
        });

        static SlotWriter internalSetMethodQualifier =
          new SlotWriter (delegate (StandardObject method, object newValue)
        {
            internalSlotSet (method, QuoteQualifier, newValue);
            return newValue;
        });

        static Keyword InternalMethodQualifier (StandardObject method)
        {
           return (Keyword) internalMethodQualifier (GuaranteeMethod (method));
        }


        static SlotReader internalMethodSpecializers =
            new SlotReader (delegate (StandardObject method)
        {
            return internalSlotRef (method, QuoteSpecializers);
        });

        static Cons InternalMethodSpecializers (StandardObject method)
        {
           return (Cons) internalMethodSpecializers (GuaranteeMethod (method));
        }

        static SlotWriter internalSetMethodSpecializers =
          new SlotWriter (delegate (StandardObject method, object specializers)
        {
            return internalSlotSet (method, QuoteSpecializers, specializers);
        });

        #endregion InternalMethodAccessors

        static bool IsInstanceOf (object item, object closClass)
        {
            if (closClass.Equals(Top))
                return true;
            StandardObject cx = ClassOf (item);
            if (cx.Equals(closClass))
                return true;
            else {
                return CL.Memq (closClass, internalClassPrecedenceList (GuaranteeClass (cx)));
            }
        }

        static bool InstancesOf (Cons items, Cons classes)
        {
            if ((items == null) || (classes == null))
                return true;
            return IsInstanceOf (items.Car, classes.Car) && InstancesOf ((Cons)items.Cdr, (Cons)classes.Cdr);
        }

        static Cons SLOTS_OF_STANDARD_CLASS =
              CL.List (CL.List (QuoteDefaultInitargs),
                       CL.List (QuoteDirectDefaultInitargs),
                       CL.List (QuoteDirectSlots),
                       CL.List (QuoteDirectSubclasses),
                       CL.List (QuoteDirectSuperclasses),
                       CL.List (QuoteFinalizedP),
                       CL.List (QuoteName),
                       CL.List (QuoteStudlyName),
                       CL.List (QuoteClassPrecedenceList),
                       CL.List (QuoteClassPrototype),
                       CL.List (QuoteGettersAndSetters),
                       CL.List (QuoteClassSlots));

        static private StandardObject bootstrapMakeStandardClass ()
        {
            StandardObject instance = ManifestInstance.CreateInstance (null, CL.Length (SLOTS_OF_STANDARD_CLASS));
            ((ManifestInstance) instance.Target).Class = instance;
            return instance;
        }

        static readonly StandardObject standardClass =
            bootstrapMakeStandardClass();

        //// Bootstrap step
        delegate object MapFunction (Cons arguments);

        static readonly Cons GettersAndSettersForClass =
            (Cons) CL.Map (QuoteList,
                           new MapFunction (delegate (Cons slotInfo)
        {
            Symbol name = (Symbol) CL.Car (slotInfo);
            Cons slotNames = (Cons) CL.Map (QuoteList, new MapFunction (CL.Car), SLOTS_OF_STANDARD_CLASS);
            int index = CL.Position (CL.Car (slotInfo), slotNames);
            return CL.List (name,
                            new SlotReader (delegate (StandardObject obj)
            {
                return StandardInstanceAccess (obj, index);
            }),
                            new SlotWriter (delegate (StandardObject obj, object newValue)
            {
                return SetStandardInstanceAccess (obj, index, newValue);
            }),
                            new SlotChanger (delegate (StandardObject obj, SlotChangingFunction func)
            {
                return StandardInstanceChange (obj, index, func);
            })
                            );
        }),
                           SLOTS_OF_STANDARD_CLASS);

        static object bootstrapStep0 =
           ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) (StandardClass, GettersAndSettersForClass);

        static SlotReader internalClassGettersAndSetters =
           (SlotReader) (CL.Cadr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)));

        static Cons InternalClassGettersAndSetters (StandardObject closClass)
        {
            return (Cons) internalClassGettersAndSetters (GuaranteeClass (closClass));
        }

        static object bootstrapStep0a =
            ((SlotWriter) (CL.Caddr (CL.Assq (QuoteName, GettersAndSettersForClass)))) (StandardClass, QuoteStandardClass);

        static readonly StandardObject top
            = (StandardObject) CL.MakeInstance (StandardClass,  // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (),
                            KW.Name, QuoteTop,
                            KW.StudlyName, "Top");

        static readonly StandardObject function
            = (StandardObject) CL.MakeInstance (StandardClass, // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteFunction,
                            KW.StudlyName, "Function");

        static readonly StandardObject standardObjectClass
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteStandardObject,
                            KW.StudlyName, "StandardObject");

        static readonly StandardObject metaobject
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass),
                            KW.Name, QuoteMetaobject,
                            KW.StudlyName, "MetaObject");

        static readonly StandardObject specializer
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Metaobject),
                            KW.Name, QuoteSpecializer,
                            KW.StudlyName, "Specializer");

        static readonly StandardObject closClass
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Specializer),
                            KW.Name, QuoteClass,
                            KW.StudlyName, "Class");

        static readonly StandardObject funcallableStandardClass
            = (StandardObject) CL.MakeInstance (StandardClass,
                    KW.DirectSuperclasses, CL.List (ClosClass),
                    KW.Name, QuoteFuncallableStandardClass,
                    KW.StudlyName, "FuncallableStandardClass");


        // Fixup the standard class
        static int bootstrapFixupStandardClass ()
        {
            internalSetClassPrecedenceList (standardClass, CL.List (ClosClass, Specializer, Metaobject, StandardObjectClass, Top));
            internalSetClassPrecedenceList (funcallableStandardClass, CL.List (ClosClass, Specializer, Metaobject, StandardObjectClass, Top));
            internalSetClassDirectSlots (standardClass, SLOTS_OF_STANDARD_CLASS);
            internalSetClassDirectSlots (funcallableStandardClass, SLOTS_OF_STANDARD_CLASS);
            internalSetClassDirectSuperclasses (standardClass, CL.List (ClosClass));
            internalSetClassDirectSuperclasses (funcallableStandardClass, CL.List (ClosClass));
            internalSetClassName (standardClass, QuoteStandardClass);
            internalSetClassStudlyName (standardClass, "StandardClass");
            internalSetClassStudlyName (funcallableStandardClass, "FuncallableStandardClass");
            internalSetClassSlots (standardClass, SLOTS_OF_STANDARD_CLASS);
            internalSetClassSlots (funcallableStandardClass, SLOTS_OF_STANDARD_CLASS);
            if (internalClassDirectSlots (standardClass) != SLOTS_OF_STANDARD_CLASS)
                throw new NotImplementedException ();
            return 1;
        }

        static int foo = bootstrapFixupStandardClass ();
        static object bootstrapStep1 =
            ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) 
               (funcallableStandardClass, GettersAndSettersForClass);


        static readonly StandardObject funcallableStandardObject =
           (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass), // Function),
                            KW.Name, QuoteFuncallableStandardObject,
                            KW.StudlyName, "FuncallableStandardObject");

        static readonly StandardObject genericFunction =
             (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                               KW.DirectSuperclasses, CL.List (Metaobject, FuncallableStandardObject),
                                               KW.Name, QuoteGenericFunction,
                                               KW.StudlyName, "GenericFunction");

        static readonly StandardObject standardGenericFunction =
                 (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                KW.DirectSlots, CL.List (CL.List (QuoteApplicationCache),
                                                         CL.List (QuoteLambdaList), 
                                                         CL.List (QuoteMethods),
                                                         CL.List (QuoteName),
                                                         CL.List (QuoteStudlyName),
                                                         CL.List (QuoteMethodClass),
                                                         CL.List (QuoteSingletonsList)),
                                KW.DirectSuperclasses, CL.List (GenericFunction),
                                KW.Name, QuoteStandardGenericFunction,
                                KW.StudlyName, "StandardGenericFunction");

 
        static readonly StandardObject method =
            (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                              KW.DirectSuperclasses, CL.List (Metaobject, FuncallableStandardObject),
                                              KW.Name, QuoteMethod,
                                              KW.StudlyName, "Method");

        static readonly StandardObject standardMethod =
            (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                              KW.DirectSlots, CL.List (
                                                                       CL.List (QuoteLambdaList),
                                                                       CL.List (QuoteName),
                                                                       CL.List (QuoteStudlyName),
                                                                       CL.List (QuoteProcedure),
                                                                       CL.List (QuoteQualifier),
                                                                       CL.List (QuoteSpecializers)),
                                              KW.DirectSuperclasses, CL.List (Method),
                                              KW.Name, QuoteStandardMethod,
                                              KW.StudlyName, "StandardMethod");

        static readonly StandardObject StandardAccessorMethod =
            (StandardObject) CL.MakeInstance (StandardClass,
                                       KW.DirectSlots, CL.List (),
                                       KW.DirectSuperclasses, CL.List (StandardMethod),
                                       KW.Name, QuoteStandardAccessorMethod,
                                       KW.StudlyName, "StandardAccessorMethod");

        static readonly StandardObject StandardReaderMethod =
            (StandardObject) CL.MakeInstance (StandardClass,
                               KW.DirectSlots, CL.List (),
                               KW.DirectSuperclasses, CL.List (StandardAccessorMethod),
                               KW.Name, QuoteStandardReaderMethod,
                               KW.StudlyName, "StandardReaderMethod");

        static readonly StandardObject StandardWriterMethod =
            (StandardObject) CL.MakeInstance (StandardClass,
                               KW.DirectSlots, CL.List (),
                               KW.DirectSuperclasses, CL.List (StandardAccessorMethod),
                               KW.Name, QuoteStandardWriterMethod,
                               KW.StudlyName, "StandardWriterMethod");

        static readonly StandardObject genericFunctionMethodClass =
            (StandardObject) CL.MakeInstance (standardGenericFunction,
                                              KW.MethodClass, StandardReaderMethod,
                                              KW.Name, QuoteGenericFunctionMethodClass,
                                              KW.StudlyName, "GenericFunctionMethodClass",
                                              KW.LambdaList, CL.List (QuoteGenericFunction));
        
        static readonly StandardObject allocateInstance =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteAllocateInstance,
                                              KW.StudlyName, "AllocateInstance",
                                              KW.LambdaList, CL.List (QuoteClass, QuoteAndRest, QuoteInitargs, QuoteAndKey, QuoteAndAllowOtherKeys));

        static readonly StandardObject computeApplicableMethods =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeApplicableMethods,
                                              KW.StudlyName, "ComputeApplicableMethods",
                                              KW.LambdaList, CL.List (QuoteGenericFunction, QuoteArguments));

        static readonly StandardObject computeDiscriminatingFunction =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeDiscriminatingFunction,
                                              KW.StudlyName, "ComputeDiscriminatingFunction",
                                              KW.LambdaList, CL.List (QuoteGenericFunction));

        static readonly StandardObject computeEffectiveMethod =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeEffectiveMethod,
                                              KW.StudlyName, "ComputeEffectiveMethod",
                                              KW.LambdaList, CL.List (QuoteComputeEffectiveMethod));

        static readonly StandardObject computeMethodMoreSpecific =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeMethodMoreSpecific,
                                              KW.StudlyName, "ComputeMethodMoreSpecific",
                                              KW.LambdaList, CL.List (QuoteGenericFunction));

        static readonly StandardObject addMethod =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteAddMethod,
                                              KW.StudlyName, "AddMethod",
                                              KW.LambdaList, CL.List (QuoteGenericFunction, QuoteMethod));

        static readonly List<StandardObject> GenericInvocationGenerics = 
            new List<StandardObject> {computeDiscriminatingFunction,
                                      computeApplicableMethods,
                                      computeEffectiveMethod,
                                      computeMethodMoreSpecific};

        static Cons GenericApplicationCacheTag = new Cons (null, null);

        static void extendSingletonsList (Cons tables, Cons specs)
        {
            if (specs != null) {
                if (internalIsSingleton (specs.Car)) {
                    throw new NotImplementedException ("extendSingletonsList");
                }
                extendSingletonsList ((Cons) tables.Cdr, (Cons) specs.Cdr);
            }
        }

        static bool IsShorter (Cons left, Cons right)
        {
            return (right != null)
                && ((left == null) || IsShorter ((Cons) left.Cdr,(Cons) right.Cdr));
        }

        static bool SameMethodSignatureLoop (Cons leftSpecs, Cons rightSpecs)
        {
            throw new NotImplementedException ();
        }

        static bool SameMethodSignature (StandardObject left, StandardObject right)
        {
            return Object.ReferenceEquals (MethodQualifier (left), MethodQualifier (right))
                && SameMethodSignatureLoop ((Cons) MethodSpecializers (left), (Cons) MethodSpecializers (right));
        }

        static Cons nFalses (int n)
        {
            if (n == 0)
                return null;
            else
                return new Cons (null, nFalses (n - 1));
        }

        static object bootstrapAddMethodImpl (StandardObject generic, StandardObject method)
        {
            Cons tables = InternalGenericSingletonsList (generic);
            Cons specs = InternalMethodSpecializers (method);
            Symbol qualifier =  InternalMethodQualifier (method);
            if (IsShorter (tables, specs)) {
                tables = (Cons) CL.Append (tables, nFalses (CL.Length (specs) - CL.Length (tables)));
                InternalSetGenericSingletonsList (generic, tables);
            }
            extendSingletonsList (specs, tables);
            if (GenericInvocationGenerics.Contains (generic)) {
                GenericApplicationCacheTag = new Cons (null, null);
                InternalSetGenericApplicationCache (generic, new Cons (GenericApplicationCacheTag, new MethodCache()));
            }
            InternalChangeGenericMethods (generic, (existingMethods) => new Cons (method, CL.RemoveIf (new Predicate (delegate (object existingMethod)
            {
                return SameMethodSignature ((StandardObject) existingMethod, method);
            }), existingMethods)));
            FuncallHandler df =(FuncallHandler) ComputeDiscriminatingFunction (generic);
            setFuncallableInstanceFunction (generic, df);
            return null;
        }

        static object bootstrapAddMethod (StandardObject self, object [] arguments)
        {
            if (arguments.Length != 2)
                throw new ArgumentException ("Wrong number of arguments to bootstrapAddMethod");
            return bootstrapAddMethodImpl (GuaranteeGeneric ((StandardObject) arguments[0]), GuaranteeMethod ((StandardObject) arguments[1]));
        }

        static object bootstrapGenericFunctionMethodClass (StandardObject self, object [] arguments)
        {
            return InternalGenericMethodClass ((StandardObject) arguments [0]);
        }

        static FuncallHandler bootstrapComputeDiscriminatingFunctionImpl (StandardObject generic)
        {
            StandardObject firstMethod = (StandardObject) CL.Car (InternalGenericMethods (generic));
            Delegate procedure = InternalMethodProcedure (firstMethod);
            FuncallHandler answer = (FuncallHandler) procedure.DynamicInvoke (null, new object [] {generic});
            return answer;
        }

        static FuncallHandler bootstrapComputeDiscriminatingFunction (StandardObject self, object [] arguments)
        {
            if (arguments.Length != 1)
                throw new ArgumentException ("Wrong number of arguments to bootstrapComputeDiscriminatingFunction");
            return bootstrapComputeDiscriminatingFunctionImpl (GuaranteeGeneric ((StandardObject) arguments [0]));
        }

        static object NullStandin = new Cons (null, null);

        static object last (object l)
        {
            Cons lp = l as Cons;
            if (lp.Cdr == null)
                return lp.Car;
            else
                return last (lp.Cdr);
        }

        static object [] PrePend (object element, object [] elements)
        {
            object [] result = new object [elements.Length + 1];
            elements.CopyTo (result, 1);
            result [0] = element;
            return result;
        }

        static FuncallHandler computeCallable (bool isGroundCase, StandardObject generic, MethodCache cache, Cons keys, object [] arguments)
        {
            FuncallHandler answer;
            if (isGroundCase) {
                Delegate m = InternalMethodProcedure ((StandardObject) last (InternalGenericMethods (generic)));
                answer = FuncallableWrapper.Create (m);
            }
            else {
                object [] newArguments = new object [arguments.Length + 1];
                arguments.CopyTo (newArguments, 1);
                newArguments [0] = generic;
                answer = (FuncallHandler) ComputeEffectiveMethod (generic, ComputeApplicableMethods (newArguments));
            }
            cache.Add (keys, answer);
            return answer;
        }

        static Cons getKeysLoop (object [] arguments, int argindex, object tables, Cons keys)
        {
            if ((tables == null) || (argindex >= arguments.Length))
                return keys;
            object thisTable = CL.Car (tables);
            object thisArg = arguments [argindex];
            object thisKey;
            MethodCache thisTable1 = thisTable as MethodCache;
            if (thisTable1 != null && thisTable1.ContainsKey (thisArg))
                thisKey = thisArg;
            else
                thisKey = ClassOf (thisArg).SerialNumber();
            return getKeysLoop (arguments, argindex + 1, CL.Cdr (tables), new Cons (thisKey, keys));
        }

        static Cons getKeys (object [] arguments, object singletons)
        {
            return (Cons) CL.Reverse (getKeysLoop (arguments, 0, singletons, null));
        }

        delegate object CDF (StandardObject generic, object [] arguments);

        static object realComputeDiscriminatingFunction (StandardObject generic, object [] arguments)
        {
            Cons appCache = InternalGenericApplicationCache (generic);
            Cons lambdaList = InternalGenericLambdaList (generic);
            Cons singletons =  InternalGenericSingletonsList (generic);
            Cons keys = getKeys (arguments, singletons);
            bool isGroundCase = GenericInvocationGenerics.Contains (generic)
                && arguments.Length > 0
                && GenericInvocationGenerics.Contains ((StandardObject) arguments [0]);
            // should do arity check here

            // check if we need to flush the cache
            if (!object.ReferenceEquals (appCache.Car, GenericApplicationCacheTag)) {
                appCache = new Cons (GenericApplicationCacheTag, new MethodCache());
                InternalSetGenericApplicationCache (generic, appCache);
            }

            MethodCache cache = (MethodCache) appCache.Cdr;
            FuncallHandler ah;
            if (!cache.TryGetValue (keys, out ah))
                ah = computeCallable (isGroundCase, generic, cache, keys, arguments);

            return ah.DynamicInvoke (generic, arguments);
        }

        delegate FuncallHandler ComputeDiscriminatingFunctionMethodSignature (NextMethodFunction callNextMethod, params object [] ignore);

        static FuncallHandler realMethodComputeDiscriminatingFunction (NextMethodFunction callNextMethod, params object [] ignore)
        {
            return new FuncallHandler (realComputeDiscriminatingFunction);
        }

        delegate bool MethodCompare2 (StandardObject left, StandardObject right);
        delegate bool MethodCompare3 (StandardObject left, StandardObject right, Cons arglist);

        static Cons SortMethods (Cons methodList, MethodCompare2 comp)
        {
            if (methodList == null)
                return null;
            if (methodList.Cdr == null)
                return methodList;
            else
                throw new NotImplementedException ("SortMethods");
        }

        static MethodCompare2 MakeSortPredicate (MethodCompare3 internalCompare, Cons arglist)
        {
            return new MethodCompare2 ((left, right) => internalCompare (left, right, arglist));
        }

        delegate bool MethodPredicate (StandardObject method);

        static Cons FilterMethods (MethodPredicate mp, Cons methodList)
        {
            if (methodList == null)
                return null;
            StandardObject thisMethod = GuaranteeMethod ((StandardObject) methodList.Car);
            if (mp (thisMethod))
                return new Cons (thisMethod, FilterMethods (mp, (Cons) methodList.Cdr));
            else
                return FilterMethods (mp, (Cons) methodList.Cdr);
        }

        delegate Cons ComputeApplicableMethodsMethodSignature (NextMethodFunction callNextMethod, StandardObject generic, params object [] suppliedArguments);
        static Cons realComputeApplicableMethods (NextMethodFunction callNextMethod, StandardObject generic, params object [] arguments)
        {
            Cons arglist = Cons.SubvectorToList (arguments, 0, arguments.Length);
            Cons methods = (Cons) internalGenericMethods (generic);
            Cons filteredMethods = FilterMethods ((m) => InstancesOf (arglist, (Cons) internalMethodSpecializers (m)), methods);
            Cons sortedMethods = SortMethods (filteredMethods,
                                              MakeSortPredicate ((MethodCompare3) ComputeMethodMoreSpecific (generic), arglist));
            return sortedMethods;
        }

        static bool realComputeMethodMoreSpecificLoop (StandardObject generic, StandardObject left, StandardObject right, Cons arglist)
        {
            throw new NotImplementedException ();
        }

        delegate MethodCompare3 ComputeMethodMoreSpecificMethodSignature (NextMethodFunction callNextMethod, StandardObject generic);
        static MethodCompare3 realComputeMethodMoreSpecific (NextMethodFunction callNextMethod, StandardObject generic)
        {
            return new MethodCompare3 ((left, right, arglist) => realComputeMethodMoreSpecificLoop (generic, left, right, arglist));
        }

        static FuncallHandler oneAroundStep (Cons methods, Cons args)
        {
            throw new NotImplementedException ();
        }

        delegate FuncallHandler methodStepper (methodStepper nextStep, Cons methodList, Cons args);

        static FuncallHandler oneStepper (methodStepper nextStep, Cons tail, Cons args)
        {
            return new FuncallHandler (delegate (StandardObject generic, object [] newargs) {
                return oneStepperGuts (nextStep, tail, args, Cons.SubvectorToList (newargs, 0, newargs.Length));
            });
        }

        static object noNextMethod (object [] args)
        {
            throw new NotImplementedException ("noNextMethod");
        }

        static NextMethodFunction makeNoNextMethod ()
        {
            return (NextMethodFunction) ((args) => noNextMethod(args));
        }

        static object ListRef (Cons args, int n)
        {
            if (args == null)
                throw new NotImplementedException ();
            if (n == 0)
                return args.Car;
            else
                return ListRef ((Cons) args.Cdr, n - 1);
        }

        static object [] ListToVector (Cons args)
        {
            object [] result = new object [CL.Length (args)];
            for (int i = 0; i < result.Length; i++) {
                result [i] = args.Car;
                args = (Cons) args.Cdr;
            }
            return result;
        }

        static object oneStepperGuts (methodStepper nextStep, Cons tail, Cons arglist, Cons newargs)
        {
            Cons args1 = newargs == null ? arglist : newargs;
            MethodStep func = (MethodStep) CL.Cdar (tail);
            NextMethodFunction a1 = tail.Cdr == null
                           ? makeNoNextMethod ()
                           : new NextMethodFunction (delegate (object [] args)
            {
                return nextStep (nextStep, (Cons) tail.Cdr, args1);
            });
            return func.DynamicInvoke (a1, ListToVector (args1));
         }

        delegate FuncallHandler ComputeEffectiveMethodMethodSignature (NextMethodFunction callNextMethod, StandardObject generic, Cons methodList);
        static FuncallHandler realComputeEffectiveMethod (NextMethodFunction callNextMethod, StandardObject generic, Cons methodList)
        {
            Cons primaryMethods = null;
            Cons aroundMethods = null;
            Cons beforeMethods = null;
            Cons afterMethods = null;

            methodStepper oneStep = oneStepper;

            while (methodList != null) {
                StandardObject method = GuaranteeMethod ((StandardObject) methodList.Car);
                methodList = (Cons) methodList.Cdr;
                Symbol q = (Symbol) internalMethodQualifier (method);
                if (q.Equals (KW.Primary)) {
                    primaryMethods = new Cons (new Cons (method, MethodStepWrapper.Create ((Delegate) internalMethodProcedure (method))), primaryMethods);
                }
                else
                    throw new NotImplementedException ();
            }
            primaryMethods = (Cons) CL.Reverse (primaryMethods);

            if (primaryMethods == null)
                throw new NotImplementedException ("no applicable method");
            else if (beforeMethods == null
                && afterMethods == null
                && aroundMethods == null)
                return oneStep (oneStep, primaryMethods, null);
            else
                return oneAroundStep (aroundMethods, null);
        }

        static StandardObject defaultInitargs =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteDefaultInitargs,
                                              KW.StudlyName, "DefaultInitargs",
                                              KW.LambdaList, CL.List (QuoteClass, QuoteSuppliedInitargs));

        delegate Cons DefaultInitargsMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass, Cons suppliedInitargs);
        static Cons methodDefaultInitargs (NextMethodFunction callNextMethod, StandardObject closClass, Cons suppliedInitargs)
        {
            // wrong!  fix later
            return suppliedInitargs;
        }

        static StandardObject initializeInstance =
	         (StandardObject) CL.MakeInstance (StandardGenericFunction,
					   KW.Name, QuoteInitializeInstance,
					   KW.StudlyName, "InitializeInstance",
					   KW.LambdaList, CL.List (QuoteInstance, QuoteAndRest, QuoteInitargs));

        static StandardObject sharedInitialize =
	         (StandardObject) CL.MakeInstance (StandardGenericFunction,
					   KW.Name, QuoteDefaultInitargs,
					   KW.StudlyName, "SharedInitialize",
					   KW.LambdaList, CL.List (QuoteInstance, QuoteSlotNames, QuoteAndRest, QuoteInitargs));

        static StandardObject methodQualifier =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.MethodClass, StandardReaderMethod,
                                              KW.Name, QuoteMethodQualifier,
                                              KW.StudlyName, "MethodQualifier",
                                              KW.LambdaList, CL.List (QuoteMethod));

        delegate object MethodQualifierMethodSignature (NextMethodFunction callNextMethod, StandardObject method);
        static object methodMethodQualifier (NextMethodFunction callNextMethod, StandardObject method)
        {
            return internalMethodQualifier (GuaranteeMethod (method));
        }

        static StandardObject methodSpecializers =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                      KW.MethodClass, StandardReaderMethod,
                                      KW.Name, QuoteMethodSpecializers,
                                      KW.StudlyName, "MethodSpecializers",
                                      KW.LambdaList, CL.List (QuoteMethod));

        delegate object MethodSpecializersMethodSignature (NextMethodFunction callNextMethod, StandardObject method);
        static object methodMethodSpecializers (NextMethodFunction callNextMethod, StandardObject method)
        {
            return internalMethodSpecializers (GuaranteeMethod (method));
        }

        delegate object AllocateInstanceMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass, Cons initargs);
        static object methodAllocateStandardInstance (NextMethodFunction callNextMethod, StandardObject closClass, Cons initargs)
        {
            throw new NotImplementedException ("methodAllocateInstance");
        }

        static object methodAllocateFuncallableInstance (NextMethodFunction callNextMethod, StandardObject closClass, Cons initargs)
        {
            throw new NotImplementedException ("methodAllocateInstance");
        }

        static bool bootstrapStep2Function ()
        {
            setFuncallableInstanceFunction (addMethod, bootstrapAddMethod);
            setFuncallableInstanceFunction (genericFunctionMethodClass, bootstrapGenericFunctionMethodClass);
            setFuncallableInstanceFunction (computeDiscriminatingFunction, bootstrapComputeDiscriminatingFunction);

            // Now we can a method to ComputeDiscriminatingFunction.
            CL.AddMethod (ComputeDiscriminatingFunction,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeDiscriminatingFunction),
                                           KW.Name, QuoteMethodComputeDiscriminatingFunction,
                                           KW.StudlyName, "Method:ComputeDiscriminatingFunction",
                                           KW.Procedure, new ComputeDiscriminatingFunctionMethodSignature (realMethodComputeDiscriminatingFunction),
                                           KW.Specializers, CL.List (StandardGenericFunction)));
     
            CL.AddMethod (ComputeApplicableMethods,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeApplicableMethods),
                                           KW.Name, QuoteMethodComputeApplicableMethods,
                                           KW.StudlyName, "Method:ComputeApplicableMethods",
                                           KW.Procedure, new ComputeApplicableMethodsMethodSignature (realComputeApplicableMethods),
                                           KW.Specializers, CL.List (StandardGenericFunction)));

            CL.AddMethod (ComputeEffectiveMethod,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeEffectiveMethod),
                                           KW.Name, QuoteMethodComputeEffectiveMethod,
                                           KW.StudlyName, "Method:ComputeEffectiveMethod",
                                           KW.Procedure, new ComputeEffectiveMethodMethodSignature (realComputeEffectiveMethod),
                                           KW.Specializers, CL.List (StandardGenericFunction)));

            CL.AddMethod (ComputeMethodMoreSpecific,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeMethodMoreSpecific),
                                           KW.Name, QuoteMethodComputeMethodMoreSpecific,
                                           KW.StudlyName, "Method:ComputeMethodMoreSpecific",
                                           KW.Procedure, new ComputeMethodMoreSpecificMethodSignature (realComputeMethodMoreSpecific),
                                           KW.Specializers, CL.List (StandardGenericFunction)));

            // Now we can add methods.
            CL.AddMethod (DefaultInitargs,
                          CL.MakeInstance (GenericFunctionMethodClass (DefaultInitargs),
                                           KW.Name, QuoteMethodDefaultInitargs,
                                           KW.StudlyName, "Method:DefaultInitargs",
                                           KW.Procedure, new DefaultInitargsMethodSignature (methodDefaultInitargs),
                                           KW.Specializers, CL.List (ClosClass)));  

            // Make sure we can inspect methods for duplicates when adding.
            CL.AddMethod (MethodQualifier,
                          CL.MakeInstance (GenericFunctionMethodClass (MethodQualifier),
                                           KW.Name, QuoteMethodMethodQualifier,
                                           KW.StudlyName, "Method:MethodQualifier",
                                           KW.Procedure, new MethodQualifierMethodSignature (methodMethodQualifier),
                                           KW.Specializers, CL.List (StandardMethod)));

            CL.AddMethod (MethodSpecializers,
              CL.MakeInstance (GenericFunctionMethodClass (MethodSpecializers),
                               KW.Name, QuoteMethodMethodSpecializers,
                               KW.StudlyName, "Method:MethodSpecializers",
                               KW.Procedure, new MethodSpecializersMethodSignature (methodMethodSpecializers),
                               KW.Specializers, CL.List (StandardMethod)));

	    // InitializeInstance default
	    // InitializeInstance StandardObject
            // InitializeInstance Class
            // InitializeInstance StandardGenericFunction
            // InitializeInstance StandardMethod

            CL.AddMethod (AllocateInstance,
                          CL.MakeInstance (GenericFunctionMethodClass (AllocateInstance),
                                           KW.Name, QuoteMethodAllocateInstance,
                                           KW.StudlyName, "Method:AllocateInstance",
                                           KW.Procedure, new AllocateInstanceMethodSignature (methodAllocateStandardInstance),
                                           KW.Specializers, CL.List (StandardClass)));

            CL.AddMethod (AllocateInstance,
                          CL.MakeInstance (GenericFunctionMethodClass (AllocateInstance),
                                           KW.Name, QuoteMethodAllocateInstance,
                                           KW.StudlyName, "Method:AllocateInstance",
                                           KW.Procedure, new AllocateInstanceMethodSignature (methodAllocateFuncallableInstance),
                                           KW.Specializers, CL.List (FuncallableStandardClass)));


            return true;
        }



        static readonly bool bootstrap_step2 = bootstrapStep2Function ();

        delegate object MakeInstanceMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass, params object [] initargs);
        static object makeInstanceMethod (NextMethodFunction callNextMethod, StandardObject closClass, params object [] initargs)
        {
            Cons initargsList = (Cons) DefaultInitargs (closClass, Cons.SubvectorToList (initargs, 0, initargs.Length));
            StandardObject instance = (StandardObject) CL.Apply (AllocateInstance, closClass, initargsList);
            CL.Apply (InitializeInstance, instance, initargsList);
            return instance;
        }

        static bool bootstrapStep3Function ()
        {
            // Fixup the generic for make.
            StandardObject newMakeInstance = (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                                                               KW.Name, QuoteMakeInstance,
                                                                               KW.StudlyName, "MakeInstance",
                                                                               KW.LambdaList, CL.List (QuoteClass, QuoteAndRest, QuoteInitargs));
            AddMethod (newMakeInstance,
                       CL.MakeInstance (StandardMethod,
                                        KW.Name, QuoteNewMakeInstanceMethod,
                                        KW.StudlyName, "Method:MakeInstance",
                                        KW.Specializers, CL.List (ClosClass),
                                        KW.Procedure, new MakeInstanceMethodSignature (makeInstanceMethod)));


            ManifestInstance oldMakeInstance = (ManifestInstance) CL.MakeInstance.Target;
            ManifestInstance newInstance = (ManifestInstance) newMakeInstance.Target;
            oldMakeInstance.Class = newInstance.Class;
            oldMakeInstance.Slots = newInstance.Slots;
            oldMakeInstance.OnFuncall = newInstance.OnFuncall;
            return true;
        }

        static readonly bool bootstrap_step3 = bootstrapStep3Function ();

        static readonly StandardObject ensureGenericFunction
                = (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                                    KW.Name, QuoteEnsureGenericFunction,
                                                    KW.LambdaList,               
                                                    CL.List (QuoteGenericFunction,
                                                          QuoteFunctionName,
                                                          QuoteAndKey,
                                                          QuoteArgumentPrecedenceOrder,
                                                          QuoteDeclarations,
                                                          QuoteDocumentation,
                                                          QuoteGenericFunctionClass,
                                                          QuoteLambdaList,
                                                          QuoteMethodClass,
                                                          QuoteMethodCombination,
                                                          QuoteName,
                                                          QuoteAndAllowOtherKeys));

        #region ExportedGenerics

        static public StandardObject AddMethod
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return addMethod;
            }
        }

        static public StandardObject AllocateInstance
        {
            [DebuggerStepThrough]
            get
            {
                return allocateInstance;
            }
        }

        static public StandardObject ClassName
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return className;
            }
        }

        static public StandardObject ClosClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return closClass;
            }
        }

        static public StandardObject ComputeApplicableMethods
        {
            [DebuggerStepThrough]
            get
            {
                return computeApplicableMethods;
            }
        }

        static public StandardObject ComputeDiscriminatingFunction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return computeDiscriminatingFunction;
            }
        }

        static public StandardObject ComputeMethodMoreSpecific
        {
            [DebuggerStepThrough]
            get
            {
                return computeMethodMoreSpecific;
            }
        }

        static public StandardObject ComputeEffectiveMethod
        {
            [DebuggerStepThrough]
            get
            {
                return computeEffectiveMethod;
            }
        }

        static public StandardObject DefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return defaultInitargs;
            }
        }

        static public StandardObject EnsureGenericFunction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return ensureGenericFunction;
            }
        }

        static public StandardObject FuncallableStandardClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return funcallableStandardClass;
            }
        }

        static public StandardObject FuncallableStandardObject
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return funcallableStandardObject;
            }
        }

        static public StandardObject Function
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return function;
            }
        }

        static public StandardObject GenericFunction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return genericFunction;
            }
        }

        static public StandardObject GenericFunctionName
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return genericFunctionName;
            }
        }

        static public StandardObject GenericFunctionMethodClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return genericFunctionMethodClass;
            }
        }

        static public StandardObject InitializeInstance
        {
            [DebuggerStepThrough]
            get
            {
                return initializeInstance;
            }
        }

        static public StandardObject MakeInstance
        {
//            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return makeInstance;
            }
        }

        static public StandardObject Metaobject
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return metaobject;
            }
        }

        static public StandardObject Method
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return method;
            }
        }

        static public StandardObject MethodQualifier
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return methodQualifier;
            }
        }

        static public StandardObject MethodSpecializers
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return methodSpecializers;
            }
        }

        static public StandardObject SharedInitialize
        {
            [DebuggerStepThrough]
            get
            {
                return sharedInitialize;
            }
        }

        static public StandardObject Specializer
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return specializer;
            }
        }

        static public StandardObject StandardClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return standardClass;
            }
        }

        static public StandardObject StandardGenericFunction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return standardGenericFunction;
            }
        }

        static public StandardObject StandardMethod
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return standardMethod;
            }
        }

        static object StandardInstanceAccess (StandardObject instance, int location)
        {
            return instance.InstanceRef (location);
        }

        static object SetStandardInstanceAccess (StandardObject instance, int location, object newValue)
        {
            return instance.InstanceSet (location, newValue);
        }

        static object StandardInstanceChange (StandardObject instance, int location, SlotChangingFunction change)
        {
            return instance.InstanceChange (location, change);
        }

        static public StandardObject StandardObjectClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return standardObjectClass;
            }
        }

        static public StandardObject Top
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return top;
            }
        }

        #endregion ExportedGenerics

        static public string PrintStandardObject (object thing)
        {
            return "A standard object";
        }

        static bool internalIsSingleton (object whatever)
        {
            return false;
        }

        static Symbol closSymbol (string name)
        {
            return closPackage.Intern (name);
        }
    }
}
