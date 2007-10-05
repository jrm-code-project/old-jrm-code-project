using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{

    public static class CLOS
    {
         // Extension methods for StandardObject
        static ManifestInstance Instance (this StandardObject obj)
        {
            return (ManifestInstance) obj.Target;
        }

        static StandardObject Class (this StandardObject obj)
        {
            return obj.Instance ().Class;
        }

        static void SetClass (this StandardObject obj, StandardObject newClass)
        {
            obj.Instance().Class = newClass;
        }

        static object [] Slots (this StandardObject obj)
        {
            throw new NotImplementedException ("Slots");
        }

        static void SetSlots (this StandardObject obj, object [] newSlots)
        {
            throw new NotImplementedException ("Slots");
        }

        static object InstanceRef (this StandardObject obj, int index)
        {
            return obj.Instance().Slots[index];
        }

        static object InstanceSet (this StandardObject obj, int index, object newValue)
        {
            obj.Instance().Slots [index] = newValue;
            return newValue;
        }

        static ApplyHandler OnApply (this StandardObject obj)
        {
            throw new NotImplementedException ("OnApply");
        }

        static void SetOnApply (this StandardObject obj, ApplyHandler newValue)
        {
            throw new NotImplementedException ("SetOnApply");
        }

        // End of extension methods

        static object applyStandardObject (ManifestInstance self, object [] arguments)
        {
            throw new NotImplementedException ("Attempt to apply non function " + self + " to " + arguments.ToString ());
        }

        static object applyUninitializedObject (ManifestInstance self, object [] arguments)
        {
            throw new NotImplementedException (self.ToString() + ": Attempt to apply uninitialized " + ((Symbol) CL.ClassName (self.Class)).Name + " to " + arguments.ToString ());
        }

        static StandardObject CreateInstance (StandardObject closClass, object [] slotVector)
        {
            StandardObject instance =
             (StandardObject) Delegate.CreateDelegate (typeof (StandardObject), new ManifestInstance (closClass, slotVector, applyStandardObject), typeof (ManifestInstance)
              .GetMethod ("DefaultInstanceMethod",
                          System.Reflection.BindingFlags.Instance
                          | System.Reflection.BindingFlags.NonPublic));
            return instance;
        }

        static StandardObject CreateFuncallableInstance (StandardObject closClass, object [] slotVector, ApplyHandler method)
        {
            StandardObject instance =
             (StandardObject) Delegate.CreateDelegate (typeof (StandardObject), new ManifestInstance(closClass, slotVector, method), typeof (ManifestInstance)
              .GetMethod ("DefaultInstanceMethod",
                          System.Reflection.BindingFlags.Instance
                          | System.Reflection.BindingFlags.NonPublic));
            return instance;
       }

        static readonly Package closPackage = Package.Clos;

        static readonly Symbol QuoteAndAllowOtherKeys = Package.CommonLisp.FindSymbol ("&ALLOW-OTHER-KEYS");
        static readonly Symbol QuoteAndKey = Package.CommonLisp.FindSymbol ("&KEY");
        static readonly Symbol QuoteAndRest = Package.CommonLisp.FindSymbol ("&REST");
        static readonly Symbol QuoteAnonymous = Package.CommonLisp.FindSymbol ("[unnamed]");
        static readonly Symbol QuoteAddMethod = Package.CommonLisp.FindSymbol ("ADD-METHOD");
        static readonly Symbol QuoteAllocateInstance = Package.CommonLisp.FindSymbol ("ALLOCATE-INSTANCE");
        static readonly Symbol QuoteArgumentPrecedenceOrder = closSymbol ("ARGUMENT-PRECEDENCE-ORDER");
        static readonly Symbol QuoteBuiltInClass = closSymbol ("BUILT-IN-CLASS");
        static readonly Symbol QuoteClassPrecedenceList = closSymbol ("CLASS-PRECEDENCE-LIST");
        static readonly Symbol QuoteClassPrototype = closSymbol ("CLASS-PROTOTYPE");
        static readonly Symbol QuoteClassSlots = closSymbol ("CLASS-SLOTS");
        static readonly Symbol QuoteClass = closSymbol ("CLASS");
        static readonly Symbol QuoteComputeDiscriminatingFunction = closSymbol ("COMPUTE-DISCRIMINATING-FUNCTION");
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
        static readonly Symbol QuoteInitargs = closSymbol ("INITARGS");
        static readonly Symbol QuoteLambdaList = closSymbol ("LAMBDA-LIST");
        static readonly Symbol QuoteList = Package.CommonLisp.FindSymbol ("LIST");
        static readonly Symbol QuoteMakeInstance = Package.CommonLisp.FindSymbol ("MAKE-INSTANCE");
        static readonly Symbol QuoteMetaobject = closSymbol ("METAOBJECT");
        static readonly Symbol QuoteMethod = closSymbol ("METHOD");
        static readonly Symbol QuoteMethodClass = closSymbol ("METHOD-CLASS");
        static readonly Symbol QuoteMethodCombination = closSymbol ("METHOD-COMBINATION");
        static readonly Symbol QuoteName = closSymbol ("NAME");
        static readonly Symbol QuoteStandardClass = closSymbol ("STANDARD-CLASS");
        static readonly Symbol QuoteSpecializer = closSymbol ("SPECIALIZER");
        static readonly Symbol QuoteStandardGenericFunction = closSymbol ("STANDARD-GENERIC-FUNCTION");
        static readonly Symbol QuoteStandardMethod = Package.CommonLisp.FindSymbol ("STANDARD-METHOD");
        static readonly Symbol QuoteStandardObject = closSymbol ("STANDARD-OBJECT");
        static readonly Symbol QuoteTop = closSymbol ("TOP");

        public delegate object SlotReader (StandardObject obj);
        delegate object SlotWriter (StandardObject obj, object newValue);
        delegate object SlotChanger (StandardObject obj, Delegate func);


        static void setFuncallableInstanceFunction (StandardObject funcallableInstance, ApplyHandler handler)
        {
            ((ManifestInstance) funcallableInstance.Target).OnApply = handler;
        }
                                                         

        // ClassOf
        delegate StandardObject ClassOfFunction (object obj);

        static ClassOfFunction classOf =
           new ClassOfFunction (delegate (object instance)
        {
            StandardObject probe = instance as StandardObject;
            return (probe == null)
              ? Top
              : ((ManifestInstance) probe.Target).Class;
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
            return bootstrapComputeClassPrecedenceList ((Cons) CL.Append (superclasses.Cdr, internalClassDirectSuperclasses ((StandardObject) superclasses.Car)),
                                                        CL.Memq (superclasses.Car, soFar)
                                                           ? soFar
                                                           : CL.Cons (superclasses.Car, soFar));
        }

        delegate Cons Allocator (object initargs);

        static object unspecificInitializer ()
        {
            throw new NotImplementedException ();
        }

        static object bootstrapMakeInstance (ManifestInstance make, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            Cons initargs = Cons.SubvectorToList (arguments, 1, arguments.Length);
            if (object.ReferenceEquals (closClass,StandardClass)
                || object.ReferenceEquals (closClass,FuncallableStandardClass)) {
                StandardObject instance = CreateInstance (closClass, new object [CL.Length (SLOTS_OF_STANDARD_CLASS)]);

                Cons directDefaultInitargs = (Cons) Utility.GetArg (initargs, KW.DirectDefaultInitargs, null);
                Cons directSlots = (Cons) Utility.GetArg (initargs, KW.DirectSlots, null);
                Cons directSuperclasses = (Cons) Utility.GetArg (initargs, KW.DirectSuperclasses, null);
                Symbol name = (Symbol) Utility.GetArg (initargs, KW.Name, QuoteAnonymous);
                Cons classPrecedenceList = bootstrapComputeClassPrecedenceList (directSuperclasses, CL.List (instance));
                Cons inheritedSlots = AppendMap (new amf (delegate (object superclass)
                {
                    return (Cons) internalClassDirectSlots ((StandardObject) superclass);
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
                             new SlotChanger (delegate (StandardObject o, Delegate modify)
                             {
                                 return SetStandardInstanceAccess (o, f, modify.DynamicInvoke (StandardInstanceAccess (o, f)));
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
                StandardObject answer = CreateFuncallableInstance (closClass, new object [CL.Length (internalClassDirectSlots (closClass))], applyUninitializedObject);
                internalSetGenericMethodClass (answer, Utility.GetArg (initargs, KW.MethodClass, StandardMethod));
                internalSetGenericName (answer, Utility.GetArg (initargs, KW.Name, QuoteAnonymous));
                internalSetGenericLambdaList (answer, lambdaList);
                return answer;
            }
            else if (object.ReferenceEquals (closClass, StandardMethod)) {
                throw new NotImplementedException ("make standard method");
            }
            else

            throw new NotImplementedException ("something else");
        }

        static readonly StandardObject makeInstance = CreateFuncallableInstance (null, null, bootstrapMakeInstance);

        static object bootstrapClassName (ManifestInstance className, object [] arguments)
        {
            return internalClassName ((StandardObject)(arguments [0]));
        }        

        static readonly StandardObject className = CreateFuncallableInstance (null, null, bootstrapClassName);

        static Cons lookupSlotInfo (StandardObject closClass, Symbol slotName)
        {
            Symbol className = null;
            object [] classSlots;
            Cons sng = (Cons) internalClassGettersAndSetters (closClass);
            Cons probe = CL.Assq (slotName, sng);
            if (probe == null) {
                className = (Symbol) internalClassName (closClass);
                classSlots = ((ManifestInstance) closClass.Target).Slots;
                throw new NotImplementedException ();
            }
            return probe;
        }

        static object internalSlotRef (StandardObject obj, Symbol slotName)
        {
            return ((SlotReader) (CL.Cadr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj);
        }

        static object internalSlotSet (StandardObject obj, Symbol slotName, object value)
        {
            return ((SlotWriter) (CL.Caddr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj, value);
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

        static SlotWriter internalSetClassName =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteName, newValue);
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

        static SlotReader internalGenericMethodClass =
            new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteMethodClass);
        });

        static SlotWriter internalSetGenericMethodClass =
    new SlotWriter (delegate (StandardObject generic, object newValue)
    {
        return internalSlotSet (generic, QuoteMethodClass, newValue);
    });


        static SlotWriter internalSetGenericName =
            new SlotWriter (delegate (StandardObject generic, object newValue)
  {
      return internalSlotSet (generic, QuoteName, newValue);
  });

        static SlotWriter internalSetGenericLambdaList =
            new SlotWriter (delegate (StandardObject obj, object newValue)
            {
                return internalSlotSet (obj, QuoteLambdaList, newValue);
            });


        static Cons SLOTS_OF_STANDARD_CLASS =
              CL.List (CL.List (QuoteDefaultInitargs),
                       CL.List (QuoteDirectDefaultInitargs),
                       CL.List (QuoteDirectSlots),
                       CL.List (QuoteDirectSubclasses),
                       CL.List (QuoteDirectSuperclasses),
                       CL.List (QuoteFinalizedP),
                       CL.List (QuoteName),
                       CL.List (QuoteClassPrecedenceList),
                       CL.List (QuoteClassPrototype),
                       CL.List (QuoteGettersAndSetters),
                       CL.List (QuoteClassSlots));

        static private StandardObject bootstrapMakeStandardClass ()
        {
            //ManifestInstance manifestInstance = new ManifestInstance (CL.Length (SLOTS_OF_STANDARD_CLASS));
            //StandardObject instance =
            //   (StandardObject) Delegate.CreateDelegate (typeof (StandardObject),
            //                                             manifestInstance,
            //                                             defaultInstanceMethod);
            // manifestInstance.Class = instance;
            StandardObject instance = CreateInstance (null, new object [CL.Length (SLOTS_OF_STANDARD_CLASS)]);
            ((ManifestInstance) instance.Target).Class = instance;
            return instance;
        }

        static readonly StandardObject standardClass
                = bootstrapMakeStandardClass ();

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
                            new SlotChanger (delegate (StandardObject obj, Delegate func)
            {
                return SetStandardInstanceAccess (obj, index, func.DynamicInvoke (StandardInstanceAccess (obj, index)));
            })
                            );
        }),
                           SLOTS_OF_STANDARD_CLASS);

        static object bootstrapStep0 =
           ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) (StandardClass, GettersAndSettersForClass);

        static SlotReader internalClassGettersAndSetters =
           (SlotReader) (CL.Cadr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)));

        static object bootstrapStep0a =
            ((SlotWriter) (CL.Caddr (CL.Assq (QuoteName, GettersAndSettersForClass)))) (StandardClass, QuoteStandardClass);

        static readonly StandardObject top
            = (StandardObject) CL.MakeInstance (StandardClass,  // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (),
                            KW.Name, QuoteTop);

        static readonly StandardObject function
            = (StandardObject) CL.MakeInstance (StandardClass, // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteFunction);

        static readonly StandardObject standardObjectClass
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteStandardObject);

        static readonly StandardObject metaobject
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass),
                            KW.Name, QuoteMetaobject);

        static readonly StandardObject specializer
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Metaobject),
                            KW.Name, QuoteSpecializer);

        static readonly StandardObject closClass
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Specializer),
                            KW.Name, QuoteClass);

        static readonly StandardObject funcallableStandardClass
    = (StandardObject) CL.MakeInstance (StandardClass,
                    KW.DirectSuperclasses, CL.List (ClosClass),
                    KW.Name, QuoteFuncallableStandardClass);




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
            internalSetClassSlots (standardClass, SLOTS_OF_STANDARD_CLASS);
            internalSetClassSlots (funcallableStandardClass, SLOTS_OF_STANDARD_CLASS);
            if (internalClassDirectSlots (standardClass) != SLOTS_OF_STANDARD_CLASS)
                throw new NotImplementedException ();
            return 1;
        }

        static int foo = bootstrapFixupStandardClass ();
        static object bootstrapStep1 =
   ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) (funcallableStandardClass, GettersAndSettersForClass);


        static StandardObject bootstrapMakeBuiltInClass ()
        {
            StandardObject builtInClass = (StandardObject) CL.MakeInstance (StandardClass,
                                                           KW.DirectSuperclasses, CL.List (ClosClass),
                                                           KW.Name, QuoteBuiltInClass);
            // fixup Top and Function
            // Hard to do.  I need to determine the class slots of builtInClass
            // and bootstrap them.
            //((ManifestInstance)(Top.Target)).Class = builtInClass;
            //  ((ManifestInstance)(Function.Target)).Class = builtInClass;
            return builtInClass;
        }

        static readonly StandardObject builtInClass = 
bootstrapMakeBuiltInClass ();


        static readonly StandardObject funcallableStandardObject =
        (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass), // Function),
                            KW.Name, QuoteFuncallableStandardObject);

        static readonly StandardObject genericFunction
            = (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                               KW.DirectSuperclasses, CL.List (Metaobject, FuncallableStandardObject),
                                               KW.Name, QuoteGenericFunction);

        static readonly StandardObject standardGenericFunction
                = (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                KW.DirectSlots, CL.List (CL.List (QuoteName),
                                                         CL.List (QuoteLambdaList),
                                                         CL.List (QuoteMethodClass)),
                                KW.DirectSuperclasses, CL.List (GenericFunction),
                                KW.Name, QuoteStandardGenericFunction);

        static readonly StandardObject genericFunctionMethodClass =
            (StandardObject) CL.MakeInstance (standardGenericFunction,
                                              KW.Name, QuoteGenericFunctionMethodClass,
                                              KW.LambdaList, CL.List (QuoteGenericFunction));

        static readonly StandardObject method =
            (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                              KW.DirectSuperclasses, CL.List (Metaobject, FuncallableStandardObject),
                                              KW.Name, QuoteMethod);

        static readonly StandardObject standardMethod =
            (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                              KW.DirectSlots, CL.List (CL.List (QuoteName),
                                                                       CL.List (QuoteLambdaList)),
                                              KW.DirectSuperclasses, CL.List (Method),
                                              KW.Name, QuoteStandardMethod);

        static readonly StandardObject allocateInstance =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteAllocateInstance,
                                              KW.LambdaList, CL.List (QuoteClass, QuoteAndRest, QuoteInitargs, QuoteAndKey, QuoteAndAllowOtherKeys));

        static readonly StandardObject computeDiscriminatingFunction =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeDiscriminatingFunction,
                                              KW.LambdaList, CL.List (QuoteGenericFunction));

        static readonly StandardObject addMethod =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteAddMethod,
                                              KW.LambdaList, CL.List (QuoteGenericFunction, QuoteMethod));

        static object bootstrapGenericFunctionMethodClass (ManifestInstance me, object [] arguments)
        {
            return internalGenericMethodClass ((StandardObject) (arguments [0]));
        }

        static object bootstrapComputeDiscriminatingFunction (ManifestInstance generic, object [] arguments)
        {
            throw new NotImplementedException ("bootstrap compute discriminating function");
        }


        static bool bootstrapStep2Function ()
        {
            setFuncallableInstanceFunction (genericFunctionMethodClass, bootstrapGenericFunctionMethodClass);
            setFuncallableInstanceFunction (computeDiscriminatingFunction, bootstrapComputeDiscriminatingFunction);
            // Now we can add methods.
            CL.AddMethod (ComputeDiscriminatingFunction,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeDiscriminatingFunction)));
            return true;
        }


        static readonly bool bootstrap_step2 = bootstrapStep2Function ();


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

        static public StandardObject AddMethod
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return addMethod;
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

        static public StandardObject ComputeDiscriminatingFunction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return computeDiscriminatingFunction;
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

        static public StandardObject GenericFunctionMethodClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return genericFunctionMethodClass;
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

        static public string PrintStandardObject (object thing)
        {
            return "A standard object";
        }

        static Symbol closSymbol (string name)
        {
            return closPackage.Intern (name);
        }
    }
}
