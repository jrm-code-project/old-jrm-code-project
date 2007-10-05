// Metadata version: v2.0.50727
.assembly extern /*23000001*/ mscorlib
{
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 )                         // .z\V.4..
  .ver 2:0:0:0
}
.assembly /*20000001*/ Jrm.StandardObject
{
  .custom /*0C000001:0A00000D*/ instance void [mscorlib/*23000001*/]System.Reflection.AssemblyFileVersionAttribute/*0100001D*/::.ctor(string) /* 0A00000D */ = ( 01 00 07 31 2E 30 2E 30 2E 30 00 00 )             // ...1.0.0.0..
  .custom /*0C000002:0A00000E*/ instance void [mscorlib/*23000001*/]System.CLSCompliantAttribute/*0100001E*/::.ctor(bool) /* 0A00000E */ = ( 01 00 01 00 00 ) 

  // --- The following custom attribute is added automatically, do not uncomment -------
  //  .custom /*0C000003:0A00000F*/ instance void [mscorlib/*23000001*/]System.Diagnostics.DebuggableAttribute/*0100001F*/::.ctor(valuetype [mscorlib/*23000001*/]System.Diagnostics.DebuggableAttribute/*0100001F*//DebuggingModes/*01000020*/) /* 0A00000F */ = ( 01 00 07 01 00 00 00 00 ) 

  .custom /*0C000004:0A00000B*/ instance void [mscorlib/*23000001*/]System.Runtime.InteropServices.GuidAttribute/*0100001B*/::.ctor(string) /* 0A00000B */ = ( 01 00 24 32 36 36 35 32 31 65 39 2D 66 63 63 38   // ..$266521e9-fcc8
                                                                                                                                                               2D 34 64 66 65 2D 39 38 61 35 2D 32 30 65 64 36   // -4dfe-98a5-20ed6
                                                                                                                                                               31 36 31 30 34 64 37 00 00 )                      // 16104d7..
  .custom /*0C000005:0A000008*/ instance void [mscorlib/*23000001*/]System.Reflection.AssemblyTrademarkAttribute/*01000018*/::.ctor(string) /* 0A000008 */ = ( 01 00 00 00 00 ) 
  .custom /*0C000006:0A000002*/ instance void [mscorlib/*23000001*/]System.Reflection.AssemblyTitleAttribute/*01000012*/::.ctor(string) /* 0A000002 */ = ( 01 00 08 4A 72 6D 2E 4C 69 73 70 00 00 )          // ...Jrm.Lisp..
  .custom /*0C000007:0A000010*/ instance void [mscorlib/*23000001*/]System.Runtime.CompilerServices.CompilationRelaxationsAttribute/*01000021*/::.ctor(int32) /* 0A000010 */ = ( 01 00 08 00 00 00 00 00 ) 
  .custom /*0C000008:0A000011*/ instance void [mscorlib/*23000001*/]System.Runtime.CompilerServices.RuntimeCompatibilityAttribute/*01000022*/::.ctor() /* 0A000011 */ = ( 01 00 01 00 54 02 16 57 72 61 70 4E 6F 6E 45 78   // ....T..WrapNonEx
                                                                                                                                                                          63 65 70 74 69 6F 6E 54 68 72 6F 77 73 01 )       // ceptionThrows.
  .custom /*0C000009:0A00000A*/ instance void [mscorlib/*23000001*/]System.Runtime.InteropServices.ComVisibleAttribute/*0100001A*/::.ctor(bool) /* 0A00000A */ = ( 01 00 00 00 00 ) 
  .custom /*0C00000A:0A000004*/ instance void [mscorlib/*23000001*/]System.Reflection.AssemblyConfigurationAttribute/*01000014*/::.ctor(string) /* 0A000004 */ = ( 01 00 00 00 00 ) 
  .custom /*0C00000B:0A000003*/ instance void [mscorlib/*23000001*/]System.Reflection.AssemblyDescriptionAttribute/*01000013*/::.ctor(string) /* 0A000003 */ = ( 01 00 00 00 00 ) 
  .custom /*0C00000C:0A000005*/ instance void [mscorlib/*23000001*/]System.Reflection.AssemblyCompanyAttribute/*01000015*/::.ctor(string) /* 0A000005 */ = ( 01 00 13 41 6C 62 61 74 72 6F 73 73 20 56 61 70   // ...Albatross Vap
                                                                                                                                                             6F 72 77 61 72 65 00 00 )                         // orware..
  .custom /*0C00000D:0A000006*/ instance void [mscorlib/*23000001*/]System.Reflection.AssemblyProductAttribute/*01000016*/::.ctor(string) /* 0A000006 */ = ( 01 00 08 4A 72 6D 2E 4C 69 73 70 00 00 )          // ...Jrm.Lisp..
  .custom /*0C00000E:0A000007*/ instance void [mscorlib/*23000001*/]System.Reflection.AssemblyCopyrightAttribute/*01000017*/::.ctor(string) /* 0A000007 */ = ( 01 00 1E 43 6F 70 79 72 69 67 68 74 20 C2 A9 20   // ...Copyright .. 
                                                                                                                                                               4A 6F 65 20 4D 61 72 73 68 61 6C 6C 20 32 30 30   // Joe Marshall 200
                                                                                                                                                               37 00 00 )                                        // 7..
}
.module Jrm.StandardObject.dll

.class /*02000003*/ public auto ansi sealed Lisp.ApplyHandler
       extends [mscorlib/*23000001*/]System.MulticastDelegate/*01000001*/
{
  .method /*06000005*/ public hidebysig specialname rtspecialname 
          instance void  .ctor(object 'object',
                               native int 'method') runtime managed
  // SIG: 20 02 01 1C 18
  {
  } // end of method ApplyHandler::.ctor

  .method /*06000006*/ public hidebysig newslot virtual 
          instance object  Invoke(class Lisp.StandardObject/*02000004*/ function,
                                  object[] arguments) runtime managed
  // SIG: 20 02 1C 12 10 1D 1C
  {
  } // end of method ApplyHandler::Invoke

  .method /*06000007*/ public hidebysig newslot virtual 
          instance class [mscorlib/*23000001*/]System.IAsyncResult/*01000003*/ 
          BeginInvoke(class Lisp.StandardObject/*02000004*/ function,
                      object[] arguments,
                      class [mscorlib/*23000001*/]System.AsyncCallback/*01000004*/ callback,
                      object 'object') runtime managed
  // SIG: 20 04 12 0D 12 10 1D 1C 12 11 1C
  {
  } // end of method ApplyHandler::BeginInvoke

  .method /*06000008*/ public hidebysig newslot virtual 
          instance object  EndInvoke(class [mscorlib/*23000001*/]System.IAsyncResult/*01000003*/ result) runtime managed
  // SIG: 20 01 1C 12 0D
  {
  } // end of method ApplyHandler::EndInvoke

} // end of class Lisp.ApplyHandler

.class /*02000026*/ public auto ansi sealed Lisp.StandardObject
       extends [mscorlib/*23000001*/]System.MulticastDelegate/*01000001*/
{
  .field /*0400003F*/ private class Lisp.StandardObject/*02000026*/ closClass
  .field /*04000040*/ private object[] slots
  .field /*04000001*/ private class Lisp.ApplyHandler/*02000003*/ onApply

  .method /*0600007B*/ public hidebysig specialname rtspecialname 
          instance void  .ctor(object 'object',
                               native int 'method') runtime managed
  // SIG: 20 02 01 1C 18
  {
  } // end of method StandardObject::.ctor

  .method /*0600007C*/ public hidebysig newslot virtual 
          instance object  Invoke(object[] arguments) runtime managed
  // SIG: 20 01 1C 1D 1C
  {
    .param [1]/*0800006F*/ 
    .custom /*0C00001B:0A000001*/ instance void [mscorlib/*23000001*/]System.ParamArrayAttribute/*01000011*/::.ctor() /* 0A000001 */ = ( 01 00 00 00 ) 
  } // end of method StandardObject::Invoke

  .method /*0600007D*/ public hidebysig newslot virtual 
          instance class [mscorlib/*23000001*/]System.IAsyncResult/*01000005*/ 
          BeginInvoke(object[] arguments,
                      class [mscorlib/*23000001*/]System.AsyncCallback/*01000006*/ callback,
                      object 'object') runtime managed
  // SIG: 20 03 12 15 1D 1C 12 19 1C
  {
  } // end of method StandardObject::BeginInvoke

  .method /*0600007E*/ public hidebysig newslot virtual 
          instance object  EndInvoke(class [mscorlib/*23000001*/]System.IAsyncResult/*01000005*/ result) runtime managed
  // SIG: 20 01 1C 12 15
  {
  } // end of method StandardObject::EndInvoke

  .property instance class Lisp.StandardObject Class()
  {
    .get instance class Lisp.StandardObject Lisp.StandardObject::get_Class()
    .set instance void Lisp.StandardObject::set_Class(class Lisp.StandardObject 'closClass')
  }

  .method /*06000080*/ public hidebysig specialname 
          instance class Lisp.StandardObject/*0100000D*/ 
          get_Class() cil managed
  // SIG: 20 00 12 35
  {
    .custom /*0C000021:0A000028*/ instance void [mscorlib/*23000001*/]System.Diagnostics.DebuggerStepThroughAttribute/*01000029*/::.ctor() /* 0A000028 */ = ( 01 00 00 00 ) 
    // Method begins at RVA 0x9d48
    // Code size       12 (0xc)
    .maxstack  1
    .locals /*11000026*/ init ([0] class Lisp.StandardObject/*0100000D*/ CS$1$0000)
    IL_0000:  /* 00   |                  */ nop
    IL_0001:  /* 02   |                  */ ldarg.0
    IL_0002:  /* 7B   | (04)00003F       */ ldfld      class Lisp.StandardObject/*0100000D*/ Lisp.StandardObject/*02000026*/::closClass /* 0400003F */
    IL_0007:  /* 0A   |                  */ stloc.0
    IL_0008:  /* 2B   | 00               */ br.s       IL_000a

    IL_000a:  /* 06   |                  */ ldloc.0
    IL_000b:  /* 2A   |                  */ ret
  } // end of method StandardObject::get_Class

  .method /*06000081*/ public hidebysig specialname 
          instance void  set_Class(class Lisp.StandardObject/*0100000D*/ 'value') cil managed
  // SIG: 20 01 01 12 35
  {
    .custom /*0C000022:0A000028*/ instance void [mscorlib/*23000001*/]System.Diagnostics.DebuggerStepThroughAttribute/*01000029*/::.ctor() /* 0A000028 */ = ( 01 00 00 00 ) 
    // Method begins at RVA 0x9d60
    // Code size       9 (0x9)
    .maxstack  8
    IL_0000:  /* 00   |                  */ nop
    IL_0001:  /* 02   |                  */ ldarg.0
    IL_0002:  /* 03   |                  */ ldarg.1
    IL_0003:  /* 7D   | (04)00003F       */ stfld      class Lisp.StandardObject/*0100000D*/ Lisp.StandardObject/*02000026*/::closClass /* 0400003F */
    IL_0008:  /* 2A   |                  */ ret
  } // end of method StandardObject::set_Class

  .property instance object [] Slots()
  {
    .get instance object [] Lisp.StandardObject::get_Slots()
    .set instance void Lisp.StandardObject::set_Slots(object [] 'slots')
  }

  .method /*06000083*/ public hidebysig specialname 
          instance object[]  get_Slots() cil managed
  // SIG: 20 00 1D 1C
  {
    .custom /*0C000024:0A000028*/ instance void [mscorlib/*23000001*/]System.Diagnostics.DebuggerStepThroughAttribute/*01000029*/::.ctor() /* 0A000028 */ = ( 01 00 00 00 ) 
    // Method begins at RVA 0x9d84
    // Code size       12 (0xc)
    .maxstack  1
    .locals /*11000002*/ init ([0] object[] CS$1$0000)
    IL_0000:  /* 00   |                  */ nop
    IL_0001:  /* 02   |                  */ ldarg.0
    IL_0002:  /* 7B   | (04)000040       */ ldfld      object[] Lisp.StandardObject/*02000026*/::slots /* 04000040 */
    IL_0007:  /* 0A   |                  */ stloc.0
    IL_0008:  /* 2B   | 00               */ br.s       IL_000a

    IL_000a:  /* 06   |                  */ ldloc.0
    IL_000b:  /* 2A   |                  */ ret
  } // end of method StandardObject::get_Slots

  .method /*06000084*/ public hidebysig specialname 
          instance void  set_Slots(object[] 'value') cil managed
  // SIG: 20 01 01 1D 1C
  {
    .custom /*0C000025:0A000028*/ instance void [mscorlib/*23000001*/]System.Diagnostics.DebuggerStepThroughAttribute/*01000029*/::.ctor() /* 0A000028 */ = ( 01 00 00 00 ) 
    // Method begins at RVA 0x9d9c
    // Code size       9 (0x9)
    .maxstack  8
    IL_0000:  /* 00   |                  */ nop
    IL_0001:  /* 02   |                  */ ldarg.0
    IL_0002:  /* 03   |                  */ ldarg.1
    IL_0003:  /* 7D   | (04)000040       */ stfld      object[] Lisp.StandardObject/*02000026*/::slots /* 04000040 */
    IL_0008:  /* 2A   |                  */ ret
  } // end of method StandardObject::set_Slots

  .method /*0600000B*/ private hidebysig static 
          object  defaultInstanceMethod(class Lisp.StandardObject/*02000004*/ that,
                                  object[] arguments) cil managed
  // SIG: 00 02 1C 12 10 1D 1C
  {
    // Method begins at RVA 0x20e1
    // Code size       14 (0xe)
    .maxstack  8
    IL_0000:  /* 02   |                  */ ldarg.0
    IL_0001:  /* 7B   | (04)000001       */ ldfld      class Lisp.ApplyHandler/*02000003*/ Lisp.StandardObject/*02000004*/::onApply /* 04000001 */
    IL_0006:  /* 02   |                  */ ldarg.0
    IL_0007:  /* 03   |                  */ ldarg.1
    IL_0008:  /* 6F   | (06)000006       */ callvirt   instance object Lisp.ApplyHandler/*02000003*/::Invoke(class Lisp.StandardObject/*02000004*/,
                                                                                                                 object[]) /* 06000006 */
    IL_000d:  /* 2A   |                  */ ret
  } // end of method StandardObject::defaultInstanceMethod

  .method /*06000085*/ public hidebysig instance object 
          InstanceRef(int32 index) cil managed
  // SIG: 20 01 1C 08
  {
    // Method begins at RVA 0x9da8
    // Code size       14 (0xe)
    .maxstack  2
    .locals /*11000004*/ init ([0] object CS$1$0000)
    IL_0000:  /* 00   |                  */ nop
    IL_0001:  /* 02   |                  */ ldarg.0
    IL_0002:  /* 7B   | (04)000040       */ ldfld      object[] Lisp.StandardObject/*02000026*/::slots /* 04000040 */
    IL_0007:  /* 03   |                  */ ldarg.1
    IL_0008:  /* 9A   |                  */ ldelem.ref
    IL_0009:  /* 0A   |                  */ stloc.0
    IL_000a:  /* 2B   | 00               */ br.s       IL_000c

    IL_000c:  /* 06   |                  */ ldloc.0
    IL_000d:  /* 2A   |                  */ ret
  } // end of method StandardObject::InstanceRef

  .method /*06000086*/ public hidebysig instance object 
          InstanceSet(int32 index,
                      object newValue) cil managed
  // SIG: 20 02 1C 08 1C
  {
    // Method begins at RVA 0x9dc4
    // Code size       16 (0x10)
    .maxstack  3
    .locals /*11000004*/ init ([0] object CS$1$0000)
    IL_0000:  /* 00   |                  */ nop
    IL_0001:  /* 02   |                  */ ldarg.0
    IL_0002:  /* 7B   | (04)000040       */ ldfld      object[] Lisp.StandardObject/*02000026*/::slots /* 04000040 */
    IL_0007:  /* 03   |                  */ ldarg.1
    IL_0008:  /* 04   |                  */ ldarg.2
    IL_0009:  /* A2   |                  */ stelem.ref
    IL_000a:  /* 04   |                  */ ldarg.2
    IL_000b:  /* 0A   |                  */ stloc.0
    IL_000c:  /* 2B   | 00               */ br.s       IL_000e

    IL_000e:  /* 06   |                  */ ldloc.0
    IL_000f:  /* 2A   |                  */ ret
  } // end of method StandardObject::InstanceSet

  .property /*17000001*/ instance class Lisp.ApplyHandler/*02000003*/
          OnApply()
  {
    .set instance void Lisp.StandardObject/*02000004*/::set_OnApply(class Lisp.ApplyHandler/*02000003*/) /* 06000009 */
  } // end of property StandardObject::OnApply

  .method /*06000009*/ public hidebysig specialname 
          instance void  set_OnApply(class Lisp.ApplyHandler/*02000003*/ 'value') cil managed
  // SIG: 20 01 01 12 0C
  {
    // Method begins at RVA 0x20d0
    // Code size       8 (0x8)
    .maxstack  8
    IL_0000:  /* 02   |                  */ ldarg.0
    IL_0001:  /* 03   |                  */ ldarg.1
    IL_0002:  /* 7D   | (04)000001       */ stfld      class Lisp.ApplyHandler/*02000003*/ Lisp.StandardObject/*02000004*/::onApply /* 04000001 */
    IL_0007:  /* 2A   |                  */ ret
  } // end of method StandardObject::set_OnApply

} // end of class Lisp.StandardObject
