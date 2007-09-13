using System;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;

namespace LScript
{

    [ClassInterface (ClassInterfaceType.AutoDispatch),
     ComVisible(true),
     Guid("62416980-8B84-4c51-A09C-AB9623C3CC3E"),
     ProgId ("LScript")]
    public class LScript : IActiveScript, IActiveScriptParse
    {
        System.Diagnostics.TraceListener traceListener = new System.Diagnostics.TextWriterTraceListener (Console.Out);
        IActiveScriptSite site;
        ScriptState currentScriptState = ScriptState.Uninitialized;

        public LScript ()
        {
            System.Diagnostics.Debug.Listeners.Add (traceListener);
        }

         ~LScript ()
        {
            System.Diagnostics.Debug.WriteLine ("Gone!");
            System.Diagnostics.Debug.Listeners.Remove (traceListener);
        }

        public void SetScriptSite (IActiveScriptSite site)
        {
            System.Diagnostics.Debug.WriteLine ("SetScriptSite()");
            this.site = site;
        }

        public void GetScriptSite (ref Guid riid, out IntPtr site)
        {
            System.Diagnostics.Debug.WriteLine ("GetScriptSite()");
            site = new IntPtr (0);
        }

        public void SetScriptState (ScriptState state)
        {
            System.Diagnostics.Debug.WriteLine ("SetScriptState(" + state.ToString() + ")");

            this.currentScriptState =  state;
             site.OnStateChange (this.currentScriptState);  
        }

        public void GetScriptState (out ScriptState scriptState)
        {
            System.Diagnostics.Debug.WriteLine ("GetScriptState()");
            scriptState = this.currentScriptState;
        }

        public void Close ()
        {
            System.Diagnostics.Debug.WriteLine ("Close()");
        }

        Type comObjectType (IDispatch obj)
        {
            ITypeInfo tinfo = obj.GetTypeInfo (0, 0x409);
            return Marshal.GetTypeForITypeInfo (Marshal.GetIUnknownForObject (tinfo));
        }

        public void AddNamedItem (string name, ScriptItem flags)
        {
            System.Diagnostics.Debug.WriteLine ("AddNamedItem(\"" + name + "\", " + flags.ToString () + ")");
            // What did he give us?
            object item;
            object typeinfo;
            if (this.site != null) {
                try {
                    this.site.GetItemInfo (name, ScriptInfo.IUnknown, out item, out typeinfo);
                    System.Diagnostics.Debug.WriteLine ("item is " + item.ToString ());
                    Type type = comObjectType (item as IDispatch);
                        System.Diagnostics.Debug.WriteLine ("Type is " + type.ToString());
                        foreach (System.Reflection.MemberInfo minfo in type.GetMembers ())
                            System.Diagnostics.Debug.WriteLine (minfo);
                }
                catch (Exception e) {
                    System.Diagnostics.Debug.WriteLine ("Exception " + e.ToString ());
                }
            }
            else {
                // I guess we can find out later.
                System.Diagnostics.Debug.WriteLine ("No site, but named items being added.");
            }
        }

        public void AddTypeLib (ref Guid typeLib, uint major, uint minor, uint flags)
        {
            System.Diagnostics.Debug.WriteLine ("AddTypeLib()");
        }

        public void GetScriptDispatch (string itemName, out object dispatch)
        {
            System.Diagnostics.Debug.WriteLine ("GetScriptDispatch()");
            dispatch = null;
        }
        public void GetCurrentScriptThreadID (out uint thread)
        {
            System.Diagnostics.Debug.WriteLine ("GetCurrentScriptThreadID()");
            thread = 0;
        }
        public void GetScriptThreadID (uint win32ThreadId, out uint thread)
        {
            System.Diagnostics.Debug.WriteLine ("GetScriptThreadID()");
            thread = 0;
        }
        public void GetScriptThreadState (uint thread, out ScriptThreadState state)
        {
            System.Diagnostics.Debug.WriteLine ("GetScriptThreadState()");
            state = ScriptThreadState.NotInScript;
        }
        public void InterruptScriptThread (uint thread, ref stdole.EXCEPINFO exceptionInfo, uint flags)
        {
            System.Diagnostics.Debug.WriteLine ("InterruptScriptThread()");
            exceptionInfo = new stdole.EXCEPINFO ();
        }

        public void Clone (out IActiveScript script)
        {
            System.Diagnostics.Debug.WriteLine ("Clone()");
            script = this;
        }

        public void InitNew ()
        {
                this.currentScriptState = ScriptState.Disconnected;
                try {
                    if (this.site != null)
                        this.site.OnStateChange (this.currentScriptState);
                }
                catch (Exception e) {
                    System.Diagnostics.Debug.WriteLine (e.Message);
                }
                finally {

            }
         
        }

        public void AddScriptlet (string defaultName,
                    string code,
                    string itemName,
                    string subItemName,
                    string eventName,
                    string delimiter,
                    uint sourceContextCookie,
                    uint startingLineNumber,
                    uint flags,
                    out string name,
                    out stdole.EXCEPINFO info)
        {
            System.Diagnostics.Debug.WriteLine ("AddScriptlet");
            name = "";
            info = new stdole.EXCEPINFO ();
        }

        public void ParseScriptText (
                string code,
                string itemName,
                object context,
                string delimiter,
                int sourceContextCookie,
                uint startingLineNumber,
                ScriptText flags,
                IntPtr result,
                IntPtr excepInfo)
        {
            Console.WriteLine ("ParseScriptText()");
        }

        public void ParseProcedureText (
        string code,
        string itemName,
            string objectName,
        IntPtr context,
        string delimiter,
        uint sourceContextCookie,
        uint startingLineNumber,
        uint flags,
        out IntPtr result)
        {
            System.Diagnostics.Debug.WriteLine ("ParseProcedureText");
            result = IntPtr.Zero;

        }

    }
}
