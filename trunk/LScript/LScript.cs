using Jrm.Lisp;
using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;


namespace LScript
{
    [ClassInterface (ClassInterfaceType.None),
     ComVisible(true),
     Guid("62416980-8B84-4c51-A09C-AB9623C3CC3E"),
     ProgId ("LScript")]
    public class Engine : IActiveScript, IActiveScriptParse, IDisposable
    {
        TraceListener traceListener = new TextWriterTraceListener (Console.Out);
        IActiveScriptSite site;
        ScriptState currentScriptState = ScriptState.Uninitialized;

        public Engine ()
        {
            Debug.Listeners.Add (traceListener);
        }

        ~Engine ()
        {
            Dispose (false);
        }

        public void Dispose ()
        {
            Dispose (true);
            GC.SuppressFinalize (this);
        }

        protected virtual void Dispose (bool disposing)
        {
            if (disposing) {
                if (traceListener != null) {
                    Debug.Listeners.Remove (traceListener);
                    traceListener.Dispose ();
                    traceListener = null;
                }
            }
        }

        public void SetScriptSite (IActiveScriptSite site)
        {
            Debug.WriteLine ("SetScriptSite()");
            this.site = site;
        }

        public void GetScriptSite (ref Guid riid, out IntPtr site)
        {
            Debug.WriteLine ("GetScriptSite()");
            site = IntPtr.Zero;
        }

        public void SetScriptState (ScriptState state)
        {
            Debug.WriteLine ("SetScriptState(" + state.ToString() + ")");
            this.currentScriptState =  state;
            // site.OnStateChange (this.currentScriptState);  
        }

        public void GetScriptState (out ScriptState scriptState)
        {
            Debug.WriteLine ("GetScriptState()");
            scriptState = this.currentScriptState;
        }

        public void Close ()
        {
            Debug.WriteLine ("Close()");
        }

        public void AddNamedItem (string name, ScriptItem flags)
        {
            Debug.WriteLine ("AddNamedItem(\"" + name + "\", " + flags.ToString () + ")");
            //// What did he give us?
            //object item;
            //object typeinfo;
            //if (this.site != null) {
            //    try {
            //        this.site.GetItemInfo (name, ScriptInfo.IUnknown, out item, out typeinfo);
            //        Debug.WriteLine ("item is " + item.ToString ());
            //        Type type = (item as IDispatch).GetTypeInfo(0,0x409);
            //            Debug.WriteLine ("Type is " + type.ToString());
            //            foreach (System.Reflection.MemberInfo minfo in type.GetMembers ())
            //                Debug.WriteLine (minfo);
            //    }
            //    catch (Exception e) {
            //        Debug.WriteLine ("Exception " + e.ToString ());
            //    }
            //}
            //else {
            //    // I guess we can find out later.
            //    Debug.WriteLine ("No site, but named items being added.");
            //}
        }

        public void AddTypeLib (ref Guid typeLib, uint major, uint minor, uint flags)
        {
            Debug.WriteLine ("AddTypeLib()");
        }

        public void GetScriptDispatch (string itemName, out object dispatch)
        {
            Debug.WriteLine ("GetScriptDispatch()");
            dispatch = null;
        }
        public void GetCurrentScriptThreadID (out uint thread)
        {
            Debug.WriteLine ("GetCurrentScriptThreadID()");
            thread = 0;
        }
        public void GetScriptThreadID (uint win32ThreadId, out uint thread)
        {
            Debug.WriteLine ("GetScriptThreadID()");
            thread = 0;
        }
        public void GetScriptThreadState (uint thread, out ScriptThreadState state)
        {
            Debug.WriteLine ("GetScriptThreadState()");
            state = ScriptThreadState.NotInScript;
        }
        public void InterruptScriptThread (uint thread, IntPtr excepinfo, uint flags)
        {
            Debug.WriteLine ("InterruptScriptThread()");
        }

        public void Clone (out IActiveScript script)
        {
            Debug.WriteLine ("Clone()");
            script = this;
        }

        public void InitNew ()
        {
            Debug.WriteLine ("InitNew()");
                this.currentScriptState = ScriptState.Disconnected;
                    if (this.site != null)
                        this.site.OnStateChange (this.currentScriptState);
         
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
                    IntPtr name,
                    IntPtr info)
        {
            Debug.WriteLine ("AddScriptlet");
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
                IntPtr info)
        {
            Debug.WriteLine ("ParseScriptText()");
            Debug.WriteLine ("code: " + code);
            Debug.WriteLine ("itemName: "+ itemName);
            Debug.WriteLine ("delimiter: "+delimiter);
            Debug.WriteLine ("flags: " + flags);
            Debug.Flush ();
            
            //result = null;
            // info = new System.Runtime.InteropServices.ComTypes.EXCEPINFO ();
            object read = CL.ReadFromString (code);
        }

    }
}
