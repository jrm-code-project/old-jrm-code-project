using System;
using System.Runtime.InteropServices;

namespace LScriptTest
{
    using ActiveScripting;
    using LScript;

  

    public class ScriptHost : IActiveScriptSite
    {
        public ScriptHost ()
        {
        }

        #region IActiveScriptSite

        public void GetDocVersionString (out string v)
        {
            throw new NotImplementedException ();
        }

        public void GetItemInfo (string name,
            uint returnMask,
            out object item,
           out System.Type typeInfo)
        {
            item = null;
            typeInfo = null;
        }

        public void GetLCID (out uint id)
        {
            throw new NotImplementedException ();
        }

        public void OnEnterScript ()
        {
        }

        public void OnLeaveScript ()
        {
        }

        public void OnScriptError (IActiveScriptError error)
        {
        }

        public void OnScriptTerminate (ref object result,
ref stdole.EXCEPINFO info)
        {
        }

       public void OnStateChange (tagSCRIPTSTATE newState)
        {
        }

        #endregion

        public void Run ()
        {
            try {
                object engine = new LScript ();
                IActiveScriptParse32 iap = engine as IActiveScriptParse32;
                iap.InitNew ();

                IActiveScript ias = engine as IActiveScript;
                ias.SetScriptSite (this);

                ias.Close ();
            }
            catch (ExecutionEngineException e) {
                System.Diagnostics.Debug.Assert (false, e.Message);
            }
        }
    }

    class Program
    {


        static void Main (string [] args)
        {
            new ScriptHost ().Run ();

        }
    }
}
