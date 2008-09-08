using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    public class CompileTimeEnvironment
    {
        string [] topFrame;
        Dictionary<string,Variable> variableTable;

        public CompileTimeEnvironment (string [] topFrame)
        {
            this.topFrame = topFrame;
            this.variableTable = new Dictionary<string, Variable> ();
        }

        public CompileTimeEnvironment Extend (string [] topFrame)
        {
            CompileTimeEnvironment ext = new CompileTimeEnvironment (topFrame);
            foreach (KeyValuePair<string,Variable> entry in variableTable) {
                bool keep = true;
                foreach (string name in topFrame) {
                    if (name == entry.Key) keep = false;
                }
                if (keep)
                    ext.variableTable.Add (entry.Key, entry.Value);
            }
            return ext;
        }

        public bool LexicalAddress (string name, out int depth, out int offset)
        {
            return this.LexicalAddress (name, out depth, out offset, 0);
        }

        bool LexicalAddress (string name, out int depth, out int offset, int depthCounter)
        {
            if (this.topFrame != null) {
                for (int i = 0; i < topFrame.Length; i++)
                    if (topFrame [i] == name) {
                        depth = depthCounter;
                        offset = i;
                        return true;
                    }
                depth = -1;
                offset = -1;
                return false;
            }
            depth = -1;
            offset = -1;
            return false;
        }


        public int Length
        {
            get
            {
                return (this.topFrame == null) ? 0 : this.topFrame.Length;
            }
        }

        internal Dictionary<string, Variable> VariableTable
        {
            get
            {
                return this.variableTable;
            }
        }

        public string this [int index]
        {
            get
            {
                return this.topFrame [index];
            }
        }
    }
}
