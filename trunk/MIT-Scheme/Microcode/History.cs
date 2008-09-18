using System;
using System.Diagnostics;

namespace Microcode
{
    [Serializable]
    class History: ISystemHunk3
    {
        HistoryElement element;

        protected History (HistoryElement element)
        {
            this.element = element;
        }

        public object Element
        {
            [DebuggerStepThrough]
            get
            {
                return this.element;
            }
        }

        public SCode Expression
        {
            [DebuggerStepThrough]
            get
            {
                return element.Expression;
            }

            set
            {
                element.Expression = value;
            }
        }

        public Environment Environment
        {
            [DebuggerStepThrough]
            get
            {
                return element.Environment;
            }
            set
            {
                element.Environment = value;
            }
        }

        public History NextReduction
        {
            [DebuggerStepThrough]
            get
            {
                return element.NextReduction;
            }

            set
            {
                element.NextReduction = value;
            }
        }

        public History Rib
        {
            [DebuggerStepThrough]
            get
            {
                return element.Rib;
            }

            set
            {
                element.Rib = value;
            }
        }

        public History NextSubproblem
        {
            [DebuggerStepThrough]
            get
            {
                return element.NextSubproblem;
            }

            set
            {
                element.NextSubproblem = value;
            }
        }

        public History PreviousSubproblem
        {
            [DebuggerStepThrough]
            get
            {
                return element.PreviousSubproblem;
            }

            set
            {
                element.PreviousSubproblem = value;
            }
        }

        public static History DummyHistory ()
        {
            History rib = new UnmarkedHistory (new HistoryRib ());
            rib.Expression = null;
            rib.Environment = null;
            rib.NextReduction = rib;
            History vertebra = new UnmarkedHistory (new HistoryVertebra ());
            vertebra.Rib = rib;
            vertebra.NextSubproblem = vertebra;
            vertebra.PreviousSubproblem = vertebra;
            return vertebra;
        }

        internal void NewReduction (SCode expression, object env)
        {
        }
        internal void NewSubproblem (SCode expression, object env)
        {
        }
        internal void ReuseSubproblem (SCode expression, object env)
        {
        }

        #region ISystemHunk3 Members

        public object SystemHunk3Cxr0
        {
            get
            {
                return element.SystemHunk3Cxr0;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public object SystemHunk3Cxr1
        {
            get
            {
                return element.SystemHunk3Cxr1;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public object SystemHunk3Cxr2
        {
            get
            {
                return element.SystemHunk3Cxr2;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        [SchemePrimitive ("SET-CURRENT-HISTORY!", 1, false)]
        public static bool SetCurrentHistory (out object answer, object arg)
        {
            answer = false;
            return false;
        }

    }

    [Serializable]
    class MarkedHistory : History
    {
        public MarkedHistory (HistoryElement element)
            : base (element)
        {
        }
    }

    [Serializable]
    class UnmarkedHistory : History
    {
        public UnmarkedHistory (HistoryElement element)
            : base (element)
        {
        }

        [SchemePrimitive ("HUNK3-B?", 1, true)]
        public static bool IsHunk3b (out object answer, object arg)
        {
            answer = arg is UnmarkedHistory;
            return false;
        }
    }

    [Serializable]
    abstract class HistoryElement : ISystemHunk3
    {

        public abstract SCode Expression
        {
            get;
            set;
        }

        public abstract Environment Environment
        {
            get;
            set;
        }

        public abstract History NextReduction
        {
            get;
            set;
        }

        public abstract History Rib
        {
            get;
            set;
        }

        public abstract History NextSubproblem
        {
            get;
            set;
        }

        public abstract History PreviousSubproblem
        {
            get;
            set;
        }
        public abstract object SystemHunk3Cxr0
        {
            get;
            set;
        }
        public abstract object SystemHunk3Cxr1
        {
            get;
            set;
        }
        public abstract object SystemHunk3Cxr2
        {
            get;
            set;
        }

    }

    [Serializable]
    class HistoryRib : HistoryElement
    {
        SCode expression;
        Environment environment;
        History nextReduction;

        public override object SystemHunk3Cxr0
        {
            [DebuggerStepThrough]
            get
            {
                return this.expression;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public override object SystemHunk3Cxr1
        {
            [DebuggerStepThrough]
            get
            {
                return this.environment;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public override object SystemHunk3Cxr2
        {
            [DebuggerStepThrough]
            get
            {
                return this.nextReduction;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }
        public override SCode Expression
        {
            [DebuggerStepThrough]
            get
            {
                return this.expression;
            }

            set
            {
                this.expression = value;
            }
        }

        public override Environment Environment
        {
            [DebuggerStepThrough]
            get
            {
                return this.environment;
            }
            set
            {
                this.environment = value;
            }
        }

        public override History NextReduction
        {
            [DebuggerStepThrough]
            get
            {
                return this.nextReduction;
            }

            set
            {
                this.nextReduction = value;
            }
        }

        public override History Rib
        {
            [DebuggerStepThrough]
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public override History NextSubproblem
        {
            [DebuggerStepThrough]
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }

        }

        public override History PreviousSubproblem
        {
            [DebuggerStepThrough]
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }

        }
    }

    [Serializable]
    class HistoryVertebra : HistoryElement
    {
        History rib;
        History nextSubproblem;
        History previousSubproblem;

        public override object SystemHunk3Cxr0
        {
            [DebuggerStepThrough]
            get
            {
                return this.rib;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public override object SystemHunk3Cxr1
        {
            [DebuggerStepThrough]
            get
            {
                return this.nextSubproblem;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public override object SystemHunk3Cxr2
        {
            [DebuggerStepThrough]
            get
            {
                return this.previousSubproblem;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }
        public override SCode Expression
        {
            [DebuggerStepThrough]
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public override Environment Environment
        {
            [DebuggerStepThrough]
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public override History NextReduction
        {
            [DebuggerStepThrough]
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public override History Rib
        {
            [DebuggerStepThrough]
            get
            {
                return this.rib;
            }

            set
            {
                this.rib = value;
            }
        }

        public override History NextSubproblem
        {
            [DebuggerStepThrough]
            get
            {
                return this.nextSubproblem;
            }

            set
            {
                this.nextSubproblem = value;
            }
        }

        public override History PreviousSubproblem
        {
            [DebuggerStepThrough]
            get
            {
                return this.previousSubproblem;
            }

            set
            {
                this.previousSubproblem = value;
            }
        }
    }
}
