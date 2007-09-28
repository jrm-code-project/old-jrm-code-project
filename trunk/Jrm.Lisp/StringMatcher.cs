using System;


namespace Lisp
{
    class StringMatcher
    {
        private string ToMatch;

        public StringMatcher (string toMatch)
        {
            this.ToMatch = toMatch;
        }

        public bool MatchesPackage (Package p)
        {
            return p.Name == this.ToMatch
                || p.Nicknames.Contains (this.ToMatch);
        }

        public bool MatchesSymbol (Symbol s)
        {
            return s.Name == this.ToMatch;
        }
    }
}
