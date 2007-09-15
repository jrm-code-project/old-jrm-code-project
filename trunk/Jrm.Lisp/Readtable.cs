using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;

namespace Lisp
{
    public enum ReadtableCase
    {
        Downcase,
        Invert,
        Preserve,
        Upcase
    };

    public enum SyntaxType
    {
        UndefinedZero,
        Constituent,
        Invalid,
        MultipleEscape,
        NonTerminatingMacroChar,
        SingleEscape,
        TerminatingMacroChar,
        Whitespace
    };

    [CLSCompliant (true)]
    public class Readtable
    {
        ReadtableCase readtableCase;
        readonly Dictionary<char, ReaderMacroFunction> MacroFunctionDictionary;
        readonly Dictionary<char, SyntaxType> SyntaxTypeDictionary;

        public Readtable ()
        {
            this.readtableCase = ReadtableCase.Upcase;
            this.SyntaxTypeDictionary = new Dictionary<char, SyntaxType> ();
            SyntaxTypeDictionary.Add ('\r', SyntaxType.Whitespace);
            SyntaxTypeDictionary.Add ('\n', SyntaxType.Whitespace);
            SyntaxTypeDictionary.Add ('\t', SyntaxType.Whitespace);
            SyntaxTypeDictionary.Add (' ', SyntaxType.Whitespace);
            SyntaxTypeDictionary.Add ('!', SyntaxType.Constituent);
            SyntaxTypeDictionary.Add ('"', SyntaxType.TerminatingMacroChar);
            SyntaxTypeDictionary.Add ('#', SyntaxType.NonTerminatingMacroChar);
            foreach (char a in "$%&")
                SyntaxTypeDictionary.Add (a, SyntaxType.Constituent);
            foreach (char a in "'()")
                SyntaxTypeDictionary.Add (a, SyntaxType.TerminatingMacroChar);
            foreach (char a in "*+")
                SyntaxTypeDictionary.Add (a, SyntaxType.Constituent);
            SyntaxTypeDictionary.Add (',', SyntaxType.TerminatingMacroChar);
            foreach (char a in "-./")
                SyntaxTypeDictionary.Add (a, SyntaxType.Constituent);
            foreach (char a in "0123456789")
                SyntaxTypeDictionary.Add (a, SyntaxType.Constituent);
            SyntaxTypeDictionary.Add (':', SyntaxType.Constituent);
            SyntaxTypeDictionary.Add (';', SyntaxType.TerminatingMacroChar);
            foreach (char a in "<=>?@")
                SyntaxTypeDictionary.Add (a, SyntaxType.Constituent);
            foreach (char a in "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                SyntaxTypeDictionary.Add (a, SyntaxType.Constituent);
            foreach (char a in "[]^_")
                SyntaxTypeDictionary.Add (a, SyntaxType.Constituent);
            SyntaxTypeDictionary.Add ('`', SyntaxType.TerminatingMacroChar);
            foreach (char a in "abcdefghijklmnopqrstuvwxyz")
                SyntaxTypeDictionary.Add (a, SyntaxType.Constituent);
            foreach (char a in "{}~")
                SyntaxTypeDictionary.Add (a, SyntaxType.Constituent);

            MacroFunctionDictionary = new Dictionary<char, ReaderMacroFunction> ();
            MacroFunctionDictionary.Add ('(', ReadLeftParen.ReaderMacroFunction);
            MacroFunctionDictionary.Add ('"', ReadDoubleQuote.ReaderMacroFunction);

        }

        public static ReaderMacroStep DefaultReaderMacroFunction (ReaderContext context, char character)
        {
            throw new NotImplementedException ();
        }


        public ReadtableCase Case
        {
            get
            {
                return this.readtableCase;
            }
            set
            {
                this.readtableCase = value;
            }
        }

        public ReaderMacroFunction GetMacroFunction (char character)
        {
            ReaderMacroFunction result;
            if (MacroFunctionDictionary.TryGetValue (character, out result))
                return result;
            else
                return DefaultReaderMacroFunction;
        }


        public SyntaxType GetSyntaxType (char character)
        {
            SyntaxType result;
            if (SyntaxTypeDictionary.TryGetValue (character, out result))
                return result;
            if (Char.IsWhiteSpace (character))
                return SyntaxType.Whitespace;
   throw new NotImplementedException("Unicode characters in readtable");
            
        }

        public bool IsWhitespaceSyntax (char character)
        {
            SyntaxType result;
            if (SyntaxTypeDictionary.TryGetValue (character, out result))
                return result == SyntaxType.Whitespace;
            else
                return Char.IsWhiteSpace (character);
        }
    }
}
