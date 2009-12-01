using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace Microcode
{
    /// <summary>
    /// Input/output channel.  Must be public.
    /// </summary>
    abstract public class Channel
    {
        static int input_channel = 0;
        static int output_channel = 1;

        static Channel [] ChannelTable = new Channel [1024];

        internal static int TTYInputChannel
        {
            get
            {
                return input_channel;
            }
        }

        internal static int TTYOutputChannel
        {
            get
            {
                return output_channel;
            }
        }

        static public void Initialize (TextReader input, TextWriter output)
        {
            ChannelTable [0] = new ScreenChannel (input, output);
            ChannelTable [1] = ChannelTable [0];
        }

        static public Channel ArgChannel (int arg)
        {
            return ChannelTable [arg];
        }

        static public int WaitOnSingleObject (int channel, int qmode)
        {
            throw new NotImplementedException ();
        }

        static public int TestSingleObject (int channel, int qmode)
        {
            int rmode = TestSingleObject1 (channel, qmode);
            return
                (rmode > 1) ? rmode :
                OSScheme.IsPendingInterrupts () ? OSIO.SelectInterrupt :
                ProcessAnyStatusChange () ? OSIO.SelectProcessStatusChange :
                0;
        }

        static public int TestSingleObject1 (int channel, int qmode)
        {
            int rmode = (qmode & OSIO.SelectModeWrite);
            if (((qmode & OSIO.SelectModeRead) != 0)
                && ((channel == TTYInputChannel)
                    ? TestForPendingEvent ()
                    : (ChannelNRead (channel) > 0)))
                rmode |= OSIO.SelectModeRead;
            return rmode;
        }

        static public bool TestForPendingEvent ()
        {
            return false;
        }

        static public bool ProcessAnyStatusChange ()
        {
            return false;
        }

        static public int ChannelNRead (int channel)
        {
            throw new NotImplementedException ();
        }

        static protected void DeallocateChannel (Channel moribund)
        {
            for (int i = 0; i < ChannelTable.Length; i++) {
                if (ChannelTable [i] == moribund) {
                    ChannelTable [i] = null;
                    return;
                }
            }
            throw new NotImplementedException ();
        }

        static public int MakeFileChannel (FileStream fileInputStream)
        {
            for (int i = 0; i < ChannelTable.Length; i++) {
                if (ChannelTable [i] == null) {
                    ChannelTable [i] = new FileChannel (fileInputStream);
                    return i;
                }
            }
            throw new NotImplementedException ();
        }

        public abstract bool Close ();
        public abstract bool IsBlocking { get; set; }
        public abstract int Read (char [] buffer, int start, int end);
        public abstract char [] TypeName { get; }
        public abstract int Write (char [] buffer, int start, int end);
    }

    class GenericChannel : Channel
    {
        public override int Write (char [] buffer, int start, int end)
        {
            throw new NotImplementedException ();
        }

        public override char [] TypeName
        {
            get { throw new NotImplementedException (); }
        }

        public override bool IsBlocking
        {
            get { throw new NotImplementedException (); }
            set { throw new NotImplementedException (); }
        }

        public override int Read (char [] buffer, int start, int end)
        {
            throw new NotImplementedException ();
        }

        public override bool Close ()
        {
            throw new NotImplementedException ();
        }
    }

    class FileChannel : Channel
    {
        FileStream stream;
        bool isBlocking;

        public FileChannel (FileStream stream)
        {
            this.stream = stream;
        }

        public override int Write (char [] buffer, int start, int end)
        {
            int count = end - start;
            byte [] byteBuffer = new byte [count];
            for (int i = 0; i < count; i++) {
                byteBuffer [i] = (byte) buffer [start + i];
            }
            this.stream.Write (byteBuffer, 0, count);
            return count;
        }

        public override char [] TypeName
        {
            get { return "file".ToCharArray (); }
        }

        public override bool IsBlocking
        {
            get { return isBlocking; }
            set { isBlocking = value; }
        }

        byte [] inputBuffer = new byte [1024];

        public override int Read (char [] buffer, int start, int end)
        {
            int length = end - start;
            while (length > this.inputBuffer.Length) {
                inputBuffer = new byte [this.inputBuffer.Length * 2];
            }
            int count = this.stream.Read (this.inputBuffer, 0, length);
            //System.Text.Encoding.ASCII.GetChars (this.inputBuffer, 0, count, buffer, start);

            for (int i = 0; i < count; i++)
                buffer[start+i] = (char) this.inputBuffer[i];
            return count;
        }

        public override bool Close ()
        {
            this.stream.Close ();
            DeallocateChannel (this);
            return true;
        }
    }

    /// <summary>
    /// I/O to the tty.
    /// </summary>
    public class ScreenChannel : Channel
    {
        TextReader input;
        TextWriter output;
        bool blocking = true;

        public ScreenChannel (TextReader input, TextWriter output)
        {
            this.input = input;
            this.output = output;
        }

        public override int Write (char [] buffer, int start, int end)
        {
            for (int i = start; i < end; i++) {
                output.Write (buffer [i]);
            }

            return end - start;
        }

        public override char [] TypeName
        {
            get { return "terminal".ToCharArray (); }
        }

        public override bool IsBlocking
        {
            get { return blocking; }
            set { blocking = value; }
        }

        static public bool firstTime = true;
        const string cannedString = "(finish-cold-load)\n";
        static object promptTime = 0;
        public override int Read (char [] buffer, int start, int end)
        {
            if (firstTime) {
                Misc.SystemClock (out promptTime);
                Console.WriteLine ("Cold load time: {0}", promptTime);
                object ignore;
                Statistics.Reset (out ignore);
                Console.WriteLine (";; Hack:  invoking finish-cold-load.");
                StandardClosure.printName = true;
                StaticClosure.printName = true;
                SimpleClosure.printName = true;
                for (int i = 0; i < cannedString.Length; i++)
                    buffer [start + i] = cannedString [i];
                firstTime = false;
                return cannedString.Length;
            }

            if (blocking == true) {
                int filled = this.input.Read (buffer, start, end - start);
                return filled;
            }
            else {
                return 0;
            }
        }

        public override bool Close ()
        {
            throw new NotImplementedException ();
        }
    }

    class AnonymousPipeChannel : Channel
    {
        public override int Write (char [] buffer, int start, int end)
        {
            throw new NotImplementedException ();
        }

        public override char [] TypeName
        {
            get { throw new NotImplementedException (); }
        }

        public override bool IsBlocking
        {
            get { throw new NotImplementedException (); }
            set { throw new NotImplementedException (); }
        }

        public override int Read (char [] buffer, int start, int end)
        {
            throw new NotImplementedException ();
        }

        public override bool Close ()
        {
            throw new NotImplementedException ();
        }
    }

    class NamedPipeChannel : Channel
    {
        public override int Write (char [] buffer, int start, int end)
        {
            throw new NotImplementedException ();
        }

        public override char [] TypeName
        {
            get { throw new NotImplementedException (); }
        }

        public override bool IsBlocking
        {
            get { throw new NotImplementedException (); }
            set { throw new NotImplementedException (); }
        }

        public override int Read (char [] buffer, int start, int end)
        {
            throw new NotImplementedException ();
        }

        public override bool Close ()
        {
            throw new NotImplementedException ();
        }
    }

    static class IOPrims
    {
        [SchemePrimitive ("CHANNEL-BLOCKING", 1, false)]
        public static bool ChannelBlocking (out object answer, object arg)
        {
            answer = (Channel.ArgChannel ((int) arg)).IsBlocking;
            (Channel.ArgChannel ((int) arg)).IsBlocking = true;
            return false;
        }
        
        [SchemePrimitive ("CHANNEL-BLOCKING?", 1, false)]
        public static bool IsChannelBlocking (out object answer, object arg)
        {
            answer = Channel.ArgChannel ((int) arg).IsBlocking;
            return false;
        }

        [SchemePrimitive ("CHANNEL-CLOSE", 1, false)]
        public static bool ChannelClose (out object answer, object arg)
        {
            answer = Channel.ArgChannel ((int) arg).Close ();
            return false;
        }

        [SchemePrimitive ("CHANNEL-DESCRIPTOR", 1, false)]
        public static bool PrimitiveChannelDescriptor (out object answer, object arg)
        {
            answer = arg;
            return false;
        }

        [SchemePrimitive ("CHANNEL-NONBLOCKING", 1, false)]
        public static bool ChannelNonBlocking (out object answer, object arg)
        {
            bool wasBlocking = Channel.ArgChannel ((int) arg).IsBlocking;
            Channel.ArgChannel ((int) arg).IsBlocking = false;
            answer = (wasBlocking);
            return false;
        }

        [SchemePrimitive ("CHANNEL-TYPE-NAME", 1, false)]
        public static bool ChannelTypeName (out object answer, object arg)
        {
            answer = Channel.ArgChannel ((int) arg).TypeName;
            return false;
        }

        [SchemePrimitive ("CHANNEL-READ", 4, false)]
        public static bool ChannelRead (out object answer, object [] arglist)
        {
            answer = Channel.ArgChannel ((int) arglist [0]).Read ((char []) arglist [1], (int) arglist [2], (int) arglist [3]);
            return false;
        }

        [SchemePrimitive ("CHANNEL-WRITE", 4, false)]
        public static bool ChannelWrite (out object answer, object [] arglist)
        {
            answer = Channel.ArgChannel ((int) arglist[0]).Write ((char []) (arglist [1]), (int) (arglist [2]), (int) (arglist [3]));
            return false;
        }

        [SchemePrimitive ("FILE-EQ?", 2, false)]
        public static bool IsFileEq (out object answer, object arg0, object arg1)
        {
            string name0 = new string ((char[]) arg0);
            string name1 = new string ((char[]) arg1);
            answer = new FileInfo (name0).FullName == new FileInfo (name1).FullName;
            return false;
        }

        static object [] directories = new object [32];

        [SchemePrimitive ("NEW-DIRECTORY-OPEN", 1, false)]
        public static bool NewDirectoryOpen (out object answer, object arg0)
        {
            string pathAndPattern = new string ((char []) arg0);
            int x = pathAndPattern.LastIndexOf ('\\');
            string path = pathAndPattern.Substring (0, x + 1);
            string pattern = pathAndPattern.Substring (x + 1, pathAndPattern.Length - (x + 1));
            for (int i = 0; i < directories.Length; i++) {
                if (directories [i] == null) {
                    directories [i] = Directory.GetFiles (path, pattern);
                    answer = i;
                    return false;
                }
            }
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("NEW-DIRECTORY-READ", 1, false)]
        public static bool NewDirectoryRead (out object answer, object arg0)
        {
            int dir = (int) arg0;
            string [] files = directories [dir] as string [];
            if (files == null)
                throw new NotImplementedException ();
            else if (files.Length == 0) {
                directories [dir] = null;
                answer = Constant.sharpF;
                return false;
            }
            else {
                answer = files [0].ToCharArray ();
                string [] newFiles = new string [files.Length - 1];
                Array.Copy (files, 1, newFiles, 0, files.Length - 1);
                directories [dir] = newFiles;
                return false;
            }
        }

        [SchemePrimitive ("NEW-DIRECTORY-CLOSE", 1, false)]
        public static bool NewDirectoryClose (out object answer, object arg0)
        {
            int dir = (int) arg0;
            directories [dir] = null;
            answer = Constant.Unspecific;
            return false;
        }

        [SchemePrimitive ("NEW-FILE-OPEN-INPUT-CHANNEL", 2, false)]
        public static bool NewFileOpenInputChannel (out object answer, object arg0, object arg1)
        {
            char [] filename = (char []) arg0;
            WeakCons holder = (WeakCons) arg1;
            int channel = Channel.MakeFileChannel (new FileStream (new String (filename), System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.Read));
            holder.Cdr = channel;
            answer = true;
            return false;
        }

        [SchemePrimitive ("NEW-FILE-OPEN-OUTPUT-CHANNEL", 2, false)]
        public static bool NewFileOpenOutputChannel (out object answer, object arg0, object arg1)
        {
            char [] filename = (char []) arg0;
            WeakCons holder = (WeakCons) arg1;
            int channel = Channel.MakeFileChannel (new FileStream (new String (filename), System.IO.FileMode.CreateNew, System.IO.FileAccess.Write, System.IO.FileShare.None));
            holder.Cdr = channel;
            answer = true;
            return false;
        }

        [SchemePrimitive("TERMINAL-NONBUFFERED", 1, false)]
        public static bool TerminalNonBuffered (out object answer, object arg0)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("TEST-SELECT-DESCRIPTOR", 3, false)]
        public static bool TestSelectDescriptor (out object answer, object arg0, object arg1, object arg2)
        {
            int channel = (int) arg0;
            bool blockp = (bool) arg1;
            int qmode = (int) arg2;

            answer = blockp ? Channel.WaitOnSingleObject (channel, qmode) : Channel.TestSingleObject (channel, qmode);
            return false;
        }

        [SchemePrimitive ("TTY-INPUT-CHANNEL", 0, true)]
        public static bool TtyInputChannel (out object answer)
        {
            answer = Channel.TTYInputChannel;
            return false;
        }

        [SchemePrimitive ("TTY-OUTPUT-CHANNEL", 0, true)]
        public static bool TtyOutputChannel (out object answer)
        {
            answer = Channel.TTYOutputChannel;
            return false;
        }
    }
}
