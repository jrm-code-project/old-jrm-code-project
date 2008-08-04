﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace Microcode
{
    public abstract class Channel
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
        FileStream inputReader;

        public FileChannel (FileStream inputReader)
        {
            this.inputReader = inputReader;
        }

        public override int Write (char [] buffer, int start, int end)
        {
            throw new NotImplementedException ();
        }

        public override char [] TypeName
        {
            get { return "file".ToCharArray (); }
        }

        public override bool IsBlocking
        {
            get { throw new NotImplementedException (); }
            set { throw new NotImplementedException (); }

        }

        public override int Read (char [] buffer, int start, int end)
        {
            byte [] myBuffer = new byte [(end - start)];
            int count = this.inputReader.Read (myBuffer, 0, (end - start));
            for (int i = 0; i < count; i++)
                buffer[start+i] = (char) myBuffer[i];
            return count;
        }

        public override bool Close ()
        {
            this.inputReader.Close ();
            DeallocateChannel (this);
            return true;
        }
    }

    class ScreenChannel : Channel
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

        public override int Read (char [] buffer, int start, int limit)
        {
            if (blocking == true) {
                int filled = this.input.Read (buffer, start, limit - start);
                return (filled);
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

        [SchemePrimitive ("CHANNEL-BLOCKING?", 1)]
        public static object IsChannelBlocking (Interpreter interpreter, object arg)
        {
            return interpreter.Return ((Channel.ArgChannel ((int) arg)).IsBlocking);
        }

        [SchemePrimitive ("CHANNEL-CLOSE", 1)]
        public static object ChannelClose (Interpreter interpreter, object arg)
        {
            return interpreter.Return ((Channel.ArgChannel ((int) arg)).Close ());
        }

        [SchemePrimitive ("CHANNEL-DESCRIPTOR", 1)]
        public static object PrimitiveChannelDescriptor (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg);
        }

        [SchemePrimitive ("CHANNEL-BLOCKING", 1)]
        public static object ChannelBlocking (Interpreter interpreter, object arg)
        {
            bool wasBlocking = (Channel.ArgChannel ((int) arg)).IsBlocking;
            (Channel.ArgChannel ((int) arg)).IsBlocking = true;
            return interpreter.Return (wasBlocking);
        }

        [SchemePrimitive ("CHANNEL-NONBLOCKING", 1)]
        public static object ChannelNonBlocking (Interpreter interpreter, object arg)
        {
            bool wasBlocking = (Channel.ArgChannel ((int) arg)).IsBlocking;
            (Channel.ArgChannel ((int) arg)).IsBlocking = false;
            return interpreter.Return (wasBlocking);
        }

        [SchemePrimitive ("CHANNEL-TYPE-NAME", 1)]
        public static object ChannelTypeName (Interpreter interpreter, object arg)
        {
            return interpreter.Return (Channel.ArgChannel ((int)arg).TypeName);
        }

        [SchemePrimitive ("CHANNEL-READ", 4)]
        public static object ChannelRead (Interpreter interpreter, object [] arglist)
        {
            return interpreter.Return (Channel.ArgChannel ((int) arglist[0]).Read ((char[]) arglist[1], (int) arglist[2], (int) arglist[3]));


        }

        [SchemePrimitive ("CHANNEL-WRITE", 4)]
        public static object ChannelWrite (Interpreter interpreter, object [] arglist)
        {
            return interpreter.Return ((Channel.ArgChannel (0)).Write ((char []) (arglist [1]), (int) (arglist [2]), (int) (arglist [3])));
        }

        [SchemePrimitive ("NEW-FILE-OPEN-INPUT-CHANNEL", 2)]
        public static object NewFileOpenInputChannel (Interpreter interpreter, object arg0, object arg1)
        {
            char [] filename = (char []) arg0;
            WeakCons holder = (WeakCons) arg1;
            int channel = Channel.MakeFileChannel(new FileStream (new String (filename), System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.Read));
            holder.Cdr = channel;
            return interpreter.Return(true);
        }

        [SchemePrimitive("TERMINAL-NONBUFFERED", 1)]
        public static object TerminalNonBuffered(Interpreter interpreter, object arg0)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive("TEST-SELECT-DESCRIPTOR", 3)]
        public static object TestSelectDescriptor(Interpreter interpreter, object arg0, object arg1, object arg2)
        {
            int channel = (int) arg0;
            bool blockp = (bool) arg1;
            int qmode = (int) arg2;

            return interpreter.Return (blockp ? Channel.WaitOnSingleObject (channel, qmode) : Channel.TestSingleObject (channel, qmode));
        }

        [SchemePrimitive ("TTY-INPUT-CHANNEL", 0)]
        public static object TtyInputChannel (Interpreter interpreter)
        {
            return interpreter.Return (Channel.TTYInputChannel);
        }

        [SchemePrimitive ("TTY-OUTPUT-CHANNEL", 0)]
        public static object TtyOutputChannel (Interpreter interpreter)
        {
            return interpreter.Return (Channel.TTYOutputChannel);
        }
    }
}