using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using VSCommand = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;

namespace VisualRust {
    internal sealed class RustSignatureHelpCommandHandler : VSCommandTarget<VSCommand>
    {
        private ISignatureHelpBroker  Broker;
        private ISignatureHelpSession CurrentSession;

        public RustSignatureHelpCommandHandler(IVsTextView viewAdapter, IWpfTextView textView, ISignatureHelpBroker broker)
            : base(viewAdapter, textView)
        {
            Broker = broker;
            CurrentSession = null;
        }

        private char GetTypeChar(IntPtr pvaIn) { return (char)(ushort)Marshal.GetObjectForNativeVariant(pvaIn); }
        private bool IsSessionStarted { get { return !(CurrentSession?.IsDismissed ?? true); } }

        protected override bool Execute(VSCommand command, uint options, IntPtr pvaIn, IntPtr pvaOut)
        {
            // Commands we handle
            switch (command)
            {
                case VSCommand.CANCEL:
                    if (CurrentSession != null)
                    {
                        CurrentSession.Dismiss();
                        return true;
                    }
                    break;
            }

            if (!ExecuteNext(command, options, pvaIn, pvaOut)) return false;

            // Commands we peek, but don't actually handle
            switch (command)
            {
                case VSCommand.TYPECHAR:
                    char ch = GetTypeChar(pvaIn);

                    switch (ch)
                    {
                    case '(': // possible start of function argument list
                    case '<': // possible start of generic argument list
                        if (IsSessionStarted)
                        {
                            RefreshSession();
                        }
                        else // We're looking for "ident (", "ident <", or "ident :: <" as hints that we should display the signature help for "ident"
                        {
                            var caret     = TextView.Caret.Position.BufferPosition;
                            var caretLine = caret.GetContainingLine();
                            var caretCol  = caret.Position - caretLine.Start.Position;
                            var tokens    = Utils.LexString(caretLine.GetText()).Where(token => !WhitespaceTokenTypes.Contains(token.Type)).ToList();

                            int tokensBefore = 0;
                            while (tokensBefore < tokens.Count && tokens[tokensBefore].StopIndex < caretCol) ++tokensBefore;
                            var tokensBeforeCaret = tokensBefore;

                            Func<int[], bool> isPrefixedBy = (lexerTokenTypes) =>
                            {
                                if (tokensBeforeCaret < lexerTokenTypes.Length) return false;

                                int offset = tokensBeforeCaret - lexerTokenTypes.Length;
                                for (int i=0; i<lexerTokenTypes.Length; ++i) if (lexerTokenTypes[i] != tokens[i + offset].Type) return false;
                                return true;
                            };

                            if (isPrefixedBy(new[] {RustLexer.RustLexer.IDENT, RustLexer.RustLexer.LPAREN }))
                            {
                                Trace("CheckStartSignatureSession: ident(");
                                StartSession();
                            }
                            else if (isPrefixedBy(new[] {RustLexer.RustLexer.IDENT, RustLexer.RustLexer.LT }))
                            {
                                Trace("CheckStartSignatureSession: ident<");
                                StartSession();
                            }
                            else if (isPrefixedBy(new[] {RustLexer.RustLexer.IDENT, RustLexer.RustLexer.MOD_SEP, RustLexer.RustLexer.LT }))
                            {
                                Trace("CheckStartSignatureSession: ident::<");
                                StartSession();
                            }
                        }
                        break;

                    case ',': // possible restart of * argument list or advance to next argument
                        if (IsSessionStarted)
                            RefreshSession();
                        else
                            StartSession();
                        break;

                    case ')': // possible end of function argument list
                    case '>': // possible end of generic argument list
                        RefreshSession();
                        break;

                    default: // misc. other typing
                        if (IsSessionStarted && string.IsNullOrEmpty(CurrentSession.SelectedSignature.ApplicableToSpan.GetText(CurrentSession.TextView.TextSnapshot)))
                            RefreshSession();
                        break;
                    }
                    break;

                default:
                    RefreshSession();
                    break;
            }

            return true; // ExecuteNext succeeded earlier
        }

        private static readonly int[] WhitespaceTokenTypes = new[]
        {
            RustLexer.RustLexer.WHITESPACE,
            RustLexer.RustLexer.COMMENT,
            RustLexer.RustLexer.DOC_COMMENT,
        };

        private bool StartSession()
        {
            Trace("StartSession");
            if (CurrentSession != null) return false;
            SnapshotPoint point = TextView.Caret.Position.BufferPosition;
            ITextSnapshot snapshot = point.Snapshot;

            if (!Broker.IsSignatureHelpActive(TextView))
            {
                CurrentSession = Broker.CreateSignatureHelpSession(TextView, snapshot.CreateTrackingPoint(point, PointTrackingMode.Negative), true);
            }
            else
            {
                CurrentSession = Broker.GetSessions(TextView)[0];
            }

            CurrentSession.Dismissed += (sender, args) => CurrentSession = null;
            CurrentSession.Start();
            return true;
        }

        private bool RefreshSession()
        {
            Trace("RefreshSession");
            if (CurrentSession == null) return false;
            CurrentSession.Recalculate();
            return true;
        }

        protected override IEnumerable<VSCommand> SupportedCommands
        {
            get
            {
                yield return VSCommand.CANCEL;

                // start / filter / complete commands
                yield return VSCommand.TYPECHAR;
                yield return VSCommand.BACKSPACE;
                yield return VSCommand.DELETE;
                yield return VSCommand.DELETEWORDLEFT;
                yield return VSCommand.DELETEWORDRIGHT;

                // TODO: Consider watching for text buffer changes instead of doing all this?
                // cursor navigation commands
                yield return VSCommand.LEFT;
                yield return VSCommand.LEFT_EXT;
                yield return VSCommand.RIGHT;
                yield return VSCommand.RIGHT_EXT;
                yield return VSCommand.UP;
                yield return VSCommand.UP_EXT;
                yield return VSCommand.DOWN;
                yield return VSCommand.DOWN_EXT;
                yield return VSCommand.PAGEUP;
                yield return VSCommand.PAGEUP_EXT;
                yield return VSCommand.PAGEDN;
                yield return VSCommand.PAGEDN_EXT;

                yield return VSCommand.BOL;
                yield return VSCommand.BOL_EXT;
                yield return VSCommand.BOL_EXT_COL;
                yield return VSCommand.EOL;
                yield return VSCommand.EOL_EXT;
                yield return VSCommand.EOL_EXT_COL;
                yield return VSCommand.HOME;
                yield return VSCommand.HOME_EXT;
                yield return VSCommand.END;
                yield return VSCommand.END_EXT;

                yield return VSCommand.LASTCHAR;
                yield return VSCommand.LASTCHAR_EXT;
                yield return VSCommand.FIRSTCHAR;
                yield return VSCommand.FIRSTCHAR_EXT;

                yield return VSCommand.CUT;
                yield return VSCommand.PASTE;
            }
        }

        protected override VSCommand ConvertFromCommandId(uint id)
        {
            return (VSCommand)id;
        }

        protected override uint ConvertFromCommand(VSCommand command)
        {
            return (uint)command;
        }

        [Conditional("DISABLED")]
        private static void Trace(string line, params object[] args)
        {
            Utils.DebugPrintToOutput(line, args);
        }
    }
}