using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Linq;
using System.Text.RegularExpressions;
using VisualRust.Racer;

namespace VisualRust {
    // Ref: https://msdn.microsoft.com/en-us/library/ee334194.aspx "Walkthrough: Displaying Signature Help"

    class RustParameter : IParameter
    {
        public string     Documentation      { get; internal set; }
        public Span       Locus              { get; internal set; }
        public string     Name               { get; internal set; }
        public Span       PrettyPrintedLocus { get { return Locus; } }
        public ISignature Signature          { get; internal set; }
    }

    class RustSignature : ISignature
    {
        public ITrackingSpan                  ApplicableToSpan     { get; internal set; }
        public string                         Content              { get; internal set; }
        public IParameter                     CurrentParameter     { get; internal set; }
        public string                         Documentation        { get; internal set; }
        public ReadOnlyCollection<IParameter> Parameters           { get; internal set; }
        public string                         PrettyPrintedContent { get { return Content; } }
        public event EventHandler<CurrentParameterChangedEventArgs> CurrentParameterChanged;
    }

    [Export(typeof(ISignatureHelpSourceProvider))]
    [ContentType("rust")]
    [Name("rustSignature")]
    internal class TestSignatureHelpSourceProvider : ISignatureHelpSourceProvider
    {
        public ISignatureHelpSource TryCreateSignatureHelpSource(ITextBuffer textBuffer)
        {
            return new RustSignatureHelpSource(textBuffer);
        }
    }

    class RustSignatureHelpSource : ISignatureHelpSource
    {
        private readonly ITextBuffer TextBuffer;

        public RustSignatureHelpSource(ITextBuffer textBuffer)
        {
            Trace("ctor");
            TextBuffer = textBuffer;
        }

        public void Dispose()
        {
            Trace("Dispose");
        }

        public void AugmentSignatureHelpSession(ISignatureHelpSession session, IList<ISignature> signatures)
        {
            // Apply the signature help session to just our current cursor position (N.B.: not our trigger point!)
            SnapshotPoint snapPoint = session.TextView.Caret.Position.BufferPosition;
            ITextSnapshot snapshot = snapPoint.Snapshot;

            var line = snapPoint.GetContainingLine();
            var lineNo = line.LineNumber+1;
            var lineText = line.GetText();
            var tokens = Utils.LexString(lineText).Where(token => !WhitespaceTokenTypes.Contains(token.Type)).ToList();
            var column = snapPoint.Position - line.Start.Position;

            Trace("AugmentSignatureHelpSession:");
            Trace("      Line: '{0}'", lineText.Replace('\t',' '));
            Trace("  Position: {0}/\\", "".PadLeft(column, ' '));

            var iBeforeHoverToken = tokens.FindLastIndex(token => token.StopIndex < column);
            if (iBeforeHoverToken == -1)
            {
                session.Dismiss();
                return;
            }

            // 1. Search 'backwards' in an attempt to find the start of the argument list
            int parensEscaped = 0;
            int parensEscapedMax = 0;
            int commasPassedAtEscapedMax = 0;
            int commasPassedAtEscapedMaxMinusOne = 0;

            int? iSignatureHelpIdentToken = null;
            for (int iToken = iBeforeHoverToken; iSignatureHelpIdentToken == null && iToken >= 0; --iToken)
            {
                var token = tokens[iToken];
                switch (token.Type)
                {
                case RustLexer.RustLexer.LPAREN:
                    if (++parensEscaped > parensEscapedMax)
                    {
                        parensEscapedMax = parensEscaped;
                        commasPassedAtEscapedMaxMinusOne = commasPassedAtEscapedMax;
                        commasPassedAtEscapedMax = 0;
                    }
                    break;
                case RustLexer.RustLexer.RPAREN:
                    --parensEscaped;
                    break;
                case RustLexer.RustLexer.COMMA:
                    if (parensEscaped == parensEscapedMax) ++commasPassedAtEscapedMax;
                    break;
                case RustLexer.RustLexer.IDENT:
                    if (parensEscaped <= 0) break; // iToken is part of a peer's child expression, ignore it
                    if (iToken+1 < tokens.Count && tokens[iToken+1].Type == RustLexer.RustLexer.LPAREN) { iSignatureHelpIdentToken = iToken; break; } // ident(
                    //if (iToken+2 < tokens.Count && tokens[iToken+1].Type == RustLexer.RustLexer.MOD_SEP && tokens[iToken+2].Type == RustLexer.RustLexer.LT) { iBackTrackToken = iToken; break; } // ident::<
                    break;
                // TODO: <> 'params' for generics
                //case RustLexer.RustLexer.LT:
                //case RustLexer.RustLexer.GT:
                }
            }

            if (iSignatureHelpIdentToken == null) // couldn't find an opening ident( to signature match for
            {
                session.Dismiss();
                return;
            }

            // 2. Search forwards in an attempt to find the matching closing parens, if any, to complete the relevant span.
            parensEscaped = 0;
            int? iEndParenToken = null;
            for (int iToken=iBeforeHoverToken+1; iEndParenToken == null && iToken < tokens.Count; ++iToken)
            {
                var token = tokens[iToken];
                switch (token.Type)
                {
                case RustLexer.RustLexer.LPAREN:
                    --parensEscaped;
                    break;
                case RustLexer.RustLexer.RPAREN:
                    if (++parensEscaped == parensEscapedMax) iEndParenToken = iToken;
                    break;
                // TODO: <> 'params' for generics
                //case RustLexer.RustLexer.LT:
                //case RustLexer.RustLexer.GT:
                }
            }

            // 3. Construct applicable to range.
            var ident = tokens[iSignatureHelpIdentToken.Value].Text;
            // Currently including parens:
            var start = tokens[iSignatureHelpIdentToken.Value+1].StartIndex;
            var stop  = iEndParenToken == null ? lineText.Length-1 : tokens[iEndParenToken.Value].StopIndex;
            ITrackingSpan applicableToSpan = snapshot.CreateTrackingSpan(new Span(line.Start + start, stop - start + 1), SpanTrackingMode.EdgeInclusive, TrackingFidelityMode.Forward);

            ITextDocument document = null;
            snapshot?.TextBuffer?.Properties?.TryGetProperty(typeof(ITextDocument), out document);
            using (var temp = new TemporaryFile(snapshot.GetText()))
            {
                var racerCompleteArgs = string.Format("complete-with-snippet {0} {1} \"{2}\" \"{3}\"", lineNo, tokens[iSignatureHelpIdentToken.Value].StopIndex, document?.FilePath ?? temp.Path, temp.Path);
                var racerCompleteMatches = RacerSingleton.Run(racerCompleteArgs); // TODO: Cache the result of this for performance.
                foreach (var racerMatchLine in racerCompleteMatches.Split('\n'))
                {
                    RacerMatch m;
                    if (!RacerMatch.TryParse(racerMatchLine, RacerMatch.Type.CompleteWithSnippet, RacerMatch.Interface.Default, out m)) continue;
                    if (m.MatchString != ident) continue;
                    if (!LegalSignatureMatchTypes.Contains(m.MatchType)) continue;

                    switch (m.MatchType)
                    {
                    case MatchType.Function:
                        {
                            var signature = CreateRustFunctionSignature(m);
                            signature.ApplicableToSpan = applicableToSpan;
                            signature.CurrentParameter = commasPassedAtEscapedMaxMinusOne < signature.Parameters.Count ? signature.Parameters[commasPassedAtEscapedMaxMinusOne] : signature.Parameters.LastOrDefault();
                            signatures.Add(signature);
                        }
                        break;
                    default:
                        Trace("{0} not yet handled", m.MatchType ?? "null");
                        break;
                    }
                }
            }

            Trace("  scopes: {0}", parensEscapedMax);
            Trace("  commas: {0}", commasPassedAtEscapedMaxMinusOne);
        }

        public ISignature GetBestMatch(ISignatureHelpSession session)
        {
            return session.Signatures.FirstOrDefault();
        }



        private static class MatchType
        {
            // See CompletableLanguageElement for full list of MatchTypes
            public const string Function    = "Function";
            public const string Struct      = "Struct";
            public const string Impl        = "Impl";
            public const string Enum        = "Enum";
            public const string EnumVariant = "EnumVariant";
            public const string Type        = "Type";
            public const string Trait       = "Trait";
        }

        private static readonly int[] WhitespaceTokenTypes = new[]
        {
            RustLexer.RustLexer.WHITESPACE,
            RustLexer.RustLexer.COMMENT,
            RustLexer.RustLexer.DOC_COMMENT,
        };

        private static readonly HashSet<string> LegalSignatureMatchTypes = new HashSet<string>(new[]
        {
            MatchType.Function,    // generic ::<>s, ()s

            // I'm not sure yet which of the following actually are sane to match against
            MatchType.Struct,      // generic <>s?
            MatchType.Impl,        // generic <>s?
            MatchType.Enum,        // generic <>s? or ()s?
            MatchType.EnumVariant, // generic <>s? or ()s?
            MatchType.Type,        // generic <>s?
            MatchType.Trait        // generic <>s?
        });

        private static IEnumerable<string> ParseSnippetArgs(RacerMatch match)
        {
            // These preconditions should only be violated by coding errors
            Debug.Assert(match.MatchType == MatchType.Function, "ParseArgList should only be used on Function s");
            Debug.Assert(match.Snippet   != null,               "ParseArgList should only be used with complete-with-snippet matches");

            // These preconditions could be violated by racer being weird
            if (match.MatchString.Length+2 > match.Snippet.Length) return new string[0];
            if (!match.Snippet.StartsWith(match.MatchString)) return new string[0];
            if (match.Snippet[match.MatchString.Length] != '(') return new string[0];
            if (match.Snippet[match.Snippet.Length-1]   != ')') return new string[0];

            var argsText = match.Snippet.Substring(match.MatchString.Length+1, match.Snippet.Length - match.MatchString.Length-2);
            if (string.IsNullOrWhiteSpace(argsText)) return new string[0];
            var args = argsText.Split(',').Select(argText => {
                var trimmed = argText.Trim().TrimStart("${:0123456789".ToCharArray()).TrimEnd('}');
                return trimmed;
            }).ToList();
            return args;
        }

        // https://doc.rust-lang.org/reference/identifiers.html
        private const string IdentStart = "[_a-zA-Z]";       // TODO: Real ident start is "_" | XID_Start
        private const string IdentContinue = "[_a-zA-Z0-9]"; // TODO: Real ident continue is XID_Continue
        private const string Ident = "(?:"+IdentStart+IdentContinue+"*)";
        private static readonly Regex ReDocArgLine = new Regex(@"^[ \t]*\*[ \t]+((`(?<argName>"+Ident+")`)|(?<argName>"+Ident+@"))[ \t]+-[ \t]+(?<argDocs>.+)$", RegexOptions.Compiled | RegexOptions.CultureInvariant | RegexOptions.ExplicitCapture | RegexOptions.Multiline);

        /// <summary>
        /// Searches the documentation for parameter name -> short description documentation in the form:
        /// 
        /// <para>/// * `paramName` - Parameter documentation</para>
        /// </summary>
        private static Dictionary<string,string> ParseDocumentationArgs(RacerMatch match)
        {
            var result = new Dictionary<string,string>();
            foreach (Match reDocArg in ReDocArgLine.Matches(match.Documentation))
            {
                var docArgName = reDocArg.Groups["argName"].Value;
                var docArgDocs = reDocArg.Groups["argDocs"].Value;
                if (!result.ContainsKey(docArgName)) result.Add(docArgName, docArgDocs);

                Trace("  Doc Name: {0}", docArgName);
                Trace("  Doc Docs: {0}", docArgDocs);
                Trace("");
            }
            return result;
        }

        /// <summary>
        /// This attempts to find "arg" in a RacerMatch.Context/RacerSignature.Content string, and return a span referencing the whole definition - including any lifetimes, types, etc.
        /// </summary>
        private static Span? SearchContentForArgDefinition(string context, string arg)
        {
            // Find the argument in the signature content
            var startOfArguments = context.IndexOf('(');
            if (startOfArguments == -1) return null; // Content has no parameter list to search through!

            // Keep searching content until we find 'arg' all by it's lonesome (not as part of a larger identifier)
            var argIndex = context.IndexOf(arg, startOfArguments+1);
            for (;; argIndex = context.IndexOf(arg, argIndex+1))
            {
                if (argIndex == -1) return null; // ran out of 'arg' matches
                if (context.Length <= argIndex + arg.Length) return null; // out of buffer
                if (!"(, ".Contains(context[argIndex-1])) continue; // '?arg' was found, where ? isn't a proper word break char - keep looking
                if (!"), :".Contains(context[argIndex+arg.Length])) continue; // 'arg?' was found, where ? isn't a proper word break char - keep looking
                break; // match found
            }

            // Ok, we found an argument!  Now expand the span to include types lifetimes etc. by scanning for the next argument or end of argument list
            var argLen = arg.Length;
            for (int i=argIndex+argLen; i<context.Length; ++i, ++argLen)
            {
                char ch = context[i];
                if (ch == ',' || ch == ')') break;
            }

            return new Span(argIndex, argLen);
        }

        private static RustSignature CreateRustFunctionSignature(RacerMatch m)
        {
            // Create signature
            var signature = new RustSignature()
            {
                Content              = m.Context,
                Documentation        = m.Documentation, // XXX: This should have markdown partially stripped probably?
            };
            Trace("Found signature: {0}", signature.Content);
            var docArgs = ParseDocumentationArgs(m);
            var snipArgs = ParseSnippetArgs(m);

            // Add parameters to signature
            var sigParams = new List<IParameter>();
            foreach (var argName in snipArgs)
            {
                string argDocs;
                var rustParam = new RustParameter()
                {
                    Name          = argName,
                    Locus         = SearchContentForArgDefinition(signature.Content, argName) ?? new Span(0,0),
                    Documentation = docArgs.TryGetValue(argName, out argDocs) ? argDocs : argName, // TODO: Search m.Documentation for appropriate bits
                    Signature     = signature,
                };
                Trace("  Sig Name: {0}", rustParam.Name);
                Trace("  Sig Docs: {0}", rustParam.Documentation);
                Trace("     Locus: {0}", signature.Content);
                Trace("            {0}{1}", "".PadLeft(rustParam.Locus.Start, ' '), "".PadLeft(rustParam.Locus.Length, '^'));
                sigParams.Add(rustParam);
            }
            signature.Parameters = sigParams.AsReadOnly();
            return signature;
        }

        [Conditional("DISABLED")] // Comment out this line to enable tracing
        private static void Trace(string message, params object[] args)
        {
            Utils.DebugPrintToOutput("[RustSignatureHelpSource] "+message, args);
        }
    }
}
