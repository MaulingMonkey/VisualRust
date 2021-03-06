﻿using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace VisualRust.Racer
{
    public struct RacerMatch
    {
        /// <summary>
        /// The full text this match matches against.
        /// <para>Example: "read_line"</para>
        /// </summary>
        public string MatchString;

        /// <summary>
        /// Snippets appear to be equivalent to the MatchString, only containing parameter name information.
        /// May be null unless using a "With Snippet" racer completion.
        /// 
        /// <para>Example: "read_line(${1:buf})"</para>
        /// </summary>
        public string Snippet;

        /// <summary>
        /// The 1-based line number where this match was defined.
        /// <para>Example: "280"</para>
        /// </summary>
        public string LineNum;

        /// <summary>
        /// The column where this match was defined.
        /// <para>Example: "11"</para>
        /// </summary>
        public string CharNum;

        /// <summary>
        /// The file where this match was defined.
        /// <para>Example: "C:\Users\User\.rustup\toolchains\stable-x86_64-pc-windows-msvc\lib\rustlib\src\rust\src\libstd\io\stdio.rs"</para>
        /// </summary>
        public string PathDisplay;

        /// <summary>
        /// What kind of thing is being matched against.
        /// <para>Example: "Function"</para>
        /// </summary>
        public string MatchType;

        /// <summary>
        /// Seems to contain the signature of the match.
        /// <para>Example: "pub fn read_line(&amp;self, buf: &amp;mut String) -> io::Result&lt;usize&gt;"</para>
        /// </summary>
        public string Context;

        /// <summary>
        /// Contains the documentation for the match.  Likely contains e.g. Markdown formatting - but the rust string has been unescaped.
        /// May be null unless using a "With Snippet" racer completion.
        /// 
        /// <para>Example: "Locks this handle and reads a line of input into the specified buffer.\n\nFor detailed semantics of this method, ..."</para>
        /// </summary>
        public string Documentation;

        static readonly Regex ReDocsCodeSection = new Regex(@"(?<=^|\n)```(?<type>[a-zA-Z_0-9]*)\n(?<code>.*?)\n```(?=\n|$)", RegexOptions.Compiled | RegexOptions.ExplicitCapture | RegexOptions.Singleline);
        static readonly Regex ReDocsCodeInline = new Regex(@"`(?<code>.*?)`", RegexOptions.Compiled | RegexOptions.ExplicitCapture);
        static readonly Regex ReDocsSummaryLine = new Regex(@"^(?<docs>.*)", RegexOptions.Compiled | RegexOptions.ExplicitCapture);
        static readonly Regex ReDocsSummaryParas = new Regex(@"(?<docs>((^|(\r?\n))(?!#)(.*))*)", RegexOptions.Compiled | RegexOptions.ExplicitCapture);

        /// <summary>
        /// Documentation with some ancillary markdown such as ```code sections``` and `inline identifiers` stripped, for use in plaintext intellisense displays.
        /// Other formatting such as # Headers will remain in place.
        /// </summary>
        public string PlainDocumentation
        {
            get
            {
                var d = Documentation ?? "";
                d = ReDocsCodeSection.Replace(d, codeSection => codeSection.Groups["code"].Value);
                d = ReDocsCodeInline .Replace(d, codeInline  => codeInline .Groups["code"].Value);
                return d;
            }
        }

        public string PlainSummaryLine
        {
            get
            {
                var m = ReDocsSummaryLine.Match(PlainDocumentation);
                return m.Success ? m.Groups["docs"].Value : "";
            }
        }

        public string PlainSummaryParagraphs
        {
            get
            {
                var docs = PlainDocumentation;
                var m = ReDocsSummaryParas.Match(PlainDocumentation);
                return m.Success ? m.Groups["docs"].Value.TrimEnd() : "";
            }
        }



        public enum Type
        {
            /// <summary>
            /// Generated by "racer.exe complete"
            /// </summary>
            Complete,

            /// <summary>
            /// Generated by "racer.exe complete-with-snippet"
            /// </summary>
            CompleteWithSnippet
        }

        public enum Interface
        {
            /// <summary>
            /// Generated by "racer.exe --interface text ..."
            /// </summary>
            Text,

            /// <summary>
            /// Generated by "racer.exe --interface tab-text ..."
            /// </summary>
            TabText,

            /// <summary>
            /// Generated by "racer.exe ..." with no explicit "--interface ..." flags
            /// </summary>
            Default=Text
        }



        /// <summary>
        /// Try and parse a single "MATCH ..." line from "racer complete" or "racer complete-with-snippet"
        /// </summary>
        /// <param name="line">The "MATCH ..." text to try and parse.</param>
        /// <param name="type">Was "complete" or "complete-with-snippet" invoked?</param>
        /// <param name="interface_">Was "--interface [text|tab-text]" used?</param>
        /// <param name="match">The resulting parsed output.</param>
        /// <returns>True if the line was successfully parsed, false otherwise</returns>
        public static bool TryParse(string line, Type type, Interface interface_, out RacerMatch match)
        {
            // Emitting code: https://github.com/racer-rust/racer/blob/4d694e1e17f58bbf01e52fc152065d4bc06157e2/src/bin/main.rs#L216-L254
            match = new RacerMatch();

            if (!line.StartsWith(interface_ == Interface.TabText ? "MATCH\t" : "MATCH ")) return false;

            var seperator = interface_ == Interface.TabText ? '\t' : type == Type.CompleteWithSnippet ? ';' : ',';
            var pos = 6; // always exactly after the initial "MATCH\t" or "MATCH " prefix

            bool success = false;
            switch (type) {
            case Type.Complete:
                success
                    =  TryParseStringUntilSkip         (line, ref pos, seperator, out match.MatchString)
                    // no "snippet" field
                    && TryParseStringUntilSkip         (line, ref pos, seperator, out match.LineNum)
                    && TryParseStringUntilSkip         (line, ref pos, seperator, out match.CharNum)
                    && TryParseStringUntilSkip         (line, ref pos, seperator, out match.PathDisplay)
                    && TryParseStringUntilSkip         (line, ref pos, seperator, out match.MatchType)
                    && TryParseStringUntilEnd          (line, ref pos,            out match.Context);
                    // no "docs" field
                break;

            case Type.CompleteWithSnippet:
                var escSepeator = interface_ == Interface.TabText ? 't' : ';';
                success
                    =  TryParseStringUntilSkip         (line, ref pos, seperator, out match.MatchString)
                    && TryParseStringUntilSkip         (line, ref pos, seperator, out match.Snippet)
                    && TryParseStringUntilSkip         (line, ref pos, seperator, out match.LineNum)
                    && TryParseStringUntilSkip         (line, ref pos, seperator, out match.CharNum)
                    && TryParseStringUntilSkip         (line, ref pos, seperator, out match.PathDisplay)
                    && TryParseStringUntilSkip         (line, ref pos, seperator, out match.MatchType)
                    && TryParseEscStringUntilSkip      (line, ref pos, escSepeator, seperator, out match.Context)
                    && TryParseSpanDebugStringUntilSkip(line, ref pos, '\0',      out match.Documentation);
                break;
            }

            return success;
        }

        private static bool TryParseStringUntilSkip(string line, ref int position, char seperator, out string span)
        {
            int end = line.IndexOf(seperator, position);
            if (end == -1)
            {
                span = null;
                return false;
            }
            else
            {
                span = line.Substring(position, end-position);
                position = end+1; // skip seperator
                return true;
            }
        }

        private static bool TryParseStringUntilEnd(string line, ref int position, out string span)
        {
            span = line.Substring(position);
            return true;
        }

        private static bool TryParseEscStringUntilSkip(string line, ref int position, char escSeperator, char seperator, out string span)
        {
            var sb = new StringBuilder();
            int start = position;
            while (position < line.Length)
            {
                char ch = line[position++];
                if (ch == '\\')
                {
                    if (position >= line.Length)
                    {
                        span = null;
                        return false; // out of buffer
                    }
                    var ch2 = line[position++];
                    if (ch2 == escSeperator)
                    {
                        sb.Append(seperator);
                    }
                    else
                    {
                        sb.Append(ch);
                        --position;
                    }
                }
                else if (ch == seperator)
                {
                    span = sb.ToString();
                    return true;
                }
                else
                {
                    sb.Append(ch);
                }
            }
            span = null;
            return false;
        }

        private static bool TryParseSpanDebugStringUntilSkip(string line, ref int position, char seperator, out string span)
        {
            // String syntax ref: https://doc.rust-lang.org/reference/tokens.html

            var unescaped = new StringBuilder();

            span = null; // Lots of failure cases
            if (position >= line.Length) return false; // no more buffer left
            if (line[position] != '"') return false; // string didn't start with a quote

            int start = ++position; // skip quote
            int end = -1;

            while (end == -1)
            {
                if (position >= line.Length) return false; // string didn't end with a quote
                char ch = line[position++];
                switch (ch)
                {
                case '\\': // escape
                    {
                        if (position >= line.Length) return false;
                        char ch2 = line[position++];

                        switch (ch2)
                        {
                        case 'u':
                        case 'U':
                            {
                                // Skip up to 6 hex digits
                                int n;
                                for (n=0; n<6 && position < line.Length; ++n, ++position)
                                {
                                    char ch3 = line[position];
                                    var isHex = ('0' <= ch3 && ch3 <= '9')
                                        || ('a' <= ch3 && ch3 <= 'f')
                                        || ('A' <= ch3 && ch3 <= 'F');
                                    if (!isHex) break;
                                }
                                var value = Convert.ToInt32(line.Substring(position-n,n), 16);
                                unescaped.Append(char.ConvertFromUtf32(value));
                            }
                            break;
                        case 'x':
                            {
                                if (position+2 > line.Length) return false;
                                var value = Convert.ToInt32(line.Substring(position,2), 16);
                                unescaped.Append((char)value);
                                position += 2; // Skip "\x??"
                            }
                            break;
                        case '\\': unescaped.Append('\\'); break;
                        case 'n':  unescaped.Append('\n'); break;
                        case 'r':  unescaped.Append('\r'); break;
                        case ';':  unescaped.Append(';');  break; // Rust doesn't normally escape this, but racer sometimes does
                        case 't':  unescaped.Append('\t'); break;
                        case '"':  unescaped.Append('\"'); break;
                        case '\'': unescaped.Append('\''); break;
                        case '0':  unescaped.Append('\0'); break;
                        }
                    }
                    break;
                case '"': // terminating quote
                    end = position-1; // don't include quote
                    break;
                default:
                    unescaped.Append(ch);
                    break;
                }
            }

            if (seperator != '\0' && (position >= line.Length || line[position++] != seperator)) return false; // expected to skip a final seperator
            span = unescaped.ToString();
            return true;
        }
    }
}
