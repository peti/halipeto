%
% Halipeto -- Haskell static web page generator
% Copyright 2004 Andrew Cooke (andrew@acooke.org)
% Copyright 2007-2010 Peter Simons (simons@cryp.to)
%
%     This program is free software; you can redistribute it and/or modify
%     it under the terms of the GNU General Public License as published by
%     the Free Software Foundation; either version 2 of the License, or
%     (at your option) any later version.
%
%     This program is distributed in the hope that it will be useful,
%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%     GNU General Public License for more details.
%
%     You should have received a copy of the GNU General Public License
%     along with this program; if not, write to the Free Software
%     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%

\section{Introduction}

Halipeto divides the generation of a web site into three areas:

\begin{itemize}

\item The site structure.

\item The site contents.

\item The structure and presentation of individual pages.

\end{itemize}

The hope, as with any ``Content Management System'', is that
separating concerns in this way will simplify maintenance and support
more complex sites while maintaining a consistent ``feel''.

Halipeto is, I believe, simpler than other systems.  It does not
support dynamic generation of pages, for example.  But since it is
designed to be extended within Haskell, and has a simple, modular
architecture, it is very easy to add further functionality.

\subsection{Main Features}

Halipeto's advantages are:

\begin{itemize}

\item Simplicity.

\item Extensiblity --- you extend the system in Haskell rather than
having a separate language embedded inside templates; it is trivial to
associate your functions with template attributes.

\item Namespaces --- external data are organised in a hierarchical
namespace (so you might access a phone number with
``customers.\-uk.\-andrew.\-phone''); templates provide a simple way of
iterating over these values and using one value to access another.

\item Modular design --- take what functions you want and leave the
rest; no pervasive Monads used across the system apart from IO.

\item Built with Haskell --- simpler, more declarative source code
with less bugs; cross-platform support.

\end{itemize}

\subsection{Design}

From a programming / design point of view, the most interesting part
of this project have been the integration of Haskell with an untyped
interface (commands within templates are XML attributes; the only
argument is the attribute value, a simple string) and --- the other
side of the same coin --- how to keep functionality within Haskell,
rather than introducing a separate language within the template.

Central to the solution to the problems has been the idea of a
central, hierarchical dictionary that serves as a structured data
store.  Combined with

\begin{itemize}

\item a simple process for iterating over subtrees

\item a simple syntax for indirection (allowing dictionary values to
modify dictionary access)

\end{itemize}

this approach has proved surprisingly powerful.

\subsection{Motivation}

I started this project wanting something that:

\begin{itemize}

\item Let me generate web pages using templates and some kind of
database, so that I could separate the text, site structure, and
HTML/CSS code.

\item Was simple.  It has to run on Linux and Windows --- since
Haskell doesn't have a good library mechanism (one is being
developed), it had to be self contained.  Nothing in the design should
exclude SQL as the database, but text files are more likely to be
portable.

\item Could be customized easily.  The functionality should be
layered, extending a basic template engine with support for
structuring a site and, on top of that, one or more ways of
associating templates with content (ie any CMS should be a last,
replaceable layer).

\end{itemize}

Things I did not want included:

\begin{itemize}

\item Dynamic web pages.  The template engine could be used to
generate dyanmic pages, but I am not interested in doing this at the
moment.

\item The need to ``protect'' the user from Haskell.  While basic
functionality should be available to anyone, the full power of Haskell
must always be available.  Having said that, I hope the package is
simple enough for anyone to use.

\item A separate language or multi--staged evaluation.  I needed to
start generating web pages fairly quickly and did not want to learn
new technology.  Nor did I have the confidence that I could design a
language significantly better than Haskell.

\end{itemize}

\subsection{Documentation}

Haddock documentation describing the functions and data structures
exported by each module is available in the html directory.

If you have \LaTeX\ installed you can generate a PDF document from the
Halipeto files by typing:

\begin{verbatim}
latex Halipeto.tex
dvipdfm Halipeto
\end{verbatim}

This expects to find the haskell.sty file, which you can get from my
web site if you do not already have it.  That, in turn, expects to
find various style files that are installed by default if you use the
tetex package.

Halipeto.tex also requires the source files (*.lhs).

\subsection{Credits}

Thanks to people on the Haskell mailing list for replying to
questions.  Thanks also to the people that developed and released
HAXML.  Peter Simons contributed the patch for v2.0.

\subsection{Licencing Conditions}

This document and all the code it includes are copyright 204 Andrew
Cooke and released under the GPL (see www.gnu.org or the LICENCE and
COPYRIGHT files for full details).

The text and images in the demo directories are copyright 2004 Andrew
Cooke and are provided only as guidance.  They are not to be displayed
as content on a public web site.

The code taken from HaXml (in the FromHaxml directory) is covered by a
separate copyright and Licence --- see the LICENSE and COPYRIGHT files
in that directory.

Andrew Cooke can be contacted at andrew@acooke.org.

Peter Simons extended this code to use HaXml via new--fangled libraries
(version 2.0).  He probably understands how things work better than Andrew
does, these days, and can be contacted at simons@cryp.to.
