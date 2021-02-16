%%
%% This is file `rapport.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% rapport.dtx  (with options: `class')
%% --------:| ----------------------------------------------------------------
%% rapport:| This package extends the article document class for students reports.
%%  Author:| Marie Pelleau
%%  E-mail:| marie.pelleau@univ-cotedazur.fr
%% License:| Released under the LaTeX Project Public License v1.3c or later
%%     See:| http://www.latex-project.org/lppl.txt
%% 
\NeedsTeXFormat{LaTeX2e}[1999/12/01]
\ProvidesClass{rapport}
    [2020/03/23 v1.00 porting the previous class (cls) to a documented source file (dtx).]
\LoadClass[a4paper, twoside]{article}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax


\RequirePackage{geometry,tikz,afterpage}
\RequirePackage{titling,fancyhdr}
\RequirePackage{graphicx}
\RequirePackage{enumitem}
\RequirePackage{natbib}
\RequirePackage{amsthm}

\def\@author{No authors given}
\def\@supervisor{No supervisor given}
\newcommand{\supervisor}[1]{\gdef\@supervisor{#1}}
\def\@universityname{Universit\'e C\^ote d'Azur}
\newcommand{\universityname}[1]{\gdef\@universityname{#1}}
\def\@universitylogo{logo-uca.png}
\newcommand{\universitylogo}[1]{\gdef\@universitylogo{#1}}
\def\@formation{Master Informatique}
\newcommand{\formation}[1]{\gdef\@formation{#1}}
\def\@formationlogo{logo_haut.png}
\newcommand{\formationlogo}[1]{\gdef\@formationlogo{#1}}
\def\@autresLogos{}
\newcommand{\autresLogos}[1]{\gdef\@autresLogos{#1}}

\def\@type{TER}
\newcommand{\type}[1]{\gdef\@type{#1}}
\def\pageblanche{
    \newpage
    \thispagestyle{empty}
    \mbox{}
    \newpage
}
\renewcommand{\maketitle}{
    \thispagestyle{empty}
    \newgeometry{left=2.5cm, right=2.5cm, top=2.5cm, bottom=2.5cm}

    \foreach \img in \@universitylogo {
      \includegraphics[height=2.5cm]{\img}
    }
    \hfill
    \includegraphics[height=2.5cm]{\@formationlogo}

    {\centering

    \vspace*{\fill}\noindent
    {\LARGE\textsc{Rapport de \@type}}

    \vspace{3cm}\noindent\rule{0.75\linewidth}{0.5mm}\\\vspace{0.5cm}
    {\Huge\textbf{\@title}}
    \\\vspace{0.5cm}\rule{0.75\linewidth}{0.5mm}

    \vspace{3cm}
    {\Large
    \begin{minipage}[t]{0.4\linewidth}
    \textit{Réalisé par}\\
    \foreach \aut in \@author {
      \textbf{\aut\\}
    }
    \end{minipage}
    \hfill
    \begin{minipage}[t]{0.4\linewidth}
    \begin{flushright}
    \textit{Encadré par}\\
    \foreach \sup in \@supervisor {
      \sup\\
    }
    \end{flushright}
    \end{minipage}
    }

    \if\@autresLogos\@empty \else
      \vfill
      \foreach \img in \@autresLogos {
        \includegraphics[height=1.5cm]{\img}\hfill
      }
    \fi

    \vspace*{\fill}\noindent
    {\large \@formation\\à \@universityname\\\@date}

    }

    \clearpage
    \restoregeometry
    \pageblanche
    \setcounter{page}{1}
}

%% 
%% Copyright (C) 2019-2020 by Marie Pelleau <marie.pelleau@univ-cotedazur.fr>
%% 
%% This work may be distributed and/or modified under the
%% conditions of the LaTeX Project Public License (LPPL), either
%% version 1.3c of this license or (at your option) any later
%% version.  The latest version of this license is in the file:
%% 
%% http://www.latex-project.org/lppl.txt
%% 
%% This work is "maintained" (as per LPPL maintenance status) by
%% Marie Pelleau.
%% 
%% This work is inspired by the exercice package of Arnaud Malapert.
%% 
%% This work consists of the file rapport.dtx and a Makefile.
%% Running "make" generates the derived files README, rapport.pdf and rapport.cls.
%% Running "make inst" installs the files in the user's TeX tree.
%% Running "make install" installs the files in the local TeX tree.
%% 
%%
%% End of file `rapport.cls'.
