/****************************************************************************
**
** Copyright (C) 2012 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: http://www.qt-project.org/
**
** This file is part of the utils of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser General Public
** License version 2.1 as published by the Free Software Foundation and
** appearing in the file LICENSE.LGPL included in the packaging of this
** file. Please review the following information to ensure the GNU Lesser
** General Public License version 2.1 requirements will be met:
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights. These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU General
** Public License version 3.0 as published by the Free Software Foundation
** and appearing in the file LICENSE.GPL included in the packaging of this
** file. Please review the following information to ensure the GNU General
** Public License version 3.0 requirements will be met:
** http://www.gnu.org/copyleft/gpl.html.
**
** Other Usage
** Alternatively, this file may be used in accordance with the terms and
** conditions contained in a signed written agreement between you and Nokia.
**
**
**
**
**
** $QT_END_LICENSE$
**
****************************************************************************/

#ifndef CPPGENERATOR_H
#define CPPGENERATOR_H

#include "qlalr.h"
#include "compress.h"
#include <QtCore/QTextStream>

struct Grammar;
struct Automaton;

struct Options {
    QString filename;
    QString merged_output;
    QString table_name;
    QString decl_file_name;
    QString impl_file_name;
    QString token_prefix;
    QString decl_text;
    QString impl_text;
    int expected_shift_reduce;
    int expected_reduce_reduce;
    bool verbose: 1;
    bool no_lines: 1;
    bool no_debug: 1;
    bool qt: 1;

    Options()
        : expected_shift_reduce(0)
        , expected_reduce_reduce(0)
        , verbose(false)
        , no_lines(false)
        , no_debug(false)
        , qt(false)
    {
        table_name = "parser_table";
    }

    QString decls() const { return decl_text; }
    QString impls() const { return impl_text; }
};

class CppGenerator
{
public:
    CppGenerator(const Options *p, Grammar *grammar, Automaton *aut, bool verbose):
        flags (p),
        grammar (grammar),
        aut (aut),
        verbose (verbose),
        debug_info (false),
        copyright (false) {}

    void operator () ();

    bool debugInfo () const { return debug_info; }
    void setDebugInfo (bool d) { debug_info = d; }

    void setCopyright (bool t) { copyright = t; }

private:
    void generateDecl (QTextStream &out);
    void generateImpl (QTextStream &out);

    QString debugInfoProt() const;
    QString copyrightHeader() const;
    QString privateCopyrightHeader() const;

private:
    static QString startIncludeGuard(const QString &fileName);
    static QString endIncludeGuard(const QString &fileName);

    const Options *flags;
    Grammar *grammar;
    Automaton *aut;
    bool verbose;
    int accept_state;
    int state_count;
    int terminal_count;
    int non_terminal_count;
    bool debug_info;
    bool copyright;
    Compress compressed_action;
    Compress compressed_goto;
    QVector<int> count;
    QVector<int> defgoto;
};

#endif // CPPGENERATOR_H
