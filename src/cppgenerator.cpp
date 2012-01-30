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


#include <QtCore/QtCore>

#include "cppgenerator.h"
#include "qlalr.h"

namespace {
QTextStream qout(stdout, QFile::WriteOnly);
QTextStream qerr(stderr, QFile::WriteOnly);
}

QString CppGenerator::copyrightHeader() const
{
    return QLatin1String(
                "/****************************************************************************\n"
                "**\n"
                "** Copyright (C) 2012 Nokia Corporation and/or its subsidiary(-ies).\n"
                "** All rights reserved.\n"
                "** Contact: http://www.qt-project.org/\n"
                "**\n"
                "** This file is part of the QtCore module of the Qt Toolkit.\n"
                "**\n"
                "** $QT_BEGIN_LICENSE:LGPL$\n"
                "** GNU Lesser General Public License Usage\n"
                "** This file may be used under the terms of the GNU Lesser General Public\n"
                "** License version 2.1 as published by the Free Software Foundation and\n"
                "** appearing in the file LICENSE.LGPL included in the packaging of this\n"
                "** file. Please review the following information to ensure the GNU Lesser\n"
                "** General Public License version 2.1 requirements will be met:\n"
                "** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.\n"
                "**\n"
                "** In addition, as a special exception, Nokia gives you certain additional\n"
                "** rights. These rights are described in the Nokia Qt LGPL Exception\n"
                "** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.\n"
                "**\n"
                "** GNU General Public License Usage\n"
                "** Alternatively, this file may be used under the terms of the GNU General\n"
                "** Public License version 3.0 as published by the Free Software Foundation\n"
                "** and appearing in the file LICENSE.GPL included in the packaging of this\n"
                "** file. Please review the following information to ensure the GNU General\n"
                "** Public License version 3.0 requirements will be met:\n"
                "** http://www.gnu.org/copyleft/gpl.html.\n"
                "**\n"
                "** Other Usage\n"
                "** Alternatively, this file may be used in accordance with the terms and\n"
                "** conditions contained in a signed written agreement between you and Nokia.\n"
                "**\n"
                "**\n"
                "**\n"
                "**\n"
                "**\n"
                "** $QT_END_LICENSE$\n"
                "**\n"
                "****************************************************************************/\n"
                "\n");
}

QString CppGenerator::privateCopyrightHeader() const
{
    return QLatin1String(
                "//\n"
                "//  W A R N I N G\n"
                "//  -------------\n"
                "//\n"
                "// This file is not part of the Qt API.  It exists for the convenience\n"
                "// of other Qt classes.  This header file may change from version to\n"
                "// version without notice, or even be removed.\n"
                "//\n"
                "// We mean it.\n"
                "//\n");
}

QString CppGenerator::startIncludeGuard(const QString &fileName)
{
    const QString normalized(QString(fileName).replace(QLatin1Char('.'), QLatin1Char('_')).toUpper());

    return QString::fromLatin1("#ifndef %1\n"
                               "#define %2\n").arg(normalized, normalized);
}

QString CppGenerator::endIncludeGuard(const QString &fileName)
{
    const QString normalized(QString(fileName).replace(QLatin1Char('.'), QLatin1Char('_')).toUpper());

    return QString::fromLatin1("#endif // %1\n").arg(normalized);
}

void CppGenerator::operator () ()
{
    // action table...
    state_count = aut->states.size ();
    terminal_count = grammar->tokens.size ();
    non_terminal_count = grammar->alternatives.size();

#define ACTION(i, j) table [(i) * terminal_count + (j)]
#define GOTO(i, j) pgoto [(i) * non_terminal_count + (j)]

    int *table = new int [state_count * terminal_count];
    ::memset (table, 0, state_count * terminal_count * sizeof (int));

    int *pgoto = new int [state_count * non_terminal_count];
    ::memset (pgoto, 0, state_count * non_terminal_count * sizeof (int));

    accept_state = -1;
    int shift_reduce_conflict_count = 0;
    int reduce_reduce_conflict_count = 0;

    foreach (State *state, aut->states) {
        const int q = state->index;

        QSet<QString> labelSet = QSet<QString>::fromList(state->labels);
        Q_ASSERT(labelSet.size() == state->labels.size());

        for (int i = 0; i < state->labels.size(); ++i) {
            const QString &key = state->labels.at(i);
            const int symbol = grammar->indexOfName(key);
            const int r = state->next.at(i)->index;

            Q_ASSERT(symbol != -1);
            Q_ASSERT(r < state_count);

            if (grammar->isNonTerminal(key)) {
                //Q_ASSERT (symbol >= terminal_count && symbol < grammar->names.size ());
                GOTO (q, symbol - terminal_count) = r;
            }

            else
                ACTION (q, symbol) = r;
        }

        foreach (const DottedItem &item, state->items) {
            if (! item.isReduceItem())
                continue;

            const int r = item.rule->index + 1;

            if (item.rule == grammar->goal)
                accept_state = q;

            foreach (const QString &s, state->lookaheads.value(item)) {
                int &u = ACTION (q, grammar->indexOfName(s));
                Q_ASSERT(grammar->isTerminal(s));

                if (u == 0)
                    u = - r;

                else if (u < 0) {
                    if (verbose)
                        qout << "*** Warning. Found a reduce/reduce conflict in state " << q << " on token ``" << s << "'' between rule "
                             << r << " and " << -u << endl;

                    ++reduce_reduce_conflict_count;

                    const int la = grammar->indexOfToken(s);
                    conflicts[q].insert(la, u);
                    conflicts[q].insert(la, -r);

                    u = qMax (u, -r);

                    if (verbose)
                        qout << "\tresolved using rule " << -u << endl;
                }

                else if (u > 0) {
                    const Token info_s = grammar->token(s);
                    if (item.rule->precToken != -1 && info_s.prec != -1) {
                        const Token info_r = grammar->token(item.rule->precToken);

                        Q_ASSERT(info_s.prec != -1);

                        if (info_r.prec > info_s.prec)
                            u = -r;
                        else if (info_r.prec == info_s.prec)
                        {
                            switch (info_r.assoc) {
                            case Token::Left:
                                u = -r;
                                break;
                            case Token::Right:
                                // shift... nothing to do
                                break;
                            case Token::Nonassoc:
                                u = 0;
                                break;
                            } // switch
                        }
                    }

                    else
                    {
                        const int la = grammar->indexOfToken(s);
                        conflicts[q].insert(la, u);
                        conflicts[q].insert(la, -r);

                        ++shift_reduce_conflict_count;

                        if (verbose)
                            qout << "*** Warning. Found a shift/reduce conflict in state " << q << " on token ``" << s << "'' with rule " << r << endl;
                    }
                }
            }
        }
    }

    if (shift_reduce_conflict_count || reduce_reduce_conflict_count)
    {
        if (shift_reduce_conflict_count != flags->expected_shift_reduce
                || reduce_reduce_conflict_count != flags->expected_reduce_reduce)
            qerr << "*** Conflicts: " << shift_reduce_conflict_count << " shift/reduce, " << reduce_reduce_conflict_count << " reduce/reduce" << endl;

        if (verbose)
            qout << endl << "*** Conflicts: " << shift_reduce_conflict_count << " shift/reduce, " << reduce_reduce_conflict_count << " reduce/reduce" << endl
                 << endl;
    }

    QBitArray used_rules (grammar->rules.count ());

    foreach (State *state, aut->states) {
        const int q = state->index;
        for (int j = 0; j < terminal_count; ++j)
        {
            int &u = ACTION (q, j);

            if (u < 0)
                used_rules.setBit (-u - 1);
        }
    }

    for (int i = 0; i < used_rules.count (); ++i)
    {
        if (! used_rules.testBit (i))
        {
            Rule *rule = grammar->rules.at(i);

            if (rule != grammar->goal) {
                fprintf(stderr, "Warning: rule useless in parser due to conflicts: %s:",
                        qPrintable(rule->lhs));
                foreach (const QString &sym, rule->rhs)
                    fprintf(stderr, " %s", qPrintable(sym));
                fprintf(stderr, "\n");
            }
        }
    }

    foreach (State *state, aut->states) {
        const int q = state->index;
        for (int j = 0; j < terminal_count; ++j) {
            int &u = ACTION (q, j);

            if (u >= 0)
                continue;

            Rule *rule = grammar->rules.at(- u - 1);

            if (state->defaultReduce == rule)
                u = 0;
        }
    }

    // ... compress the goto table
    defgoto.resize (non_terminal_count);
    for (int j = 0; j < non_terminal_count; ++j)
    {
        count.fill (0, state_count);

        int &mx = defgoto [j];

        for (int i = 0; i < state_count; ++i)
        {
            int r = GOTO (i, j);

            if (! r)
                continue;

            ++count [r];

            if (count [r] > count [mx])
                mx = r;
        }
    }

    for (int i = 0; i < state_count; ++i)
    {
        for (int j = 0; j < non_terminal_count; ++j)
        {
            int &r = GOTO (i, j);

            if (r == defgoto [j])
                r = 0;
        }
    }

    compressed_action (table, state_count, terminal_count);
    compressed_goto (pgoto, state_count, non_terminal_count);

    delete[] table;
    table = 0;

    delete[] pgoto;
    pgoto = 0;

#undef ACTION
#undef GOTO

    if (! flags->merged_output.isEmpty())
    {
        QFile f(flags->merged_output);
        if (! f.open (QFile::WriteOnly))
        {
            fprintf (stderr, "*** cannot create %s\n", qPrintable(flags->merged_output));
            return;
        }

        QTextStream out (&f);

        // copyright headers must come first, otherwise the headers tests will fail
        if (copyright)
        {
            out << copyrightHeader()
                << privateCopyrightHeader()
                << endl;
        }

        out << "// This file was generated by qlalr - DO NOT EDIT!\n";

        out << startIncludeGuard(flags->merged_output) << endl;

        if (copyright) {
            out << "#if defined(ERROR)" << endl
                << "#  undef ERROR" << endl
                << "#endif" << endl << endl;
        }

        generateDecl (out);
        generateImpl (out);
        out << flags->decls();
        out << flags->impls();
        out << endl;

        out << endIncludeGuard(flags->merged_output) << endl;

        return;
    }

    // default behaviour
    QString declFileName = flags->table_name.toLower () + QLatin1String("_p.h");
    QString bitsFileName = flags->table_name.toLower () + QLatin1String(".cpp");

    { // decls...
        QFile f (declFileName);
        f.open (QFile::WriteOnly);
        QTextStream out (&f);

        QString prot = declFileName.toUpper ().replace (QLatin1Char ('.'), QLatin1Char ('_'));

        // copyright headers must come first, otherwise the headers tests will fail
        if (copyright)
        {
            out << copyrightHeader()
                << privateCopyrightHeader()
                << endl;
        }

        out << "// This file was generated by qlalr - DO NOT EDIT!\n";

        out << "#ifndef " << prot << endl
            << "#define " << prot << endl
            << endl;

        if (copyright) {
            out << "#include <QtCore/qglobal.h>" << endl << endl;
            out << "QT_BEGIN_NAMESPACE" << endl << endl;
        }
        generateDecl (out);
        if (copyright)
            out << "QT_END_NAMESPACE" << endl;

        out << "#endif // " << prot << endl << endl;
    } // end decls

    { // bits...
        QFile f (bitsFileName);
        f.open (QFile::WriteOnly);
        QTextStream out (&f);

        // copyright headers must come first, otherwise the headers tests will fail
        if (copyright)
            out << copyrightHeader();

        out << "// This file was generated by qlalr - DO NOT EDIT!\n";

        out << "#include \"" << declFileName << "\"" << endl << endl;
        if (copyright)
            out << "QT_BEGIN_NAMESPACE" << endl << endl;
        generateImpl(out);
        if (copyright)
            out << "QT_END_NAMESPACE" << endl;

    } // end bits

    if (! flags->decl_file_name.isEmpty ())
    {
        QFile f (flags->decl_file_name);
        f.open (QFile::WriteOnly);
        QTextStream out (&f);
        out << flags->decls();
    }

    if (! flags->impl_file_name.isEmpty ())
    {
        QFile f (flags->impl_file_name);
        f.open (QFile::WriteOnly);
        QTextStream out (&f);
        out << flags->impls();
    }
}

QString CppGenerator::debugInfoProt() const
{
    QString prot = QLatin1String("QLALR_NO_");
    prot += flags->table_name.toUpper();
    prot += QLatin1String("_DEBUG_INFO");
    return prot;
}

void CppGenerator::generateDecl (QTextStream &out)
{
    out << "class " << flags->table_name << endl
        << "{" << endl
        << "public:" << endl
        << "  enum VariousConstants {" << endl;

    foreach (const Token &t, grammar->tokens) {
        QString name = t.name;
        int value = grammar->indexOfToken(t.name);

        if (name == QLatin1String ("$end"))
            name = QLatin1String ("EOF_SYMBOL");

        else if (name == QLatin1String ("$accept"))
            name = QLatin1String ("ACCEPT_SYMBOL");

        else
            name.prepend (flags->token_prefix);

        out << "    " << name << " = " << value << "," << endl;
    }

    out << endl
        << "    ACCEPT_STATE = " << accept_state << "," << endl
        << "    RULE_COUNT = " << grammar->rules.size () << "," << endl
        << "    STATE_COUNT = " << state_count << "," << endl
        << "    TERMINAL_COUNT = " << terminal_count << "," << endl
        << "    NON_TERMINAL_COUNT = " << non_terminal_count << "," << endl
        << endl
        << "    GOTO_INDEX_OFFSET = " << compressed_action.index.size () << "," << endl
        << "    GOTO_INFO_OFFSET = " << compressed_action.info.size () << "," << endl
        << "    GOTO_CHECK_OFFSET = " << compressed_action.check.size () << endl
        << "  };" << endl
        << endl
        << "  static const char  *const    spell [];" << endl
        << "  static const short             lhs [];" << endl
        << "  static const short             rhs [];" << endl;

    if (debug_info)
    {
        QString prot = debugInfoProt();

        out << endl << "#ifndef " << prot << endl
            << "  static const int     rule_index [];" << endl
            << "  static const int      rule_info [];" << endl
            << "#endif // " << prot << endl << endl;
    }

    out << "  static const short    goto_default [];" << endl
        << "  static const short  action_default [];" << endl
        << "  static const short    action_index [];" << endl
        << "  static const short     action_info [];" << endl
        << "  static const short    action_check [];" << endl;

    if (flags->glr_parser) {
        out << "  static const short  conflict_index [];" << endl
            << "  static const short   conflict_info [];" << endl
            << "  static const short  conflict_check [];" << endl
            << "  static const short       conflicts [];" << endl;
    }

    out << endl
        << "  static inline int nt_action (int state, int nt)" << endl
        << "  {" << endl
        << "    const int yyn = action_index [GOTO_INDEX_OFFSET + state] + nt;" << endl
        << "    if (yyn < 0 || action_check [GOTO_CHECK_OFFSET + yyn] != nt)" << endl
        << "      return goto_default [nt];" << endl
        << endl
        << "    return action_info [GOTO_INFO_OFFSET + yyn];" << endl
        << "  }" << endl
        << endl
        << "  static inline int t_action (int state, int token)" << endl
        << "  {" << endl
        << "    const int yyn = action_index [state] + token;" << endl
        << endl
        << "    if (yyn < 0 || action_check [yyn] != token)" << endl
        << "      return - action_default [state];" << endl
        << endl
        << "    return action_info [yyn];" << endl
        << "  }" << endl;

    if (flags->glr_parser) {
        out << endl
            << "  static inline const short *alternatives (int state, int token)" << endl
            << "  {" << endl
            << "    const int yyn = conflict_index [state] + token;" << endl
            << endl
            << "    if (yyn < 0 || conflict_check [yyn] != token)" << endl
            << "      return 0;" << endl
            << endl
            << "    return &conflicts [conflict_info [yyn]];" << endl
            << "  }" << endl;
    }

    out << "};" << endl
        << endl
        << endl;
}

static QList<int> stripped(const QList<int> &v)
{
    QMap<int, bool> m;
    foreach (int x, v)
        m.insert(x, true);
    return m.keys();
}

void CppGenerator::generateImpl (QTextStream &out)
{
    int idx = 0;

    out << "const char *const " << flags->table_name << "::spell [] = {";
    idx = 0;

    bool first_nt = true;

    foreach (const QString &t, grammar->names) {
        bool terminal = grammar->isTerminal(t);

        if (! (debug_info || terminal))
            break;

        if (idx)
            out << ',';

        if (! (idx % 10))
            out << endl << ' ';

        if (terminal) {
            QString spell = grammar->token(t).spell;

            if (spell.isEmpty ())
                out << ' '  << '0';
            else
                out << ' ' << '"' << spell << '"';
        } else {
            if (first_nt) {
                first_nt = false;
                QString prot = debugInfoProt();
                out << endl << "#ifndef " << prot << endl;
            }
            out << ' ' << '"' << t << '"';
        }
        ++idx;
    }

    if (debug_info)
        out << endl << "#endif // " << debugInfoProt() << endl;

    out << "};" << endl << endl;

    out << "const short " << flags->table_name << "::lhs [] = {";
    idx = 0;
    foreach (const Rule *rule, grammar->rules) {
        if (idx)
            out << ',';

        if (! (idx % 10))
            out << endl << ' ';

        out << ' ' << grammar->indexOfName(rule->lhs);
        ++idx;
    }
    out << "};" << endl << endl;

    out << "const short " << flags->table_name << "::rhs [] = {";
    idx = 0;
    foreach (const Rule *rule, grammar->rules) {
        if (idx)
            out << ',';

        if (! (idx % 10))
            out << endl << ' ';

        out << ' ' << rule->rhs.size ();
        ++idx;
    }
    out << "};" << endl << endl;

    if (debug_info) {
        QString prot = debugInfoProt();

        out << endl << "#ifndef " << prot << endl;
        out << "const int " << flags->table_name << "::rule_info [] = {";
        idx = 0;
        foreach (const Rule *rule, grammar->rules) {
            out << endl << "  ";

            if (idx)
                out << ", ";
            else
                out << "  ";

            out << grammar->indexOfName(rule->lhs);

            foreach (const QString &n, rule->rhs)
                out << ", " << grammar->indexOfName(n);
            ++idx;
        }
        out << "};" << endl << endl;

        out << "const int " << flags->table_name << "::rule_index [] = {";
        idx = 0;
        int offset = 0;
        foreach (const Rule *rule, grammar->rules) {
            if (idx)
                out << ',';

            if (! (idx % 10))
                out << endl << ' ';

            out << ' ' << offset;
            offset += rule->rhs.size () + 1;
            ++idx;
        }
        out << "};" << endl
            << "#endif // " << prot << endl << endl;
    }

    out << "const short " << flags->table_name << "::action_default [] = {";
    idx = 0;
    foreach (State *state, aut->states) {
        if (idx)
            out << ',';

        if (! (idx % 10))
            out << endl << ' ';

        if (state->defaultReduce)
            out << ' ' << state->defaultReduce->index + 1;
        else
            out << ' ' << '0';
        ++idx;
    }
    out << "};" << endl << endl;

    out << "const short " << flags->table_name << "::goto_default [] = {";
    for (int i = 0; i < defgoto.size (); ++i) {
        if (i)
            out << ',';

        if (! (i % 10))
            out << endl << ' ';

        out << ' ' << defgoto [i];
    }
    out << "};" << endl << endl;

    out << "const short " << flags->table_name << "::action_index [] = {";
    for (int i = 0; i < compressed_action.index.size (); ++i) {
        if (! (i % 10))
            out << endl << ' ';

        out << ' ' << compressed_action.index [i] << ',';
    }
    out << endl;
    for (int i = 0; i < compressed_goto.index.size (); ++i) {
        if (i)
            out << ',';

        if (! (i % 10))
            out << endl << ' ';

        out << ' ' << compressed_goto.index [i];
    }
    out << "};" << endl << endl;

    out << "const short " << flags->table_name << "::action_info [] = {";
    for (int i = 0; i < compressed_action.info.size (); ++i) {
        if (! (i % 10))
            out << endl << ' ';

        out << ' ' << compressed_action.info [i] << ',';
    }
    out << endl;
    for (int i = 0; i < compressed_goto.info.size (); ++i) {
        if (i)
            out << ',';

        if (! (i % 10))
            out << endl << ' ';

        out << ' ' << compressed_goto.info [i];
    }
    out << "};" << endl << endl;

    out << "const short " << flags->table_name << "::action_check [] = {";
    for (int i = 0; i < compressed_action.check.size (); ++i) {
        if (! (i % 10))
            out << endl << ' ';

        out << ' ' << compressed_action.check [i] << ',';
    }
    out << endl;
    for (int i = 0; i < compressed_goto.check.size (); ++i) {
        if (i)
            out << ',';

        if (! (i % 10))
            out << endl << ' ';

        out << ' ' << compressed_goto.check [i];
    }
    out << "};" << endl << endl;

    if (flags->glr_parser) {
        QList<QList<int> > processed;
        QList<int> offsets;
        QList<int> flat;
        flat.push_back(0);

        QMapIterator<int, QMultiMap<int, int> > it(conflicts);
        while (it.hasNext()) {
            it.next();
            const QMultiMap<int, int> &m = it.value();
            int last_la = -1;
            foreach (int la, m.keys()) {
                if (last_la == la)
                    continue;
                last_la = la;
                const QList<int> v = stripped(m.values(la));
                int offset = processed.indexOf(v);
                if (offset == -1) {
                    offset = flat.size();
                    flat += v;
                    flat.append(0);
                    processed.append(v);
                    offsets.append(offset);
                }
            }
        }
        QVector<int> data(state_count * terminal_count);
        it.toFront();
        while (it.hasNext()) {
            it.next();
            const int q = it.key();
            const QMultiMap<int, int> m = it.value();
            int last_la = -1;
            foreach (int la, m.keys()) {
                if (la == last_la)
                    continue;
                last_la = la;
                const QList<int> v = stripped(m.values(la));
                const int idx = processed.indexOf(v);
                Q_ASSERT(idx != -1);
                const int offset = offsets.at(idx);
                data[q * terminal_count + la] = offset;
            }
        }

        Compress k;
        k(data.data(), state_count, terminal_count);

        out << "const short " << flags->table_name << "::conflict_index [] = {";
        for (int i = 0; i < k.index.size(); ++i) {
            if (i)
                out << ',';
            if (! (i % 10))
                out << endl << ' ';
            out << ' ' << k.index.at(i);
        }
        out << "};" << endl
            << endl;

        out << "const short " << flags->table_name << "::conflict_info [] = {";
        for (int i = 0; i < k.info.size(); ++i) {
            if (i)
                out << ',';
            if (! (i % 10))
                out << endl << ' ';
            out << ' ' << k.info.at(i);
        }
        out << "};" << endl
            << endl;

        out << "const short " << flags->table_name << "::conflict_check [] = {";
        for (int i = 0; i < k.check.size(); ++i) {
            if (i)
                out << ',';
            if (! (i % 10))
                out << endl << ' ';
            out << ' ' << k.check.at(i);
        }
        out << "};" << endl
            << endl;

        out << "const short " << flags->table_name << "::conflicts [] = {";
        for (int i = 0; i < flat.size(); ++i) {
            if (i)
                out << ',';
            if (! (i % 10))
                out << endl << ' ';
            out << ' ' << flat.at(i);
        }
        out << "};" << endl
            << endl;
    }
}
