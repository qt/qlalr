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

#include "qlalr.h"

#include <QtCore/QSet>
#include <QtCore/QMap>
#include <QtCore/QString>
#include <QtCore/QVector>
#include <QtCore/QStack>
#include <QtCore/QStringList>
#include <algorithm>
#include <limits.h>
#include <cstdio>
#include <QtCore/QtDebug>

//#define QLALR_DEBUG 1

//
// digraph
//
struct Digraph::Node {
    State *state;
    QString nt;
    QVector<Digraph::Node *> out;
    int dfn;
    bool root;

    explicit Node(State *state, const QString &nt)
        : state(state), nt(nt), dfn(0), root(true)
    {
    }


    bool operator == (const Digraph::Node &other) const
    {
        return state == other.state && nt == other.nt;
    }

    bool operator != (const Digraph::Node &other) const
    {
        return ! operator ==(other);
    }
};

Digraph::Node *Digraph::node(State *state, const QString &nt)
{
    foreach (Digraph::Node *node, nodes) {
        if (node->state == state && node->nt == nt)
            return node;
    }
    Node *node = new Digraph::Node(state, nt);
    nodes.push_back(node);
    return node;
}

Digraph::~Digraph()
{
    qDeleteAll(nodes);
}

void Digraph::edge(Digraph::Node *source, Digraph::Node *target)
{
    target->root = false;
    if (! source->out.contains(target))
        source->out.push_back(target);
}

Token::Token()
    : assoc(Nonassoc), prec(-1)
{
}

Token::Token(const QString &name, const QString &spell, Assoc assoc, int prec)
    : name(name), spell(spell), assoc(assoc), prec(prec)
{
}

Rule::Rule(Grammar *G, const QString &lhs)
    : G(G), lhs(lhs), index(0), precToken(-1)
{
}

Grammar::Grammar()
    : goal(0)
{
    enterToken("$end", QLatin1String("end of file"), Token::Nonassoc, -1);
}

Grammar::~Grammar()
{
    qDeleteAll(rules);
}

const Token &Grammar::token(int index) const
{
    return tokens.at(index);
}

const Token &Grammar::token(const QString &name) const
{
    return tokens.at(indexOfToken(name));
}

int Grammar::indexOfName(const QString &name) const
{
    return names.indexOf(name);
}

bool Grammar::isTerminal(const QString &name) const
{
    return indexOfToken(name) != -1;
}

int Grammar::indexOfToken(const QString &name) const
{
    for (int i = 0; i < tokens.size(); ++i) {
        if (tokens.at(i).name == name)
            return i;
    }
    return -1;
}

bool Grammar::isNonTerminal(const QString &name) const
{
    return ! isTerminal(name);
}

void Grammar::enterToken(const QString &name, const QString &spell, Token::Assoc assoc, int prec)
{
    int index = indexOfToken(name);
    if (index == -1) {
        tokens.push_back(Token(name, spell, assoc, prec));
        names.push_back(name);
    } else {
        Token &tk = tokens[index];

        if (! spell.isEmpty())
            tk.spell = spell;

        tk.assoc = assoc; // always change the associativity

        if (prec != -1) {
            // fix this token's precedence.
            tk.prec = prec;
        }
    }
    if (! names.contains(name))
        names.push_back(name);
}

void Grammar::setStartSymbol(const QString &name)
{
    Q_ASSERT(! goal);

    if (isTerminal(name))
        fprintf(stderr, "expected a non-terminal symbol instead of `%s'\n",
                qPrintable(name));

    startRule("$accept");
    goal = currentRule();
    enterSymbol(name);
    enterSymbol("$end");
    finishRule();
}

Rule *Grammar::currentRule()
{
    return rules.back();
}

void Grammar::startRule(const QString &name)
{
    if (isTerminal(name))
        fprintf(stderr, "expected a non-terminal symbol instead of `%s'\n",
                qPrintable(name));
    int index = rules.size();
    rules.push_back(new Rule(this, name));
    currentRule()->index = index;

    if (! names.contains(name))
        names.append(name);
}

void Grammar::finishRule()
{
    Rule *rule = currentRule();
    if (rule->precToken == -1) {
        for (int i = rule->rhs.size() - 1; i != -1; --i) {
            if (isTerminal(rule->rhs.at(i))) {
                rule->precToken = indexOfToken(rule->rhs.at(i));
                break;
            }
        }
    }
    alternatives[rule->lhs].push_back(rule);
}

void Grammar::enterSymbol(const QString &name)
{
    currentRule()->rhs.push_back(name);
}

bool Grammar::isNullable(const QString &name) const
{
    return nullables.contains(name);
}

void Grammar::computeNullables()
{
    bool changed;

    do {
        changed = false;

        QHashIterator<QString, QVector<Rule *> > it(alternatives);
        while (it.hasNext()) {
            it.next();
            const QString &lhs = it.key();

            if (! isNullable(lhs)) {
                foreach (Rule *rule, it.value()) {
                    bool nullable = true;

                    foreach (const QString &sym, rule->rhs) {
                        nullable = isNullable(sym);

                        if (! nullable)
                            break;
                    }
                    if (nullable) {
                        changed = true;
                        nullables.insert(lhs);
                        break;
                    }
                }
            }
        }
    } while (changed);
}

void Grammar::dump(FILE *fp)
{
#ifdef QLALR_DEBUG
    foreach (const Token &tk, tokens) {
        fprintf(fp, "%%token %s \"%s\"\n", qPrintable(tk.name), qPrintable(tk.spell));
    }
    fprintf(fp, "\n%%start $accept\n");
    fprintf(fp, "\n%%%%\n");
#endif
    QHashIterator<QString, QVector<Rule *> > it(alternatives);
    while (it.hasNext()) {
        it.next();
        fprintf(fp, "\n%s", qPrintable(it.key()));
        bool fst = true;
        foreach (Rule *rule, it.value()) {
            fprintf(fp, "\n%7d %s", rule->index, fst ? ":" : "|");
            fst = false;
            foreach (const QString &symbol, rule->rhs) {
                fprintf(fp, " %s", qPrintable(symbol));
            }
        }
        fprintf(fp, "\n\t;\n");
    }
}

DottedItem::DottedItem()
    : rule(0), dot(0)
{
}

DottedItem::DottedItem(Rule *rule, int dot)
    : rule(rule), dot(dot)
{
}

bool DottedItem::isReduceItem() const
{
    return dot == rule->rhs.size();
}

bool DottedItem::operator == (const DottedItem &other) const
{
    return rule == other.rule && dot == other.dot;
}

bool DottedItem::operator != (const DottedItem &other) const
{
    return ! operator == (other);
}

bool DottedItem::operator < (const DottedItem &other) const
{
    if (rule->index == other.rule->index)
        return dot < other.dot;
    return rule->index < other.rule->index;
}

const QString &DottedItem::symbol() const
{
    static QString nil;
    return isReduceItem() ? nil : rule->rhs.at(dot);
}

bool DottedItem::isTerminal() const
{
    return rule->G->isTerminal(rule->rhs.at(dot));
}

bool DottedItem::isNonTerminal() const
{
    return rule->G->alternatives.find(rule->rhs.at(dot)) != rule->G->alternatives.end();
}

DottedItem DottedItem::next() const
{
    return DottedItem(rule, dot + 1);
}

State::State(const QVector<DottedItem> &kernel)
    : index(0)
    , kernel(kernel)
    , defaultReduce(0)
{
    QVector<DottedItem> wl;
    wl.reserve(kernel.size());
    foreach (const DottedItem &item, kernel)
        wl.push_back(item);
    while (! wl.empty()) {
        DottedItem item = wl.front();
        wl.pop_front();
        if (! items.contains(item)) {
            items.push_back(item);
            if (! item.isReduceItem()) {
                if (item.isNonTerminal()) {
                    foreach (Rule *r, item.rule->G->alternatives.value(item.symbol())) {
                        wl.push_back(DottedItem(r, 0));
                    }
                }
            }
        }
    }
    qSort(items);
}

bool State::isEmpty() const
{
    return kernel.isEmpty();
}

State *State::goTo(const QString &a) const
{
    for (int i = 0; i < labels.size(); ++i) {
        if (labels.at(i) == a)
            return next.at(i);
    }
    return 0;
}

Automaton::Automaton()
    : start(0)
{
}

Automaton::~Automaton()
{
    qDeleteAll(states);
}

State *Automaton::getState(const QVector<DottedItem> &kernel)
{
    Q_ASSERT(! kernel.isEmpty());
    foreach (State *state, states) {
        if (state->kernel == kernel) {
            return state;
        }
    }
    State *state = new State(kernel);
    state->index = states.size();
    states.push_back(state);
    return state;
}

const QVector<State *> &Automaton::next(State *state)
{
    if (! state->next.empty())
        return state->next;

    if (state->isEmpty()) {
        qDebug() << state->index << "is empty";
    }

    QVector<State *> result;
    Q_ASSERT(! state->isEmpty());
    QHash<QString, QVector<DottedItem> > buckets;
    foreach (const DottedItem &item, state->items) {
        if (! item.isReduceItem()) {
            DottedItem n = item.next();
            if (! buckets.value(item.symbol()).contains(n))
                buckets[item.symbol()].push_back(n);
        }
    }
    QHashIterator<QString, QVector<DottedItem> > it(buckets);
    while (it.hasNext()) {
        it.next();
        state->labels.push_back(it.key());
        QVector<DottedItem> kernel = it.value();
        qSort(kernel);
        result.push_back(getState(kernel));
    }
    state->next = result;
    return state->next;
}


void Automaton::gen(Grammar *G)
{
    G->computeNullables();
    computeLookback();
    reads(G);
    includesAndFollows(G);
    lookaheads();
    defaultReduceActions();
}

void Automaton::computeLookback()
{
    // (q, A->w) lookback (p, A)  iff p -- ...w... --> q
    foreach (State *p, states) {
        foreach (const DottedItem &item, p->items) {
            if (item.dot != 0)
                continue;
            DottedItem b = item;
            State *q = p;
            while (! b.isReduceItem()) {
                q = q->goTo(b.symbol());
                b = b.next();
            }
            const int size = q->lookbacks.value(b).size();
            q->lookbacks[b].insert(p);
            if (size != q->lookbacks.value(b).size()) {
#ifdef QLALR_DEBUG
                fprintf(stderr, "(%s ->", qPrintable(b.rule->lhs));
                foreach (const QString &sym, b.rule->rhs) {
                    fprintf(stderr, " %s", qPrintable(sym));
                }
                fprintf(stderr, ", %d) lookback (%d, %s)\n", q->index, p->index, qPrintable(b.rule->lhs));
#endif
            }
        }
    }
}

void Automaton::reads(Grammar *G)
{
    directReads(G);

    Digraph graph;
    foreach (State *q, states) {
        for (int i = 0; i < q->labels.size(); ++i) {
            if (G->isNonTerminal(q->labels.at(i))) {
                State *r = q->next.at(i);
                for (int j = 0; j < r->labels.size(); ++j) {
                    if (G->isNullable(r->labels.at(j))) {
                        Digraph::Node *s = graph.node(q, q->labels.at(i));
                        Digraph::Node *t = graph.node(r, r->labels.at(j));
#ifdef QLALR_DEBUG
                        fprintf(stderr, "(%d, %s) reads (%d, %s)\n",
                                s->state->index, qPrintable(s->nt),
                                t->state->index, qPrintable(t->nt));
#endif
                        graph.edge(s, t);
                    }
                }
            }
        }
    }

    reads_dfn = 0;

    foreach (Digraph::Node *node, graph.nodes) {
        if (node->root)
            visitReadNode(node);
    }

    foreach (Digraph::Node *node, graph.nodes)
        visitReadNode(node);
}

void Automaton::directReads(Grammar *G)
{
    // DR(p, A) = { t in T | p --- A ---> r --- t ---> }
    foreach (State *p, states) {
        for (int i = 0; i < p->labels.size(); ++i) {
            const QString &nt = p->labels.at(i);
            if (G->isNonTerminal(nt)) {
#ifdef QLALR_DEBUG
                fprintf(stderr, "DR(%d, %s) = {", p->index, qPrintable(nt));
#endif
                State *r = p->next.at(i);
                foreach (const QString &t, r->labels) {
                    if (G->isTerminal(t)) {
                        const int size = p->reads.value(nt).size();
                        p->reads[nt].insert(t);
                        if (size != p->reads.value(nt).size()) {
#ifdef QLALR_DEBUG
                            fprintf(stderr, " %s", qPrintable(t));
#endif
                        }
                    }
                }
#ifdef QLALR_DEBUG
                fprintf(stderr, " }\n");
#endif
            }
        }
    }
}

void Automaton::visitReadNode(Digraph::Node *node)
{
    if (node->dfn)
        return; // nothing to do

    const int N = ++reads_dfn;
    node->dfn = N;

    reads_stack.push(node);

    foreach (Digraph::Node *r, node->out) {
        visitReadNode(r);
        node->dfn = qMin(N, r->dfn);
        // merge the names
        if (*node != *r)
            node->state->reads[node->nt] += r->state->reads.value(r->nt);
    }

    if (node->dfn == N) {
        Digraph::Node *tos = reads_stack.top();
        do {
            tos = reads_stack.top();
            reads_stack.pop();
            tos->dfn = INT_MAX;
        } while (tos != node);
    }
}

void Automaton::includesAndFollows(Grammar *G)
{
    // (p, A) includes (p', B) iff B -> xAy, y ->* epsilon and p' --- x ---> p
    foreach (State *p, states) {
        p->follows = p->reads;
#ifdef QLALR_DEBUG
        foreach (const QString &sym, p->follows.keys()) {
            fprintf(stderr, "FOLLOW(%d, %s) = { %d }\n", p->index, qPrintable(sym), p->follows.value(sym).size());
        }
#endif
    }

    includes_dfn = 0;
    Digraph graph;

    foreach (State *p1, states) {
        foreach (const DottedItem &item, p1->items) {
            if (item.dot == 0) {
                DottedItem a = item;
                State *p = p1;
                while (! a.isReduceItem()) {
                    if (a.isNonTerminal()) {
                        DottedItem b = a.next();
                        for (; ! b.isReduceItem(); b = b.next()) {
                            if (! G->isNullable(b.symbol()))
                                break;
                        }
                        if (b.isReduceItem()) {
                            // found an include
                            Digraph::Node *s = graph.node(p, a.symbol());
                            Digraph::Node *t = graph.node(p1, b.rule->lhs);
#ifdef QLALR_DEBUG
                            fprintf(stderr, "(%d, %s) includes (%d, %s)\n",
                                    s->state->index, qPrintable(s->nt),
                                    t->state->index, qPrintable(t->nt));
#endif
                            graph.edge(s, t);
                        }
                    }
                    p = p->goTo(a.symbol());
                    a = a.next();
                }
            }
        }
    }

    foreach (Digraph::Node *node, graph.nodes) {
        if (node->root)
            visitIncludeNode(node);
    }

    foreach (Digraph::Node *node, graph.nodes) {
        visitIncludeNode(node);
    }
}

void Automaton::visitIncludeNode(Digraph::Node *node)
{
    if (node->dfn)
        return;

    const int N = ++includes_dfn;
    node->dfn = N;

    includes_stack.push(node);

    foreach (Digraph::Node *r, node->out) {
        visitIncludeNode(r);
        node->dfn = qMin(N, r->dfn);
        // merge the names
        if (*node != *r) {
            node->state->follows[node->nt] += r->state->follows.value(r->nt);
#ifdef QLALR_DEBUG
            fprintf(stderr, "follows(%d, %s) += follows(%d, %s)  |%d| |%d|\n",
                    node->state->index, qPrintable(node->nt),
                    r->state->index, qPrintable(r->nt),
                    node->state->follows.value(node->nt).size(),
                    r->state->follows.value(r->nt).size());
#endif
        }
    }

    if (node->dfn == N) {
        Digraph::Node *tos = includes_stack.top();
        do {
            tos = includes_stack.top();
            includes_stack.pop();
            tos->dfn = INT_MAX;
        } while (tos != node);
    }
}

//
// lookaheads
//
void Automaton::lookaheads()
{
    foreach (State *p, states) {
        foreach (const DottedItem &item, p->items) {
            if (! item.isReduceItem())
                continue;
            const QString &lhs = item.rule->lhs;
            foreach (State *q, p->lookbacks.value(item)) {
                p->lookaheads[item] += q->follows.value(lhs);
            }
        }
    }
}

void Automaton::defaultReduceActions()
{
    foreach (State *state, states) {
        int size = -1;
        Rule *def = 0;

        foreach (const DottedItem &item, state->items) {
            if (! item.isReduceItem())
                continue;

            const int la = state->lookaheads.value (item).size();
            if (! def || la > size) {
                def = item.rule;
                size = la;
            }
        }

        if (def) {
            Q_ASSERT (size >= 0);
            state->defaultReduce = def;
        }
    }
}

void Automaton::dump(Grammar *G, FILE *fp)
{
    foreach (State *state, states) {
        fprintf(fp, "\nstate %d\n", state->index);
        foreach (const DottedItem &item, state->kernel) {
            fprintf(fp, "\t%s:", qPrintable(item.rule->lhs));
            for (int i = 0; i < item.rule->rhs.size(); ++i) {
                if (i == item.dot)
                    fprintf(fp, " .");
                fprintf(fp, " %s", qPrintable(item.rule->rhs.at(i)));
            }
            if (item.isReduceItem()) {
                fprintf(fp, " .");
#ifdef QLALR_DEBUG
                fprintf(fp, " {");
                foreach (const QString &la, state->lookaheads.value(item))
                    fprintf(fp, " %s", qPrintable(la));
                fprintf(fp, " }");
#endif
            }
            fprintf(fp, " (%d)\n", item.rule->index);
        }

        bool fst = true;

        // shift/reduce conflicts
        foreach (const DottedItem &item, state->items) {
            if (item.isReduceItem()) {
                foreach (const QString &la, state->lookaheads.value(item)) {
                    if (state->goTo(la) != 0) {
                        if (fst)
                            fprintf(fp, "\n");
                        fst = false;
                        fprintf(fp, "\tshift/reduce conflict on token %s\n",
                                qPrintable(la));
                    }
                }
            }
        }

        // reduce/reduce conflicts
        foreach (const DottedItem &item, state->items) {
            if (item.isReduceItem()) {
                foreach (const QString &la, state->lookaheads.value(item)) {
                    foreach (const DottedItem &other, state->items) {
                        if (other != item && state->lookaheads.value(other).contains(la)) {
                            if (fst)
                                fprintf(fp, "\n");
                            fst = false;
                            fprintf(fp, "\treduce/reduce conflict between %d and %d on token %s\n",
                                    item.rule->index, other.rule->index, qPrintable(la));
                        }
                    }
                }
            }
        }

        fst = true;
        foreach (const DottedItem &item, state->items) {
            if (item.isReduceItem()) {
                if (item.rule == state->defaultReduce)
                    continue;

                if (fst)
                    fprintf(fp, "\n");
                fst = false;
                foreach (const QString &la, state->lookaheads.value(item)) {
                    fprintf(fp, "\t%-40s reduce using rule %d\n",
                            qPrintable(la), item.rule->index);
                }
            }
        }

        fst = true;
        for (int i = 0; i < state->labels.size(); ++i) {
            if (G->isTerminal(state->labels.at(i))) {
                if (fst)
                    fprintf(fp, "\n");
                fst = false;
                fprintf(fp, "\t%-40s shift, go to state %d\n",
                        qPrintable(state->labels.at(i)), state->next.at(i)->index);
            }
        }

        fst = true;
        for (int i = 0; i < state->labels.size(); ++i) {
            if (! G->isTerminal(state->labels.at(i))) {
                if (fst)
                    fprintf(fp, "\n");
                fst = false;
                fprintf(fp, "\t%-40s go to state %d\n",
                        qPrintable(state->labels.at(i)), state->next.at(i)->index);
            }
        }

        if (state->defaultReduce)
            fprintf(fp, "\n\tdefault reduce using rule %d (%s)\n",
                    state->defaultReduce->index, qPrintable(state->defaultReduce->lhs));
    }
}
