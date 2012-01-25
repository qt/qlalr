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

#ifndef QLALR_H
#define QLALR_H

#include <QtCore/QString>
#include <QtCore/QVector>
#include <QtCore/QHash>
#include <QtCore/QStringList>
#include <QtCore/QStack>
#include <QtCore/QSet>
#include <QtCore/QMap>

struct State;
struct Grammar;

//
// digraph
//
struct Digraph {
    struct Node;

    ~Digraph();

    Node *node(State *state, const QString &nt);
    void edge(Digraph::Node *source, Digraph::Node *target);

    QVector<Digraph::Node *> nodes;
};

struct Token {
    enum Assoc {
        Nonassoc,
        Left,
        Right
    };

    QString name;
    QString spell;
    Assoc assoc;
    int prec;

    Token();
    Token(const QString &name, const QString &spell, Assoc assoc, int prec);
};

struct Rule {
    Grammar *G;
    QString lhs;
    QStringList rhs;
    int index;
    int precToken;
    Rule(Grammar *G, const QString &lhs);
};

struct Grammar {
    QVector<Token> tokens;
    QVector<Rule *> rules;
    QHash<QString, QVector<Rule *> > alternatives;
    QSet<QString> nullables;
    QStringList names;
    Rule *goal;

    Grammar();
    ~Grammar();

    static const QString eofToken() { return QLatin1String("$eof"); }
    static const QString acceptSymbol() { return QLatin1String("$accept"); }

    const Token &token(int index) const;
    const Token &token(const QString &name) const;
    int indexOfName(const QString &name) const;
    int indexOfToken(const QString &name) const;
    bool isTerminal(const QString &name) const;
    bool isNonTerminal(const QString &name) const;
    bool isNullable(const QString &name) const;

    void enterToken(const QString &name, const QString &spell, Token::Assoc assoc, int prec);
    void enterSymbol(const QString &name);

    Rule *currentRule();
    void setStartSymbol(const QString &name);
    void startRule(const QString &name);
    void finishRule();

    void computeNullables();

    void dump(FILE *fp);
};

struct DottedItem {
    Rule *rule;
    int dot; // [0, N]

    DottedItem();
    DottedItem(Rule *rule, int dot);

    bool isReduceItem() const;
    bool isTerminal() const;
    bool isNonTerminal() const;

    const QString &symbol() const;
    DottedItem next() const;

    bool operator == (const DottedItem &other) const;
    bool operator != (const DottedItem &other) const;
    bool operator < (const DottedItem &other) const;
};

struct State {
    int index;
    QVector<DottedItem> kernel;
    QVector<DottedItem> items;
    QVector<State *> next;
    QStringList labels;
    QMap<DottedItem, QSet<State *> > lookbacks;
    QHash<QString, QSet<QString> > reads;
    QHash<QString, QSet<QString> > follows;
    QMap<DottedItem, QSet<QString> > lookaheads;
    Rule *defaultReduce;

    State(const QVector<DottedItem> &kernel);

    bool isEmpty() const;
    State *goTo(const QString &a) const;
};

struct Automaton {
    State *start;
    QVector<State *> states;

    Automaton();
    ~Automaton();

    State *getState(const QVector<DottedItem> &kernel);

    const QVector<State *> &next(State *state);

    void gen(Grammar *G);
    void dump(Grammar *G, FILE *fp);

private:
    void computeLookback();

    void reads(Grammar *G);
    void directReads(Grammar *G);
    void visitReadNode(Digraph::Node *node);

    void includesAndFollows(Grammar *G);
    void visitIncludeNode(Digraph::Node *node);

    void lookaheads();
    void defaultReduceActions();

private:
    int reads_dfn;
    int includes_dfn;
    QStack<Digraph::Node *> reads_stack;
    QStack<Digraph::Node *> includes_stack;
};


#endif // QLALR_H
