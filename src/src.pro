TEMPLATE = app
QT = core
TARGET = qlalr
mac:CONFIG -= app_bundle
CONFIG += console

# Input
HEADERS += qlalr.h compress.h cppgenerator.h
SOURCES += qlalr.cpp compress.cpp cppgenerator.cpp qlalr_parser.cpp

PARSER_SOURCES = qlalr.qlalr
OTHER_FILES = qlalr.qlalr

make_parser.name = Generate the front-end
make_parser.input = qlalr.qlalr
make_parser.output = qlalr_parser.cpp
make_parser.commands = qlalr --qt --no-debug --no-lines qlalr.qlalr

QMAKE_EXTRA_TARGETS += make_parser
