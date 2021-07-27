QT       += core

QT       -= gui

TARGET = c2obx
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

QMAKE_CFLAGS += -std=c99

SOURCES += main.cpp \
    chibicc/hashmap.c \
    chibicc/parse.c \
    chibicc/preprocess.c \
    chibicc/tokenize.c \
    chibicc/type.c \
    chibicc/unicode.c \
    chibicc/strings.c \
    chibicc/codegen.c

HEADERS += \
    chibicc/chibicc.h
