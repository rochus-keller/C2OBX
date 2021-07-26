#-------------------------------------------------
#
# Project created by QtCreator 2021-07-24T18:28:28
#
#-------------------------------------------------

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
    chibicc/chibicc.h \
    Testdaten/sdl2/begin_code.h \
    Testdaten/sdl2/close_code.h \
    Testdaten/sdl2/SDL_assert.h \
    Testdaten/sdl2/SDL_atomic.h \
    Testdaten/sdl2/SDL_audio.h \
    Testdaten/sdl2/SDL_bits.h \
    Testdaten/sdl2/SDL_blendmode.h \
    Testdaten/sdl2/SDL_clipboard.h \
    Testdaten/sdl2/SDL_config_android.h \
    Testdaten/sdl2/SDL_config_iphoneos.h \
    Testdaten/sdl2/SDL_config_macosx.h \
    Testdaten/sdl2/SDL_config_minimal.h \
    Testdaten/sdl2/SDL_config_pandora.h \
    Testdaten/sdl2/SDL_config_psp.h \
    Testdaten/sdl2/SDL_config_windows.h \
    Testdaten/sdl2/SDL_config_winrt.h \
    Testdaten/sdl2/SDL_config_wiz.h \
    Testdaten/sdl2/SDL_config.h \
    Testdaten/sdl2/SDL_copying.h \
    Testdaten/sdl2/SDL_cpuinfo.h \
    Testdaten/sdl2/SDL_egl.h \
    Testdaten/sdl2/SDL_endian.h \
    Testdaten/sdl2/SDL_error.h \
    Testdaten/sdl2/SDL_events.h \
    Testdaten/sdl2/SDL_filesystem.h \
    Testdaten/sdl2/SDL_gamecontroller.h \
    Testdaten/sdl2/SDL_gesture.h \
    Testdaten/sdl2/SDL_haptic.h \
    Testdaten/sdl2/SDL_hints.h \
    Testdaten/sdl2/SDL_joystick.h \
    Testdaten/sdl2/SDL_keyboard.h \
    Testdaten/sdl2/SDL_keycode.h \
    Testdaten/sdl2/SDL_loadso.h \
    Testdaten/sdl2/SDL_locale.h \
    Testdaten/sdl2/SDL_log.h \
    Testdaten/sdl2/SDL_main.h \
    Testdaten/sdl2/SDL_messagebox.h \
    Testdaten/sdl2/SDL_metal.h \
    Testdaten/sdl2/SDL_misc.h \
    Testdaten/sdl2/SDL_mouse.h \
    Testdaten/sdl2/SDL_mutex.h \
    Testdaten/sdl2/SDL_name.h \
    Testdaten/sdl2/SDL_opengl_glext.h \
    Testdaten/sdl2/SDL_opengl.h \
    Testdaten/sdl2/SDL_opengles.h \
    Testdaten/sdl2/SDL_opengles2_gl2.h \
    Testdaten/sdl2/SDL_opengles2_gl2ext.h \
    Testdaten/sdl2/SDL_opengles2_gl2platform.h \
    Testdaten/sdl2/SDL_opengles2_khrplatform.h \
    Testdaten/sdl2/SDL_opengles2.h \
    Testdaten/sdl2/SDL_pixels.h \
    Testdaten/sdl2/SDL_platform.h \
    Testdaten/sdl2/SDL_power.h \
    Testdaten/sdl2/SDL_quit.h \
    Testdaten/sdl2/SDL_rect.h \
    Testdaten/sdl2/SDL_render.h \
    Testdaten/sdl2/SDL_revision.h \
    Testdaten/sdl2/SDL_rwops.h \
    Testdaten/sdl2/SDL_scancode.h \
    Testdaten/sdl2/SDL_sensor.h \
    Testdaten/sdl2/SDL_shape.h \
    Testdaten/sdl2/SDL_stdinc.h \
    Testdaten/sdl2/SDL_surface.h \
    Testdaten/sdl2/SDL_system.h \
    Testdaten/sdl2/SDL_syswm.h \
    Testdaten/sdl2/SDL_test_assert.h \
    Testdaten/sdl2/SDL_test_common.h \
    Testdaten/sdl2/SDL_test_compare.h \
    Testdaten/sdl2/SDL_test_crc32.h \
    Testdaten/sdl2/SDL_test_font.h \
    Testdaten/sdl2/SDL_test_fuzzer.h \
    Testdaten/sdl2/SDL_test_harness.h \
    Testdaten/sdl2/SDL_test_images.h \
    Testdaten/sdl2/SDL_test_log.h \
    Testdaten/sdl2/SDL_test_md5.h \
    Testdaten/sdl2/SDL_test_memory.h \
    Testdaten/sdl2/SDL_test_random.h \
    Testdaten/sdl2/SDL_test.h \
    Testdaten/sdl2/SDL_thread.h \
    Testdaten/sdl2/SDL_timer.h \
    Testdaten/sdl2/SDL_touch.h \
    Testdaten/sdl2/SDL_types.h \
    Testdaten/sdl2/SDL_version.h \
    Testdaten/sdl2/SDL_video.h \
    Testdaten/sdl2/SDL_vulkan.h \
    Testdaten/sdl2/SDL.h
