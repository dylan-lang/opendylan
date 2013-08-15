Life Example Project
====================

This is a DUIM implementation of John Conway's game of Life.  See
the **Help > Rules** menu item in the example for the rules of the
game.  You can choose a predefined pattern from the menu or draw
your own pattern on the window and see how long it lives.

The program is composed of the following parts:

``library.dylan``
    Contains the library and module definitions.

``util.dylan``
    Utility functions, macros, and constants used by the rest of the program.

``logic.dylan``
    The game rules are implemented here, in the function do-n-generations.

``events.dylan``
    Implements freehand pattern drawing on the Life window by handling various
    mouse events.

``patterns.dylan``
    Definitions for the patterns in the Patterns menu.

``frame.dylan``
    The DUIM frame definition, including command tables and most command
    implementations.

``display.dylan``
    Code for redisplaying the Life window.

``*.ico``
    Various icon files used by the Life user interface. *(Unused)*

``life-resources.rc``
    Resource file giving names to the various icons used. *(Unused)*

``README.rst``
    This file.
