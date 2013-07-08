Disclaimer
==========

This is a very alpha version of this project and so I don't claim that it does anything useful at the present time.

My helpful advice is: "If in doubt read the source code."

How it works
============

Linear Jazz Improvisation, [created by Dr Ed Byrne](http://byrnejazz.com), is a method to meaningful improvisation for musicians. You systematically create melodies, rehearse, and then edit later by keeping what you like when you play live.

Program Input
-------------

This program takes a music chart with an optional melody, which it then methodically targets with chromatic targeting groups you provide by extracting and chromatically tageting four principle lines: the melody (if supplied), the bass and the two guide tone lines.

For example, this is the song Autumn Leaves with a simplified melody.

    Gm

    Cm7 /   /   /   | F7  /   /   /   | Bb^  /   /   /   | Eb^  /   /   /   |
    A0  /   /   /   | D7  /   /   /   | Gm   /   /   /   | Gm   /   /   /   ||

    Cm7 /   /   /   | F7  /   /   /   | Bb^  /   /   /   | Eb^  /   /   /   |
    A0  /   /   /   | D7  /   /   /   | Gm   /   /   /   | Gm   /   /   /   ||

    A0  /   /   /   | D7  /   /   /   | Gm   /   /   /   | Gm   /   /   /   |
    Cm7 /   /   /   | F7  /   /   /   | Bb^  /   /   /   | Eb^  /   /   /   ||

    A0  /   /   /   | D7  /   /   /   | Gm7  /   C7  /   | Fm7  /   Bb7 /   |
    Eb^ /   /   /   | A0  /   D7  /   | Gm   /   /   /   | Gm   /   /   /   |||

    Eb1             | Eb1             | D1               | D1               |
    C1              | C1              | Bb1              | Bb1              ||

    Eb1             | Eb1             | D1               | D1               |
    C1              | C1              | Bb1              | Bb1              ||

    A1              | A1              | Bb1              | Bb1              |
    C1              | C1              | D1               | D1               ||

    Eb1             | A1              | D1               | G1               |
    C1              | A1              | G1               | G1               |||

The program then outputs [Lilypond](http://www.lilypond.org/) notation for you to rehearse with.

Usage
-----

There are two executables `lji` and `lji-exercises` both have the same usage apart from a small detail. The latter, `lji-exercises`, will ignore the first command and targeting information to create all the exercises you need to practice LJI as specified in Dr Byrne's first book.
