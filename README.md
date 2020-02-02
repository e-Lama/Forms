# Welcome to libgui

Form is program which allows you creating... forms. It uses libgui.

# Guarantees and Liability

   This document and all other parts of libgui are distributed in the
   hope they will be useful, but there is NO GUARANTEE that they are
   complete, accurate, non-infringing or usable for any purpose whatsoever.
   Contributors are NOT LIABLE for any damages that result from using
   Harbour in any ways. For more legal details, see [LICENSE](LICENSE).

   The information this document is subject to change without notice
   and does not represent any future commitment by the participants
   of the project.


# How to Participate

There are several ways to help making libgui better:

- You can give feedback/suggestions to developers on available
  channels, see [Harbour Links](#harbour-links).
- Submit a change:

  1. Fork Forms
  2. Create a branch: `git checkout -b my_mod`
  3. Commit your changes: `git commit -am "Added my feature"`
  4. Push to the branch: `git push origin my_mod`
  5. Open a Pull Request

# How to Get

## Get Harbour

To get and install Harbour, see [Harbour](https://harbour.github.io/)

## Get libgui

To get and install libgui, see [libgui](https://github.com/e-Lama/libgui)

## Download Forms

You'll need Git version control software installed on your system
and issue this command:

    git clone https://github.com/e-Lama/Forms.git

You can get subsequent updates using this command:

    git pull

# How to Build

> At now Forms isn't prepared to build and install on other systems than Linux.

## On Linux hosts

To build, setup paths in make.hbp and type:

    $ ./make.sh

To test it, setup paths in envset.sh and type:

    $ . envset.sh
    $ ./forms.bin

# Harbour Links

  * [Homepage](https://harbour.github.io/)
  * [Users' Mailing List](https://groups.google.com/group/harbour-users/) (English language)
  * [Development Mailing List](https://groups.google.com/group/harbour-devel/) (English language)
  * [Source code](https://github.com/harbour/core)
  * [Localization](https://www.transifex.com/projects/p/harbour/)
  * [Issues](https://github.com/harbour/core/issues)
  * Documents:
     * [hbmk2 documentation](utils/hbmk2/doc/hbmk2.en.md)
     * [hbrun documentation](contrib/hbrun/doc/hbrun.en.md)
     * [ChangeLog](ChangeLog.txt?raw=true)
     * Comparing [Harbour to xHarbour](doc/xhb-diff.txt?raw=true)
     * CA-Cl*pper 5.3 [online documentation](http://x-hacker.org/ng/53guide/)
     * Harbour [online documentation](http://harbour.github.io/doc/)
     * Harbour [internal documents](doc/)
     * [Wikipedia](https://en.wikipedia.org/wiki/Harbour_compiler)

---
This document is based on [this](https://github.com/harbour/core/blob/master/README.md), which was made by Viktor Szakats (vszakats.net/harbour)
