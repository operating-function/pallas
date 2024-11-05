---
id: standard-library-intro
title: Standard Library
---

# Standard Library

## Finding additional standard library functions

The entire "standard library" is defined in the consecutively-numbered `sire/sire_<n>_<name>.sire` files. The files are quite readable and we encourage you to explore after becoming familiar with the documentation.

When first navigating a file, check out the list of imports at the top to see what _this_ file depends on.  Skimming the list of exports at the bottom (after reading any initial comment at the very top) can give you a sense of file content. Often, the `=?=` test cases can be very helpful.

### Summary of the standard library files

Below is a summary of each of the standard library files. Particularly helpful ones for a beginner are annotated with a 👍. Files that require a more advanced understanding are annotated with a ❗. Most of these files define lower-level dependencies that other higher-order (and easier to understand) subsequent functions rely on.

* [`sire_01_fan.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_01\_fan.sire) - Defines named wrappers around PLAN operations
* [`sire_02_bit.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_02\_bit.sire) - Booleans
* [`sire_03_nat.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_03\_nat.sire) - Natural numbers and operating on them
* [`sire_04_cmp.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_04\_cmp.sire) - Comparison, ordering and equality
* [`sire_05_row.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_05\_row.sire) - Rows and basic operations on them
* ❗ [`sire_06_rex.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_06\_rex.sire) - Representation for rex trees - mostly needed for macros.
* 👍 [`sire_07_dat.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_07\_dat.sire) - Data structures; rows, lists, maybe, either, etc.
* 👍 [`sire_10_str.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_10\_str.sire) - ASCII characters and strings
* [`sire_11_set.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_11\_set.sire) - Sets
* 👍 [`sire_12_tab.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_12\_tab.sire) - Tabs
* ❗ [`sire_13_exp.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_13\_exp.sire) - More rex and macro utilities
* ❗ [`sire_14_hax.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_14\_hax.sire) - Explains how the `#` rune is used for macros
* ❗ [`sire_15_pad.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_15\_pad.sire) - Bit-strings encoded as nats
* 👍 [`sire_16_bar.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_16\_bar.sire) - Byte-arrays and operations
* [`sire_17_sug.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_17\_sug.sire) - Syntactic sugar and convenience macros
* ❗ [`sire_18_pat.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_18\_pat.sire) - Pattern matching
* ❗ [`sire_19_bst.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_19\_bst.sire) - Binary search trees
* ❗ [`sire_20_prp.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_20\_prp.sire) - Sire properties
* [`sire_21_switch.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_21\_switch.sire) - Atomic switch
* ❗ [`sire_22_seed.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_22\_seed.sire) - Seed; serialization framework
* ❗ [`sire_23_repl.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_23\_repl.sire) - REPL utilities
* ❗ [`sire_24_rex.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_24\_rex.sire) - Rex
* [`sire_25_datatype.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_25\_datatype.sire) - Datacase/Record
* ⁉️ [`sire_26_compile.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_26\_compile.sire) - Backend of the sire compiler
* ⁉️ [`sire_27_sire.sire`](https://github.com/operating-function/pallas/blob/master/sire/sire\_27\_sire.sire) - Sire-in-sire; can be used to bootstrap itself

Taking a look at the list above, you can also get a sense of how the Sire source files start at the basics of wrapping PLAN and incrementally build on each other until the full system is realized.\
