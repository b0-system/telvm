telvm – Get prompts on virtual machines (including Windows)
===========================================================

**WARNING** Telvm is work in progress.

Telvm gets you prompts on [QEMU] virtual machines from your Unix
terminal. In particular it has support for building and getting
prompts on Windows via [Windows Validation OS].

Ultimately it is just a managed `$XDG_DATA_DIR/telvm` directory, a
not so well informed oracle for QEMU invocations and a bit of ISO
file munging.

Telvm is distributed unders the ISC license.

[QEMU]: https://www.qemu.org/
[Windows Validation OS]: https://learn.microsoft.com/en-us/windows-hardware/manufacture/desktop/validation-os-overview?view=windows-11

## Installation

telvm can be installed with `opam`:

    opam install telvm

If you don't use `opam` consult the [`opam`](opam) file for build instructions.

You also need an install of [`qemu`] and for the Windows support of
[`curl`], [`unzip`] and [`cabextract`].

```
brew install qemu cabextract       # brew 
apt install qemu-system cabextract # debian
```

Additionally depending on the platform the following tools are needed:

* MacOS, `hdiutil` (system provided).
* Linux, `parted`, `losetup`, `mkfs.exfat` and `mkfs.fat`.

[`cabextract`]: https://repology.org/project/cabextract/versions
[`qemu`]: https://repology.org/project/qemu/versions
[`unzip`]: https://repology.org/project/unzip/versions
[`curl`]: https://repology.org/project/curl/versions

## Quick start

[Here](https://erratique.ch/software/telvm/doc/#quick_start).

## Documentation

The documentation can be consulted [online] or via `odig doc telvm`.

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: https://erratique.ch/software/telvm/doc
[OCaml forum]: https://discuss.ocaml.org
