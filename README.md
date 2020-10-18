A dissection of L
=================

[![Creative Commons
Licence](https://i.creativecommons.org/l/by/4.0/88x31.png)](http://creativecommons.org/licenses/by/4.0/)

This work is licensed under a [Creative Commons
Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).


### Build instructions

I wrote this article in [Melt](https://opam.ocaml.org/packages/melt/),
an Ocaml-based latex generator. The build system uses Ocamlbuild. Both
of which are not very well maintained at the moment. So building will
be a bit difficult. Considering the situation, an ideal solution would
be to provide a `shell.nix` with all the right environment to build. I
haven't done this yet, though. So, right now, it would be a bit of an
adventure.

As a hint to what versions of everything is needed, I seem to have
built this project in April 2014. So whatever was the versions at the
time may work, though it is not impossible that innocent young me was
depending on a never-released master version of Melt. So, there can
definitely be some archaeology to do.
