# diagrams-wx

[![Hackage](https://img.shields.io/hackage/v/diagrams-wx.svg)](https://hackage.haskell.org/package/diagrams-wx)
[![Build Status](https://img.shields.io/travis/spinda/diagrams-wx/master.svg)](https://travis-ci.org/spinda/diagrams-wx)
[![Code Climate](https://img.shields.io/codeclimate/github/spinda/diagrams-wx.svg)](https://codeclimate.com/github/spinda/diagrams-wx)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/diagrams-wx.svg)](http://packdeps.haskellers.com/feed?needle=diagrams-wx)

A rendering backend for drawing diagrams in wxWidgets (using
[wxHaskell](https://wiki.haskell.org/WxHaskell)), built on top of the
[diagrams-cairo](https://github.com/diagrams/diagrams-cairo) backend.

## Documentation

See the Haddock page for the
[Diagrams.Backend.WX](https://hackage.haskell.org/package/diagrams-wx-0.1.0.0/docs/Diagrams.Backend.WX.html)
module.

## Building

Everything necessary to build [wxHaskell](https://wiki.haskell.org/WxHaskell)
and [diagrams-cairo](https://github.com/diagrams/diagrams-cairo) is necessary
to build `diagrams-wx`. Please see their respective documentation.

On Ubuntu, the setup instructions for these packages can be condensed down to
the following:

1. Install required external libraries:

     sudo apt-get install libwxgtk3.0-dev libwxgtk-media3.0-dev libcairo2-dev libpango1.0-dev

2. Ensure `happy` and `alex` are installed:

     # for Cabal
     cabal install happy
     cabal install alex
     # for stack
     stack install happy
     stack install alex

3. Ensure `gtk2hs-buildtools` is installed:

     # for Cabal
     cabal install gtk2hs-buildtools
     # for stack
     stack install gtk2hs-buildtools

Once all dependencies are set up, the `diagrams-wx` package can be built in the
usual way, via `cabal build` or `stack build` depending on your setup.

## Demo

A simple demo is available in the `demo/` directory. After building, it can be
launched with `cabal run diagrams-wx-demo` or `stack exec diagrams-wx-demo`.

## Legal

Copyright (C) 2016 Michael Smith.

diagrams-wx is licensed under the [BSD 3-clause license](/LICENSE).

