#!/usr/bin/env bash

if [ ! -f .paket/paket.exe ]; then
    mono .paket/paket.bootstrapper.exe
fi

mono .paket/paket.exe restore
mono packages/FAKE/tools/fake.exe "RunTests"
