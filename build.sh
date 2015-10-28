#!/usr/bin/env bash

mono .paket/paket.bootstrapper.exe
run .paket/paket.exe restore
mono packages/fake/tools/fake.exe build.fsx
