#!/usr/bin/env bash

mono .paket/paket.exe restore
mono packages/fake/tools/fake.exe build.fsx
