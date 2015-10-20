#!/bin/bash

mono .paket/paket.exe install
mono packages/fake/tools/fake.exe build.fsx
