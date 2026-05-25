#!/bin/bash

find src/ -name '*.idr' | entr pack typecheck
