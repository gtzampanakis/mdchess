#!/bin/bash

find src/ -name '*.idr' | entr ./run.bash
