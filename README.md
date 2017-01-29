# accelerate-camera-sandbox
Program to do weird gpu accelerated visualizations with Haskell's accelerate library and web cams

Code borrowed heavily from the [accelerate-examples](https://github.com/AccelerateHS/accelerate-examples) fluid program

Relies on llvm-3.9 and opencv2 (for webcam), uses [accelerate-llvm](https://github.com/AccelerateHS/accelerate-llvm) so check out their install instructions to get all of that working. Couldn't get nvvm working as of January 29, 2017. Only tested on Ubuntu 16.04

After stack install, get usage with ```camera-sandbox-exe --help```

Demo video here:

[![Demo](https://img.youtube.com/vi/og7VCyBso3M/0.jpg)](https://www.youtube.com/watch?v=og7VCyBso3M)
