# Animated Popcorn

## What is it?
Popcorn fractals are created by simulating a discrete time dynamical system,
then viewing a 2-d histogram of their visited sites to create an image.

A good overview can be found [here](http://paulbourke.net/fractals/popcorn/)
(which has nicer colours than this repo owing to the thoughtfully chosen colour
maps).

This repository adds a time-element to the dynamical system, resulting in
animated popcorn fractals. Simulating these in Haskell efficiently requires some
mutability, motivating the use of the ST Monad.

## How to use
Haskell and Stack are used to build. To build, run:
```bash
stack build
```
then run the main program with:
```bash
stack exec -- animated-popcorn
```

## Results
A sample image and GIF follow below. Parameters are encoded in the filenames
using underscores as delimiters.

Image | Gif 
------------ | --------
![image](image_15e-2_512_256_(255,128,255).jpg "Static Image")|![gif](gif_1e-2_1.1_256_256_(128,255,255)_upsampled.gif "GIF output")
