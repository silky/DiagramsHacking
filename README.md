# Diagrams Hacking

## Requirements

In a few of these experiments we use the `gsl-random` library, which requires
the external package `libgsl-dev` to be installed: `apt-get install
libgsl-dev` on Ubuntu works for me; something else might work for you!


## `RandomPlacement`

A few options for random placement of points. Take a look at the source code
for more details.

### Building

```
stack build :random-placement
```

### Running

```
stack exec -- random-placement -w 100 -h 100 -o out.svg
```


## `RetroHaskell`

![](examples/retro-haskell.png)

Made with:

```
stack exec -- retro-haskell -w 600 -h 600 -o retro-haskell.png
convert retro-haskell.png -crop 600x300+0+0 retro-haskell.png
```

### Building

```
stack build :retro-haskell
```


## `ForceLayout` (Incomplete)

### Building

```
export IMAGE_NAME=diagrams-hacking
docker build -t $IMAGE_NAME .
stack --docker --docker-image=$IMAGE_NAME build :force-layout
```

