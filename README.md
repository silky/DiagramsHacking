# Diagrams Hacking

## `RetroHaskell`

### Building

```
stack build :retro-haskell
```

## `ForceLayout`

### Building

```
export IMAGE_NAME=diagrams-hacking
docker build -t $IMAGE_NAME .
stack --docker --docker-image=$IMAGE_NAME build :force-layout
```

