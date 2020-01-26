# Snake

This is an example application for [sdl2](https://hackage.haskell.org/package/sdl2).

Click on the SDL module in the link for a tutorial on how to get started with sdl2.

## Build and Run

```sh
stack build && stack exec app
```

## Gameplay

![Gameplay gif](gameplay.gif)

## Want to tinker with this game? Here are a few ideas

- Do nothing when the user tries to move to the direction the snake is coming from instead of failing
- Change the snake's speed
- Add a key to restart the game
- Keep score and report to the user
- Report game over to the game window instead of to the console
