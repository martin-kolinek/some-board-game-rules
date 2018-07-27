(import ./. {}).shellFor {
  packages = p: [p.some-board-game-rules];
  withHoogle = true;
}