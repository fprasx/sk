install:
    dune build
    cp -R _build/default/bin/main.exe ~/.local/bin/sk
    chmod +x ~/.local/bin/sk
