install:
    dune build
    cp -R _build/install/default/bin/sk ~/.local/bin/sk
