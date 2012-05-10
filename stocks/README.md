    rebar get-deps
    rebar compile
    cd rel
    rebar generate
    sh ./stocknode/bin/stocknode console

look at `http://localhost:8000/`
