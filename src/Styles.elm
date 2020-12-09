module Styles exposing (styleNode)

import Html exposing (Html)


styleNode : Html msg
styleNode =
    Html.node "style" [] [ Html.text styles ]


styles : String
styles =
    """
html, body { margin: 0; padding: 0; }

body {
    background: #0f0f23;
    color: #ccc;
    font-family: "Source Code Pro", monospace;
    font-size: 14pt;
}

.container {
    padding: 20px;
}

.Layout {
    display: flex;
}

.InputWrapper {
    margin: 0 0 10pt 0;
}

.InputWrapper label {
    display: block;
    margin: 0 0 4pt 0;
}

.InputWrapper textarea {
    color: inherit;
    border: 1px solid #666;
    background: #10101a;
    padding: 0 2px;
    font-family: inherit;
    font-size: inherit;
    margin: 0;
    color: #ccc;
    min-height: 100px;
}

select {
    background: transparent;
    border: 0;
    font-family: inherit;
    font-size: inherit;
    margin: 0;
    padding: 0;
    color: #ccc;
    cursor: pointer;
    appearance: none;
}

select:hover,
select:focus {
    outline: none;
    color: #090;
}

button {
    background: transparent;
    border: 0;
    font-family: inherit;
    font-size: inherit;
    margin: 0;
    padding: 0;
    color: #090;
    cursor: pointer;
}

button:hover,
button:focus {
    color: #9f9;
}

button[disabled] {
    opacity: 0.4;
}

button[disabled]:hover,
button[disabled]:focus {
    color: #090;
}

.Solver {
    margin: 10pt;
}

.Solver_Title {
    color: #090;
}

.Solver textarea {
    min-height: auto;
    max-height: 80px;
    background: transparent;
    border: none;
    color: #ccc;
    border: 1px solid #666;
    background: #10101a;
    padding: 0 2px;
    font-size: 14pt;
    margin: 0;
    color: #ccc;
}

.Solver_Input textarea {
    opacity: 0.5;
    height: 2char;
}


"""
