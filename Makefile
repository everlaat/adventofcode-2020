all: elm

elm:
	elm make --optimize ./src/Main.elm --output ./docs/index.html;%