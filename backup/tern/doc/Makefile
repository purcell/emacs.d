all: manual.html

manual.html: src/manual.txt src/asciidoc.conf
	PATH=../node_modules/codemirror/bin:$(PATH) asciidoc --backend=html5 -o manual.html src/manual.txt
