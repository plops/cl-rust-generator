for i in Cargo.toml src/*.rs
do
    echo "// start of "$i
    cat $i
done

echo "
 want to have simple code with low dependencies. review the source code and improve/refactor it if you can. try to  make it as small an readable as possible while still providing the ability to rescale the input image. i want to use this program to remote control x11 gui programs (e.g. entering text and pressing buttons in a webbrowser) for this to succeed we may need to introduce the method to render the text elements in a TUI environment and allow to create `macros` where we can perform certain operations like entering text when a text appears in a capture area."
