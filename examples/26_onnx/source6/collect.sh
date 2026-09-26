for i in src/*.rs *.toml scripts/*.*
do
    echo "// start of "$i
    cat $i
done

#echo 'I want to have simple code with low dependencies. review the source code and improve/refactor it if you can. try to  make it as small an readable as possible while still providing the current functionality (rescale, pan, inject x11 events, render text of screenshot in console). The automation part currently struggles with recognizing the strings, e.g. in rules.metaai.toml there we should look for the string `Ask Meta AI...`. However, sometimes this reads as `Ask Meta Al..`, i.e. the `I` character reads as `l` and one of the points is missing. Currently, I just check for the substring `Ask Meta A` but it would be nicer to have a comparison function that takes into consideration the likelihood of missidentifying simila glyphs or skipping small ones or being confused by the `|` sign indicating the cursor. Another problem is that I currently manually have to give firefox the focus by placing the mouse cursor into the window. Otherwise, the text is not being injected.'
