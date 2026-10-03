for i in server/Cargo.toml server/src/*.rs \
			   client/Cargo.toml client/src/*.rs \
			   android_client/rust-core/src/*.rs android_client/android-app/app/src/main/java/de/lbw/client/*.kt
do
    echo "// start of "$i
    cat $i
done

#echo "is it possible to port the client software to iphone? what approach would you suggest, a rust kernel or porting everything? how can we establish the ssh tunnel on the iphone?"

echo "this program is a remote desktop application for extremely low bandwidth use. it works but i find that the code base is too complex. review the code with the goal to remove unneccesary code and convert it into an absolutely minimal viable product. while i still want to keep dependencies low, i prefer adding dependencies if that means the code base gets simpler."
