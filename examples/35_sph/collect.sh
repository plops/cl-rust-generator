for i in plan/20261006_01_init/plan.md \
	     ../29_lowbandwidth/plan/20260929_01_lowbandwidth/prompt.txt \
	     ../32_cuda-rust/plan/misc/cuda-rust.md \
	     ../34_copernicus_radar/plan/20261005_01_gpu_speed/prompt.txt
do
    echo "// start of "$i
    cat $i
done

echo "du bist in einem docker container und hast zugriff auf eine a4000 gpu. du kannst mittels der datei in /tmp/.X11-unix als user ubuntu auf den X server auf dem host zugreifen. plane die implementierung der sph fluid simulation mit visualisierung. benutze NVIDIA/cuda-rust: cuda-oxide is a Rust-to-CUDA compiler. schlage vor welche anderen libraries verwendet werden sollten um die abhaengigkeiten gering zu halten aber dennoch keine zeit mit implementation von features zu verschwenden die einfach aus libraries kommen koennen. schlage zunaechst sets von features vor die das programm aufweisen sollte (mit defaults). schliesslich moechte ich von dir eine prompt.txt datei im schema der beispiele (mit expliziter aufgabe und einem generellen teil der die entwicklungsumgebung (docker) und vorgehensweisen (deepwiki, tests, enddoku) beschreibt). das programm soll im docker container in /workspace/src/cl-rust-generator/examples/35_sph. die beispiel dateien habe ich auf dem host gesammelt (wo /workspace/src/ /home/kiel/stage ist)"
