for i in 06_parallel_text/code 07_glutin_gl/code 03_glium/rs03_glium 05_imgui/rs05_imgui 01_gcd/rs01_gcd 08_glfw/code 04_azul/rs04_azul 02_webgcd/rs02_webgcd; do
    cd $i
    cargo clean
    cd ../..
done
    
