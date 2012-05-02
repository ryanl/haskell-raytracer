# Run tests

if [ $(ghc -e test_join src/Image.hs) != True ]; then
    echo "Image.hs: test failed. Build aborted"
    exit
fi
if [ $(ghc -e test_create_ppm src/Image.hs) != True ]; then
    echo "Image.hs: test failed. Build aborted"
    exit
fi

echo "All tests passed."

mkdir -p obj/
ghc --make src/Main.hs -isrc/ -odir obj/ -o raytracer
# -odir
