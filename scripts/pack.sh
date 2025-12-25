NAME="hconf"
EXECUTABLE="$NAME"

case "$(uname)" in
    "Darwin")
        OS=mac-os;;
    MINGW64_NT-*|MSYS_NT-*)
        OS=windows;;
    *)
        OS=linux
esac

if [ "$OS" == "windows" ]; then
  EXECUTABLE="$NAME.exe"
fi

# Build
rm -rf out
mkdir -p out
stack build hconf
cp "$(stack exec which $NAME)" ./out/$EXECUTABLE

if [ "$OS" != "windows" ]; then
  chmod +x ./out/$EXECUTABLE
fi

# Package
cd out
7z a ../hconf.zip .
cd ..
rm -rf out
