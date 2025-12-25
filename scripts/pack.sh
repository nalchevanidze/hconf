NAME="hconf"
EXECUTABLE="$NAME"
rm -rf out

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
stack build hconf
mkdir -p out
cp "$(stack exec which $NAME)" ./out/$EXECUTABLE

if [ "$OS" != "windows" ]; then
  chmod +x ./out/$EXECUTABLE
fi

# Package
cd out
7z a ../hconf.zip .
cd ..