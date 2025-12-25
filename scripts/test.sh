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

rm -rf out
mkdir -p out

7z e "$NAME.zip" -o./out

./out/$EXECUTABLE about

rm -rf out