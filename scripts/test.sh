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

./out/$EXECUTABLE about
