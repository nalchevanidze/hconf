# Arguements:
readonly VERSION='0.3.3'
readonly REPO='nalchevanidze/hconf'
readonly APP_NAME='hconf'

# Colors
readonly ALERT='\033[0;31m'
readonly STD='\033[0m'
readonly INFO='\033[1;36m'
readonly WARN='\033[1;33m'
readonly SUCCSESS='\033[1;32m'

case "$(uname)" in
    "Darwin")
        OS=mac-os;;
    MINGW64_NT-*|MSYS_NT-*)
        OS=windows;;
    *)
        OS=linux
esac

## install bin
if [ -d "$HOME/bin"  ]; then
  BIN_DIR="$HOME/bin"
elif [ -d "$HOME/.local/bin"  ]; then
  BIN_DIR="$HOME/.local/bin"
else 
  BIN_DIR="$HOME/.local/bin"
  mkdir -p $BIN_DIR
fi

readonly URL="https://github.com/$REPO/releases/download/$VERSION/$APP_NAME-$OS.zip"


if [ -d .pkg-local ]; then
  rm -rf .pkg-local
fi

mkdir .pkg-local
cd .pkg-local

echo "\n${INFO}installing $APP_NAME $VERSION ${STD}\n" 
echo " - source: $URL";
curl -o "$APP_NAME.zip" -LO "$URL" -s

echo " - extracting"

unzip -q "$APP_NAME.zip"
chmod 777 ./$APP_NAME

echo " - copying binary to $BIN_DIR"

cp ./$APP_NAME $BIN_DIR/$APP_NAME
echo " - clean up"

cd ..  
rm -rf .pkg-local

echo "";

if command -v $APP_NAME
then
  echo "${SUCCSESS} installation succeeded for $($APP_NAME about).${STD}";
else 
  echo "add ${WARN}$BIN_DIR${STD} to enviroment PATH to execute $APP_NAME.";
fi

echo "";