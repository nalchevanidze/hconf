
readonly ALERT='\033[0;31m'
readonly STD='\033[0m'
readonly INFO='\033[1;36m'
readonly WARN='\033[1;33m'
readonly SUCCSESS='\033[1;32m'
readonly VERSION='0.1.2'

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

readonly URL="https://github.com/nalchevanidze/hconf/releases/download/$VERSION/hconf-$OS.zip"

if [ -f /tmp/testfile ]; then
  rm -rf .local
fi

mkdir .local
cd .local

echo "\n${INFO}installing hconf-$VERSION)${STD}" 
echo " - source: $URL";
curl -o "hconf.zip" -LO "$URL" -s

echo " - extracting"

unzip -q hconf.zip
chmod 777 ./hconf

echo " - copying binary to $BIN_DIR"

cp ./hconf $BIN_DIR/hconf

echo " - clean up"

cd ..  
rm -rf .local

# reuprt status
echo "";

if ! command -v hconf &> /dev/null
then
  echo "add ${WARN}$BIN_DIR${STD} to enviroment PATH to execute hconf.";
else 
  echo "${SUCCSESS}honf-$(hconf version) installation succeeded.${STD}";
fi

echo "if you are mac user allow execution at: ${INFO}System Settings > Privacy & security${STD}";
echo "";