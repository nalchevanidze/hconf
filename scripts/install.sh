
readonly ALERT='\033[0;31m'
readonly STD='\033[0m'
readonly INFO='\033[1;36m'
readonly WARN='\033[1;33m'
readonly SUCCSESS='\033[1;32m'

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    readonly OS_NAME="linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    readonly OS_NAME="mac-os"
else
    echo "${ALERT}unsupported operating system${STD}";
    exit 1
fi

echo "installing hconf on: ${INFO}$OS_NAME${STD}"

readonly MIME_TYPE="Accept: application/octet-stream"
readonly GH_URL="https://github.com/nalchevanidze/hconf/releases/download/0.1.2/hconf-$OS_NAME.zip"

echo "downloading: ${INFO}$GH_URL${STD}";

mkdir .local
cd .local
curl -o "hconf.zip" -LO  "$GH_URL"

echo "extracting: ${INFO}hconf.zip${STD}"

unzip -q hconf.zip
.. cd 

## install bin
if [ -d "$HOME/bin"  ]; then
  LOCAL_BIN_DIR="$HOME/bin"
elif [ -d "$HOME/.local/bin"  ]; then
  LOCAL_BIN_DIR="$HOME/.local/bin"
else 
  LOCAL_BIN_DIR="$HOME/.local/bin"
  mkdir -p $LOCAL_BIN_DIR
fi

chmod 777 ./hconf
mv ./hconf $LOCAL_BIN_DIR/hconf
rm hconf.zip

# reuprt status
echo "";

if ! command -v hconf &> /dev/null
then
  echo "add ${WARN}$LOCAL_BIN_DIR${STD} to enviroment PATH to execute hconf.";
else 
  readonly VERSION=$(hconf version);
  echo "${SUCCSESS}honf-$VERSION installation succeeded.${STD}";
fi

echo "if you are mac user allow execution at: ${INFO}System Settings > Privacy & security${STD}";
echo "";