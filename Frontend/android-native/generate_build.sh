# !/bin/bash
export NODE_OPTIONS=--openssl-legacy-provider

# Enable case-insensitive matching
shopt -s nocasematch

user="nammaYatri"
partner="nammaYatriPartner"
success=true

# Build options - NyUserProdDebug | YsUserProdDebug | YatriUserProdDebug | NyDriverProdDebug | YsDriverProdDebug | YatriDriverProdDebug 

print_status() {
    status=$1
    if [ "$status" == "success" ]; then
        echo -e "\e[32m✔ Passed\e[0m"
    elif [ "$status" == "failure" ]; then
        echo -e "\e[31m✘ Failed\e[0m"
        exit 1
    else
        echo "Unknown status"
        exit 1
    fi
}

if [[ "$1" =~ .*"ny".* ]]; then
    user="nammaYatri"
    partner="nammaYatriPartner"
elif [[ "$1" =~ .*"yatri".* ]]; then
    user="yatri"
    partner="yatriPartner"
elif [[ "$1" =~ .*"ys".* ]]; then
    user="jatriSaathi"
    partner="yatriSathiPartner"
fi



if [[ "$1" =~ .*"User".* ]]; then
    echo " ---------- Customer prod:android :- --------------"
    cd ../ui-customer || print_status "failure"
    npm install && npm install purescript
    npm run prod:android || print_status "failure"
    cd ./../android-native || print_status "failure"
    echo " ---------- Copy index_bundle.js Customer :- --------------"
    mkdir -p "app/src/user/$user/assets/juspay" || print_status "failure"
    rm -rf app/src/user/$user/assets/juspay/index_bundle.js || print_status "failure"
    cp ./../ui-customer/dist/android/index_bundle.js app/src/user/$user/assets/juspay || print_status "failure"
    rm -rf app/src/user/$user/assets/juspay/juspay_assets.json || print_status "failure"
    cd .. || print_status "failure"
    sh userJuspayAssets.sh $user || print_status "failure"
else
    echo " ---------- Driver prod:android :- --------------"
    cd ../ui-driver || print_status "failure"
    npm install && npm install purescript
    npm run prod:android || print_status "failure"
    cd ./../android-native || print_status "failure"
    echo " ---------- Copy index_bundle.js Driver :- --------------"
    mkdir -p "app/src/driver/$partner/assets/juspay" || print_status "failure"
    rm -rf app/src/driver/$partner/assets/juspay/index_bundle.js || print_status "failure"
    cp ./../ui-driver/dist/android/index_bundle.js app/src/driver/$partner/assets/juspay || print_status "failure"
    rm -rf app/src/driver/$partner/assets/juspay/juspay_assets.json || print_status "failure"
    cd .. || print_status "failure"
    sh driverJuspayAssets.sh $partner || print_status "failure"
fi

shopt -u nocasematch


cd android-native || print_status "failure"
echo " ---------- Update Becknbase file :- --------------"
sed -i 's|v1-assets_downloader.jsa|index_bundle.js|' app/src/main/assets/juspay/becknbase.html || print_status "failure"
print_status "success"
cd ..

# Generate KeyStore if not exists
cd android-native || print_status "failure"
if [[ ! -f "my-release-key.keystore" ]]; then
    echo "-------------Generate a KeyStore-----------------------"
    keytool -genkey -v -keystore my-release-key.keystore -alias juspay -keyalg RSA -keysize 2048 -validity 10000 || print_status "failure"
fi

# Build the app
echo " ---------- Build the app :- --------------"
./gradlew assemble$1 || print_status "failure"

echo -e "\nFinal Summary:"
echo -n "Step 1: Update local.properties "
print_status "success"
echo -n "Step 2: Build and copy index_bundle.js "
print_status "success"
echo -n "Step 3: Update Becknbase "
print_status "success"
echo -n "Step 4: Build process for $1 "
print_status "success"



sed -i 's|index_bundle.js|v1-assets_downloader.jsa|' app/src/main/assets/juspay/becknbase.html || print_status "failure"


if [ "$success" = true ]; then
    echo -e "\n Build Generated Successfully!"
else
    echo -e "\nBuild process encountered some issues."
fi