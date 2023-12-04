
# Frontend - NammaYatri

This is the frontend codebase for the Nammayatri app.

## Installation

To install the frontend dependencies, first enter the development shell by running
```bash
   nix develop .#frontend
```
This will give you access to all of the purescript tooling as well as the correct version of node

Then navigate to the frontend > ui-customer/ui-driver both and install the npm dependencies by running the following command:
```bash
  npm i
```

Start the development server by running the following command:
```bash
  npm start
```


## Android

To start the frontend app for Android, follow these steps:

    1. Open `android-native` in Android Studio
    2. select build variant
    3. Add the google-services.json file to the app folder.
    4. Add the MAPS_API_KEY to the local.properties file.
    5. Add USER_MERCHANT_ID and DRIVER_MERCHANT_ID to the local.properties file.
    6. Run the app after selecting a device and build variant.

To create bundle for the app, follow these steps:
    1. Open `android-native` in terminal
    2. run the below command

```bash
  bash bundling.sh <userMerchantDir> <driverMerchantDir>
```

```bash
  ex :  bash bundling.sh nammaYatri nammaYatriPartner
```

To update the `juspay_assets.json`, follow these steps:
    1. Open `android-native` in terminal
    2. run the below command

# For user

```bash
  bash userJuspayAssets.sh <userMerchantDir>
```

   

```bash
   ex :  bash userJuspayAssets.sh nammaYatri
```

# For driver

```bash
  bash driverJuspayAssets.sh <driverMerchantDir>
```

```bash
   ex : bash driverJuspayAssets.sh nammaYatri
```

**Starting the Server**

    npm run start:android:<master, sandbox, or prod>

## IOS

To start the frontend app for IOS, follow these steps:

**Starting the Server**

    npm run start:ios:<master, sandbox, or prod>

**To Update The strings files**

put your string in the following sheets : 
  - https://docs.google.com/spreadsheets/d/1543N264Vbl374BEXgZXLxxNmJ3VkkWM50GaPghLPLkU/edit#gid=0 (for customer)
  - https://docs.google.com/spreadsheets/d/1543N264Vbl374BEXgZXLxxNmJ3VkkWM50GaPghLPLkU/edit#gid=641543613 (for driver)

then publish your changes from the google sheets.

Then run the following command in Frontend directory in your terminal: 
```bash
  bash updateTranslation.sh <directory>
```

<directory> = ui-driver ( for driver ) | ui-customer ( for customer )