
D.P.F iOS Native Components ( D.P.F iOSNC )

To use iCloud, you first needs to join the Apple Developer Program. 
If you don’t have one, go and purchase it as it is a prerequisite for using iCloud.

Assuming you have an iOS developer account, we’ll first create the App ID with iCloud feature enabled. Log onto the iOS Provisioning Portal and select the App IDs in the sidebar. 

Now, you must be open and edit iOSiCloud.entitlements file and change two key values:

<key>com.apple.developer.ubiquity-container-identifiers</key>

<key>com.apple.developer.ubiquity-kvstore-identifier</key>


I tested this demo on Device.
For using on Simulator you must be google it!
