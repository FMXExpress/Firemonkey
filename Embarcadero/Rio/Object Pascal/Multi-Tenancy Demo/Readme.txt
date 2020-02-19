// Copyright (c) 2017 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------

To run the Sample Multi-Tenant application:

1. Install the latest version of the EMS Server.
2. Open emsserver.ini file. You can find it in the default location: C:\Users\Public\Documents\Embarcadero\EMS\emsserver.ini.
3. In the [Server.Tenants] section, uncomment the following line: "MultiTenantMode=1".
4. Open project group  .\MultiTenantMode\MultiTenantMode.groupproj.
5. Select MultiTenantSample_EMSPackage.bpl project.
6. In CommonDataModule.pas, find the IBConnection component and make sure the connection with the EMS database is valid.
7. Build and run MultiTenantSample_EMSPackage. EMS Server should start successfully.
6. Select MultiTenantSample_Client.dproj, open ClienDataModule, and make sure that URLHost and URLPort to your EMS Server are valid. Build and run it. 

Here are the passwords for the Sample RAD Server Multi-Tenant application:

1. Tenant ID and Secret:
You can find the predefined tenant ids and secrets by opening SQLScripts.pas (part of the EMS Package) and looking at the source code.


2.	Downtown Toy Store: password:  '1'
	Employees:
	sTestUser1 = 'John';
 	sTestUser2 = 'Sam';
Passwords to Employees (both managers and cashiers): '1'

3.	Midtown Toy Store: password:  '2'
	Employees:
 	sTestUser3 = 'Michael';
 	sTestUser4 = 'Tina';
Passwords to Employees (both managers and cashiers): '1'
