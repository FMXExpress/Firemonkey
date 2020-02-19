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
unit SQLScripts;

interface

const
  SCRIPT_CREATE_TABLE =
    'CREATE TABLE STORE_ITEMS' + sLineBreak +
    '(' + sLineBreak +
    '  "ITEMID"      INTEGER NOT NULL,' + sLineBreak +
    '  "TENANTID"    VARCHAR(50) NOT NULL,' + sLineBreak +
    '  "BARCODE"     VARCHAR(20) NOT NULL,' + sLineBreak +
    '  "ITEMNAME"    VARCHAR(50) NOT NULL,' + sLineBreak +
    '  "DESCRIPTION" VARCHAR(300) NOT NULL,' + sLineBreak +
    '  "QTY"	       INTEGER NOT NULL,' + sLineBreak +
    '  PRIMARY KEY ("ITEMID")' + sLineBreak +
    ');'  ;

function SCRIPT_CLEAR_DATA: string;
function SCRIPT_INSERT_TENANTS: string;
function SCRIPT_INSERT_ITEMS: string;

implementation

uses
  System.SysUtils;

const
  _TENANT_ID_1 = 'AB715312-06FF-41AB-BBB5-07FF6F101B49';
  _TENANT_SECRET_1 = '1';
  _TENANT_ID_2 = '37B3DBCB-A27E-4F89-8947-3AE24317B924';
  _TENANT_SECRET_2 = '2';

  _SCRIPT_CLEAR_DATA =
    'select * from "DELETETENANTBYID"(''%s'');' + sLineBreak +
    '\' + sLineBreak +
    'select * from "DELETETENANTBYID"(''%s'');' + sLineBreak +
    '\' + sLineBreak +
    'DROP TABLE STORE_ITEMS';

  _SCRIPT_INSERT_TENANTS =
    'select * from "ADDTENANT"(''%s'', ''Downtown Toy Store'', 1, ''6f548ce232672b19919083472870c7e5'', ''ujvHhO'', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, '''');' +  sLineBreak +{  Password: "1" }
    '\' + sLineBreak +
    'select * from "ADDTENANT"(''%s'', ''Midtown Toy Store'', 1, ''4b5db52b1a8ae8b62aed06de31203f57'', ''ULJvCp'', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, '''');';{ Password: "2" }

  _SCRIPT_INSERT_ITEMS_TENANT_1 =
    'INSERT INTO STORE_ITEMS VALUES (1, ''%0:s'', ''143454564'', ''Car'', ''Red car'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (2, ''%0:s'', ''869234294'', ''Building Sets'', ''Robotics themes'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (3, ''%0:s'', ''879234294'', ''Building Sets'', ''Theme parks'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (4, ''%0:s'', ''236934568'', ''Dolls'', ''Blonde doll'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (5, ''%0:s'', ''996934568'', ''Dolls'', ''African dolls'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (6, ''%0:s'', ''454563523'', ''Car'', ''Sport cabriolet.'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (7, ''%0:s'', ''006934568'', ''Car'', ''Radio controlled truck'', 1);' + sLineBreak +
    '\' + sLineBreak;

  _SCRIPT_INSERT_ITEMS_TENANT_2 =
    'INSERT INTO STORE_ITEMS VALUES (20, ''%0:s'', ''546456456'', ''Car'', ''Super fast car'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (21, ''%0:s'', ''454563523'', ''Car'', ''Radio controlled blue car'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (22, ''%0:s'', ''226456456'', ''Building Sets'', ''Super Heroes'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (23, ''%0:s'', ''114563523'', ''Building Sets'', ''Technic'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (24, ''%0:s'', ''006456456'', ''Ant Farm'', ''Small ant farm.'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (25, ''%0:s'', ''884563523'', ''Dolls'', ''Frozen Charlotte'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (26, ''%0:s'', ''454563523'', ''Car'', ''Hatchback model'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (27, ''%0:s'', ''454563523'', ''Car'', ''Minivan model'', 1);' + sLineBreak +
    '\' + sLineBreak +
    'INSERT INTO STORE_ITEMS VALUES (28, ''%0:s'', ''426456456'', ''Dolls'', ''Fashion dolls'', 1);';

function SCRIPT_CLEAR_DATA: string;
begin
  Result := Format(_SCRIPT_CLEAR_DATA, [_TENANT_ID_1, _TENANT_ID_2]);
end;

function SCRIPT_INSERT_TENANTS: string;
begin
  Result := Format(_SCRIPT_INSERT_TENANTS, [_TENANT_ID_1, _TENANT_ID_2]);
end;

function SCRIPT_INSERT_ITEMS: string;
begin
  Result :=
    Format(_SCRIPT_INSERT_ITEMS_TENANT_1, [_TENANT_ID_1]) +
    Format(_SCRIPT_INSERT_ITEMS_TENANT_2, [_TENANT_ID_2]);
end;

end.
