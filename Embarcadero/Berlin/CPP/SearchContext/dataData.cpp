//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#pragma hdrstop

#include "dataData.h"
#include "FMX.Dialogs.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "System.Classes.TPersistent"
#pragma resource "*.dfm"
TdtmdlData *dtmdlData;
//---------------------------------------------------------------------------
String FileName()
{
   return(ExtractFilePath(GetModuleName(0))+"iconData.cds");
}
//---------------------------------------------------------------------------
__fastcall TdtmdlData::TdtmdlData(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdtmdlData::DataModuleCreate(TObject *Sender)
{
	this->cdsIconData->FileName = FileName();

	if (FileExists(this->cdsIconData->FileName)) {
		try {
			cdsIconData->Active = True;
		} catch (...) {
			cdsIconData->Close();
			cdsIconData->CreateDataSet();
		}
	}
	else
	{
		cdsIconData->CreateDataSet();
	}

}

//---------------------------------------------------------------------------
void __fastcall TdtmdlData::cdsIconDataAfterPost(TDataSet *DataSet)
{
	cdsIconData->SaveToFile();
}
//---------------------------------------------------------------------------
