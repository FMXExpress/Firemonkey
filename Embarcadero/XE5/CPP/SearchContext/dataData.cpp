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
