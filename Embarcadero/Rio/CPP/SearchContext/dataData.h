//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef dataDataH
#define dataDataH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Data.DB.hpp>
#include <Datasnap.DBClient.hpp>
//---------------------------------------------------------------------------
class TdtmdlData : public TDataModule
{
__published:	// IDE-managed Components
	TClientDataSet *cdsIconData;
	TIntegerField *cdsIconDataID;
	TStringField *cdsIconDataCategory;
	TStringField *cdsIconDataDescription;
	TMemoField *cdsIconDataSearchTerms;
	TBlobField *cdsIconDataIcon;
	Data::Db::TDataSource *dsIconData;
	void __fastcall DataModuleCreate(TObject *Sender);
	void __fastcall cdsIconDataAfterPost(TDataSet *DataSet);
private:	// User declarations
public:		// User declarations
	__fastcall TdtmdlData(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdtmdlData *dtmdlData;
//---------------------------------------------------------------------------
#endif
