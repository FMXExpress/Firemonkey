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
