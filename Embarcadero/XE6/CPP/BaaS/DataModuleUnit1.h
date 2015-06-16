//---------------------------------------------------------------------------

#ifndef DataModuleUnit1H
#define DataModuleUnit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <IPPeerClient.hpp>
#include <REST.Backend.KinveyProvider.hpp>
#include <REST.Backend.ParseProvider.hpp>
#include <REST.Backend.KinveyServices.hpp>
#include <REST.Backend.MetaTypes.hpp>
#include <REST.Backend.Providers.hpp>
#include <REST.Backend.ServiceComponents.hpp>
#include <REST.Backend.ServiceTypes.hpp>
#include <REST.OpenSSL.hpp>
#include <System.JSON.hpp>
#include <REST.OpenSSL.hpp>
#include <System.SysUtils.hpp>
#include "ToDoItemTypes.h"
#include <vector>
#include <map>
//---------------------------------------------------------------------------
class TDataModule1 : public TDataModule
{
__published:	// IDE-managed Components
	TKinveyProvider * KinveyProvider1;
	TBackendStorage * BackendStorage1;
private:	// User declarations
	//TBackendObjectList__1<TObject*> *FBackendList;
	TListBindSourceAdapter *FToDoItemsAdapter;
        std::map<TToDo*, String> FToDoItemsMap;
	void __fastcall CreateToDoList(const String AProviderID,
		const TBackendStorageApi &AStorage, TJSONArray * & AJSONArray,
                System::DynamicArray<TBackendEntityValue> &AMetaArray);
	TBindSourceAdapter* __fastcall GetToDoItemsAdapter(void);
	void __fastcall  AfterPost(TBindSourceAdapter *Sender);
	void __fastcall  BeforeDelete(TBindSourceAdapter *Sender);
	String __fastcall  AddBackendItem(const TToDo *AItem);
	void __fastcall  UpdateBackendItem(const TToDo *AItem);
	void __fastcall DeleteBackendItem(const TToDo *AItem);
        TJSONObject * __fastcall ObjectToJSON(const TToDo *AToDo);
        TToDo * __fastcall JSONToObject(const TJSONObject *AJSONObject);
public:		// User declarations
	__fastcall TDataModule1(TComponent* Owner);
public:
	void __fastcall RefreshAdapter();
	__property TBindSourceAdapter * ItemAdapter = {read=GetToDoItemsAdapter};
};
//---------------------------------------------------------------------------
extern PACKAGE TDataModule1 *DataModule1;
//---------------------------------------------------------------------------
#endif
