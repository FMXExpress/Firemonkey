//---------------------------------------------------------------------------
#pragma hdrstop

#include "DataModuleUnit1.h"
#include <System.Generics.Collections.hpp>
#include <map>
#include <memory>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "FMX.Controls.TControl"
#pragma resource "*.dfm"
TDataModule1 *DataModule1;
//---------------------------------------------------------------------------
__fastcall TDataModule1::TDataModule1(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::CreateToDoList(const String AProviderID,
	const TBackendStorageApi &AStorage, TJSONArray * & AJSONArray,
        System::DynamicArray<TBackendEntityValue> &AMetaArray)
{
	String LQuery[1];
        if(SameText(AProviderID, TCustomKinveyProvider_ProviderID)) {
                LQuery[0] = Format("sort=%s", ARRAYOFCONST((TToDoNames::TitleElement)));
        }
        else if(SameText(AProviderID, TCustomParseProvider_ProviderID)) {
                LQuery[0] = Format("order=%s", ARRAYOFCONST((TToDoNames::TitleElement)));
        }
        else {
    		throw System::Sysutils::Exception("Unknow provider");
        }
    	std::auto_ptr<TJSONArray> LJSONArray(new TJSONArray());
    	const_cast<TBackendStorageApi&>(AStorage).QueryObjects(TToDoNames::BackendClassname, EXISTINGARRAY(LQuery), LJSONArray.get(), AMetaArray );
    	AJSONArray = LJSONArray.release();
}
//---------------------------------------------------------------------------
TBindSourceAdapter * __fastcall TDataModule1::GetToDoItemsAdapter(void)
{
	if(FToDoItemsAdapter == NULL) {
		System::Generics::Collections::TList__1<System::TObject*>* LList;
		LList = new System::Generics::Collections::TList__1<System::TObject*>();
		FToDoItemsAdapter = new TListBindSourceAdapter(this, LList, __classid(TToDo), true);
		FToDoItemsAdapter->AfterPost = AfterPost;
        	FToDoItemsAdapter->BeforeDelete = BeforeDelete;
	}
	return FToDoItemsAdapter;

}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::AfterPost(TBindSourceAdapter *Sender)
{
	try
	{
		TToDo * LToDo = (TToDo*)(FToDoItemsAdapter->List->Items[FToDoItemsAdapter->ItemIndex]);
       
		if (FToDoItemsMap.find(LToDo) != FToDoItemsMap.end()) {
			UpdateBackendItem(LToDo);
		}
		else {
			String LObjectID = AddBackendItem(LToDo);
                        FToDoItemsMap[LToDo] = LObjectID;
		}
	}
	catch(...) {
		FToDoItemsAdapter->Edit();
		throw;
    }
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::BeforeDelete(TBindSourceAdapter *Sender)
{
	TToDo * LToDo = (TToDo*)(FToDoItemsAdapter->List->Items[FToDoItemsAdapter->ItemIndex]);
        if (FToDoItemsMap.find(LToDo) != FToDoItemsMap.end())
		DeleteBackendItem(LToDo);
}
//---------------------------------------------------------------------------
String __fastcall TDataModule1::AddBackendItem(const TToDo *AItem)
{
	TBackendEntityValue LEntity;
        std::auto_ptr<TJSONObject> LJSON(ObjectToJSON(AItem));
        BackendStorage1->Storage->CreateObject(TToDoNames::BackendClassname, LJSON.get(), LEntity);
	return LEntity.ObjectID;
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::UpdateBackendItem(const TToDo *AItem)
{
	TBackendEntityValue LUpdate;
        std::auto_ptr<TJSONObject> LJSONObject(ObjectToJSON(AItem));
	std::map<TToDo*, String>::iterator found = FToDoItemsMap.find((TToDo*)AItem);
	BackendStorage1->Storage->UpdateObject(TToDoNames::BackendClassname, found->second,
          LJSONObject.get(), LUpdate);
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::DeleteBackendItem(const TToDo *AItem)
{
	std::map<TToDo*, String>::iterator found = FToDoItemsMap.find((TToDo*)AItem);
	BackendStorage1->Storage->DeleteObject(TToDoNames::BackendClassname, found->second);
}

TJSONObject * __fastcall TDataModule1::ObjectToJSON(const TToDo * AToDo)
{
std::auto_ptr<TJSONObject> LJSON(new TJSONObject);
        if (AToDo->Title != "") {
            LJSON->AddPair(TToDoNames::TitleElement, AToDo->Title);
        }
        if (AToDo->Content != "") {
            LJSON->AddPair(TToDoNames::ContentElement, AToDo->Content);
		}
        TJSONObject * LResult = LJSON.release();
        return LResult;
}

TToDo * __fastcall TDataModule1::JSONToObject(const TJSONObject * AJSONObject)
{
std::auto_ptr<TToDo> LToDo(new TToDo());
        TJSONValue * LValue;
        LValue = const_cast<TJSONObject*>(AJSONObject)->GetValue(TToDoNames::TitleElement);
        if (LValue != NULL) {
		  LToDo->Title = LValue->Value();
        }
        LValue = const_cast<TJSONObject*>(AJSONObject)->GetValue(TToDoNames::ContentElement);
        if (LValue != NULL) {
          LToDo->Content = LValue->Value();
        }
		TToDo * LResult = LToDo.release();
        return LResult;
}


//---------------------------------------------------------------------------
void __fastcall TDataModule1::RefreshAdapter()
{

        TJSONArray * LJSONArray;
        System::DynamicArray<TBackendEntityValue> LMetaArray;
        CreateToDoList(BackendStorage1->Provider->ProviderID, *BackendStorage1->Storage, LJSONArray, LMetaArray);
        std::auto_ptr<TObject> LArrayRef(LJSONArray);  // Free later

	TList__1<TObject*> * LList = NULL;
        LList = new TList__1<TObject*>();
        std::auto_ptr<TObject> LListRef(LList);  // Free later

        FToDoItemsMap.clear();
        for(int i = 0; i < LJSONArray->Count; i++) {
            TToDo * item = JSONToObject((TJSONObject*)(LJSONArray->Items[i]));
            LList->Add(item);
            FToDoItemsMap[item] = LMetaArray[i].ObjectID;
        }
        if(FToDoItemsAdapter == NULL) {
                FToDoItemsAdapter = new TListBindSourceAdapter(this, LList, __classid(TToDo), true);
        }
        FToDoItemsAdapter->SetList(LList, true);
        LListRef.release(); // Don't free
}
//---------------------------------------------------------------------------
