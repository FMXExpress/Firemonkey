// ---------------------------------------------------------------------------

#ifndef CacheDataModuleUH
#define CacheDataModuleUH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.JSON.hpp>
#include <vector>
// ---------------------------------------------------------------------------

struct TItem {
private:
    TDateTime FTime;
    String FDevice;
    String FValue;
    String FPerson;

public:
    __fastcall TItem(TDateTime ATime, String ADevice, String APerson,
	TJSONObject * AValue) {
	FTime = ATime;
	FDevice = ADevice;
	FPerson = APerson;
	FValue = AValue->ToJSON();
    }

    __fastcall TItem(void) {
    }

    __property String Person = {read = FPerson};
    __property String Device = {read = FDevice};
    __property TDateTime Time = {read = FTime};
    __property String Value = {read = FValue};
};

class TCache {
private:
    std::vector<TItem> *FList;

public:
    __fastcall TCache() {
	FList = new std::vector<TItem>();
    }

    __fastcall ~TCache() {
	delete FList;
    }

    typedef void __fastcall(__closure * TItemsEvent)(const TItem & AItem,
	bool &ADone);

    bool __fastcall TryGetRecent(String ADevice, TItem &AItem);
    bool __fastcall EnumItems(String ADevice, TItemsEvent AOnItem);
    void __fastcall ClearDevice(String ADevice);
    void __fastcall Save(TItem * AItem);
};

class TCacheDataModule : public TDataModule {
__published: // IDE-managed Components
	private : // User declarations

    TCache * FCache;

private:
    void __fastcall OnItem(const TItem &AItem, bool &ADone);

public: // User declarations

    __fastcall TCacheDataModule(TComponent* Owner);
    __fastcall ~TCacheDataModule(void);
    void __fastcall SaveDeviceData(String ADevice, TDateTime ATime,
	TJSONObject * AData);
    void __fastcall ClearDeviceData(String ADevice);
    bool __fastcall TryGetRecentDeviceData(String ADevice, TDateTime &ATime,
	TJSONObject * AData);
    bool __fastcall TryGetDeviceDataStats(String ADevice, TDateTime &ATime,
	TJSONObject *AData);
};

// ---------------------------------------------------------------------------
extern PACKAGE TCacheDataModule *CacheDataModule;
// ---------------------------------------------------------------------------
#endif
