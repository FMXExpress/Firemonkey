//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ToDoItemTypes;

interface

type



  TToDo = class(TObject)
  private
    FTitle: string;
    FContent: string;
  public
    property Title: string read FTitle write FTitle;
    property Content: string read FContent write FContent;
  end;

  TToDoNames = record
  public
    const
      TitleProperty = 'Title';
      ContentProperty = 'Content';
      BackendClassname = 'ToDos';
      TitleElement = 'title';
      ContentElement = 'content';
  end;

implementation

end.
