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
