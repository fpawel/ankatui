unit PropertyUtils;

interface

uses Vcl.Controls;

procedure ConvertToHighColor(ImageList: TImageList);

implementation

uses Winapi.commctrl;

procedure ConvertToHighColor(ImageList: TImageList);

// To show smooth images we have to convert the image list from 16 colors to high color.

var
  IL: TImageList;

begin
  // Have to create a temporary copy of the given list, because the list is cleared on handle creation.
  IL := TImageList.Create(nil);
  IL.Assign(ImageList);

  with ImageList do
    Handle := ImageList_Create(Width, Height, ILC_COLOR16 or ILC_MASK, Count, AllocBy);
  ImageList.Assign(IL);
  IL.Free;
end;

end.
