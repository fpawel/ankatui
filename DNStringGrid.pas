unit DNStringGrid;

interface

uses
  Classes, Grids;

type
  TColWidthChanged = procedure(Sender: TObject; const ColIndex: Integer) of object;

  TDNStringGrid = class(TStringGrid)
  private
    FWidths: array of Integer;
    FOnColWidthChanged: TColWidthChanged;
    procedure FormatArray;
  protected
    procedure Loaded; override;
    procedure ColWidthsChanged; override;
    procedure Paint; override;
  published
    property OnColWidthChanged: TColWidthChanged read FOnColWidthChanged
                                                 write FOnColWidthChanged;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDNStringGrid]);
end;

{ TDNStringGrid }

procedure TDNStringGrid.ColWidthsChanged;
var
  i: Integer;
begin
  inherited;
  // поиск изменившегося столбца
  for i:= 0 to ColCount-1 do
    if ColWidths[i] <> FWidths[i] then
    begin
      FWidths[i]:= ColWidths[i];
      Break;
    end;
  if Assigned(FOnColWidthChanged) then FOnColWidthChanged(Self,i);
end;

procedure TDNStringGrid.FormatArray;
var
  i: Integer;
begin
  // если не в режиме разработки, формируется массив,
  // содержащий ширины столбцов
  if csDesigning in ComponentState then Exit;
  SetLength(FWidths,ColCount);
  for i:= 0 to ColCount-1 do
    FWidths[i]:= ColWidths[i];
end;

procedure TDNStringGrid.Loaded;
begin
  inherited Loaded;
  FormatArray;
end;

// если количество столбцов изменилось, заново формируется массив.
// делается в этом обработчике, поскольку иначе невозможно
// отловить событие изменения количества столбцов
procedure TDNStringGrid.Paint;
begin
  if ColCount <> High(FWidths)+1 then FormatArray;
  inherited Paint;
end;

end.
