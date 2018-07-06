unit UnitFormPartiesNodes;

interface

uses
    System.SysUtils, System.Variants,
    System.Classes,
    FireDAC.Comp.Client, UnitData,
    VirtualTrees;

type
    PTreeData = ^RTreeData;

    RColumn = record
        Text: string;
        FontSize: integer;
        ImageIndex: integer;
    end;

    TNodeData = class
    public
        FNode: PVirtualNode;
        FPopulated: boolean;
        FTreeView: TVirtualStringTree;
        FColumn: array [0 .. 2] of RColumn;

        procedure Populate; virtual; abstract;
        constructor Create(ATreeView: TVirtualStringTree;
          ANode: PVirtualNode); virtual;
    end;

    TNodeYear = class(TNodeData)
    public
        FYear: integer;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; AYear: integer);
    end;

    TNodeMonth = class(TNodeData)
    public
        FYear, FMonth: integer;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          AYear, AMonth: integer);
    end;

    TNodeDay = class(TNodeData)
    public
        FYear, FMonth, FDay: integer;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          AYear, AMonth, ADay: integer);
    end;

    TNodeParty = class(TNodeData)
    public
        FYear, FMonth, FDay: integer;
        FPartyID: int64;
        FWaht: string;
        FHasLog: boolean;
        FCreatedAt: TDateTime;
        FValues: TKeysValues;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          AYear, AMonth, ADay: integer; ACreatedAt: TDateTime; APartyID: int64;
          AWaht: string; AHasLog: boolean);

    end;

    TNodeProduct = class(TNodeData)
    public
        FPartyID: int64;
        FSerial: integer;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          APartyID: int64; ASerial: integer);
    end;

    TNodePartyLogsRoot = class(TNodeData)
    public
        FPartyID: int64;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          APartyID: int64);
        procedure Populate; override;
    end;


    TNodePartyDayLog = class(TNodeData)
    public
        FDay, FMonth, FYear: integer;
        FPartyID: int64;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          APartyID: int64; ADay, AMonth, AYear: integer);
        procedure Populate; override;
    end;

    TNodeWorkLog = class(TNodeData)
    public
        FCreatedAt: TDateTime;
        FWork: string;
        FRecordID: int64;
        FHasError: boolean;
        FHasChildren: boolean;
        FWorkIndex: integer;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          FDQuery: TFDQuery);
        procedure Populate; override;
    end;

    RTreeData = record
        X: TNodeData;
    end;

implementation

uses dateutils, FireDAC.Stan.PAram, stringutils, richeditutils;

function inttostr2(n: integer): string;
begin
    result := inttostr(n);
    if n < 10 then
        result := '0' + result;
end;

procedure freeNodeData(t: TVirtualStringTree; n: PVirtualNode);
var
    p: PTreeData;
begin
    if not Assigned(n) then
        exit;
    p := t.GetNodeData(n);
    p.X.Free;
    freeNodeData(t, n.FirstChild);
    freeNodeData(t, n.NextSibling);
end;

constructor TNodeData.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode);
var
    d: RTreeData;
    c: RColumn;
    i: integer;
begin
    inherited Create;
    d.X := self;
    FTreeView := ATreeView;
    FNode := ANode;
    ATreeView.SetNodeData(FNode, PTreeData(d));
    ATreeView.HasChildren[FNode] := true;
    for i := 0 to 2 do
    begin
        FColumn[i].ImageIndex := -1;
    end;

end;

constructor TNodeYear.Create(ATreeView: TVirtualStringTree; AYear: integer);
begin
    inherited Create(ATreeView, ATreeView.AddChild(nil));
    FYear := AYear;
    FColumn[0].ImageIndex := 0;
    FColumn[0].Text := inttostr(FYear);

end;

constructor TNodeMonth.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode; AYear, AMonth: integer);
var
    dt: TDateTime;
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    FYear := AYear;
    FMonth := AMonth;
    FColumn[0].ImageIndex := 1;
    dt := EncodeDateTime(2000, FMonth, 1, 0, 0, 0, 0);
    FColumn[0].Text := FormatDateTime('mm', dt) + ' ' +
      FormatDateTime('mmmm', dt);
end;

constructor TNodeDay.Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
  AYear, AMonth, ADay: integer);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    FYear := AYear;
    FMonth := AMonth;
    FDay := ADay;
    FColumn[0].ImageIndex := 2;
    FColumn[0].Text := inttostr(FDay);
    if FDay < 10 then
        FColumn[0].Text := '0' + FColumn[0].Text;
    ATreeView.Expanded[FNode] := true;
end;

constructor TNodeParty.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode; AYear, AMonth, ADay: integer; ACreatedAt: TDateTime;
  APartyID: int64; AWaht: string; AHasLog: boolean);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    FPartyID := APartyID;
    FValues := DataModule1.PartyValues(FPartyID);
    FWaht := AWaht;
    FHasLog := AHasLog;
    FYear := AYear;
    FMonth := AMonth;
    FDay := ADay;
    FCreatedAt := ACreatedAt;

    FColumn[0].Text := AWaht;
    FColumn[0].ImageIndex := 3;

    FColumn[1].Text := inttostr(FPartyID);
    FColumn[2].Text := FormatDateTime('dd MMMM', FCreatedAt);

end;

constructor TNodeProduct.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode; APartyID: int64; ASerial: integer);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    FPartyID := APartyID;
    FColumn[0].ImageIndex := 4;
    FSerial := ASerial;
    FColumn[0].Text := '№ ' + inttostr(ASerial);

end;

constructor TNodePartyLogsRoot.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode; APartyID: int64);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    FColumn[0].Text := 'журнал';
    FPartyID := APartyID;
end;

constructor TNodePartyDayLog.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode; APartyID: int64; ADay, AMonth, AYear: integer);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    FDay := ADay;
    FMonth := AMonth;
    FYear := AYear;
    FColumn[0].Text := FormatDateTime('dd MMMM',
      EncodeDate(FYear, FMonth, FDay));
    FPartyID := APartyID;
end;

constructor TNodeWorkLog.Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
  FDQuery: TFDQuery);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    with FDQuery do
    begin
        FWork := FieldValues['work_name'];
        FRecordID := FieldValues['work_id'];
        FCreatedAt := FieldValues['created_at'];
        FHasError := FieldValues['has_error'];
        FWorkIndex := FieldValues['work_index'];
        FHasChildren := FieldValues['has_children'];
    end;

    FColumn[0].Text := FWork;
    FColumn[1].Text := inttostr2(FWorkIndex);
    FColumn[2].Text := FormatDateTime('HH:nn', IncHour(FCreatedAt, 3));
    if FHasError then
        FColumn[0].ImageIndex := 5;

    FPopulated := not FHasChildren;
    FTreeView.HasChildren[FNode] := FHasChildren;

end;




procedure TNodePartyDayLog.Populate;
begin
    with DataModule1.FDQueryPartyWorks do
    begin
        ParamByName('party_id').Value := FPartyID;
        ParamByName('year').Value := FYear;
        ParamByName('month').Value := FMonth;
        ParamByName('day').Value := FDay;
        open;
        First;
        while not Eof do
        begin
            TNodeWorkLog.Create(FTreeView, FNode, DataModule1.FDQueryPartyWorks);
            Next;
        end;
        Close;
    end;
end;

procedure TNodeWorkLog.Populate;
begin
    with DataModule1.FDQueryWorksByParentRecordID do
    begin
        ParamByName('parent_work_id').Value := FRecordID;
        open;
        First;
        FTreeView.HasChildren[FNode] := false;
        while not Eof do
        begin
            TNodeWorkLog.Create(FTreeView, FNode,
              DataModule1.FDQueryWorksByParentRecordID);
            Next;
            FTreeView.HasChildren[FNode] := true;
        end;
        Close;
    end;

end;

procedure TNodeYear.Populate;
begin
    with DataModule1.FDQuery1 do
    begin
        SQL.Text := 'SELECT * FROM party_year_month WHERE year = :year;';
        ParamByName('year').Value := FYear;
        open;
        First;
        while not Eof do
        begin
            TNodeMonth.Create(FTreeView, FNode, FYear, FieldValues['month']);
            Next;
        end;
        Close;
    end;
end;

procedure TNodeMonth.Populate;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM party_year_month_day ' +
          'WHERE year = :year AND month = :month;';
        ParamByName('year').Value := FYear;
        ParamByName('month').Value := FMonth;
        open;
        First;
        while not Eof do
        begin
            TNodeDay.Create(FTreeView, FNode, FYear, FMonth,
              FieldValues['day']);
            Next;
        end;
        Free;
    end;
end;

procedure TNodeDay.Populate;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM party_info ' +
          'WHERE year = :year AND month = :month AND day = :day;';
        ParamByName('year').Value := FYear;
        ParamByName('month').Value := FMonth;
        ParamByName('day').Value := FDay;
        open;
        First;
        while not Eof do
        begin
            TNodeParty.Create(FTreeView, FNode, FYear, FMonth, FDay,
              FieldValues['created_at'], FieldValues['party_id'],
              FieldValues['what'], FieldValues['has_log']);
            Next;
        end;
        Free;
    end;
end;

procedure TNodeParty.Populate;
var
    serial: integer;
begin
    if FHasLog then
        TNodePartyLogsRoot.Create(FTreeView, FNode, FPartyID);

    for serial in DataModule1.PartyProducts(FPartyID) do
        TNodeProduct.Create(FTreeView, FNode, FPartyID, serial);

end;

(*
TNodePartyLogsRoot = class(TNodeData)
    public
        FPartyID: int64;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          APartyID: int64;);
        procedure Populate; override;
    end;
*)

procedure TNodePartyLogsRoot.Populate;
begin
   with DataModule1.FDQueryPartyWorksDays do
        begin
            ParamByName('party_id').Value := FPartyID;
            open;
            First;
            while not Eof do
            begin
                TNodePartyDayLog.Create(FTreeView, FNode, FPartyID,
                  FieldValues['day'], FieldValues['month'],
                  FieldValues['year']);
                Next;
            end;
            Close;
        end;
end;

end.
