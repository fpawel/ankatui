unit UnitFormChartNodes;

interface

uses
    System.SysUtils, System.Variants,
    System.Classes,
    FireDAC.Comp.Client, UnitData,
    VirtualTrees, vcl.graphics;

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
        FColumn: array [0 .. 3] of RColumn;

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

    RSeriesInfo = record
        Year, Month, Day: integer;
        CreatedAt: TDateTime;
        PartyID: int64;
        SeriesID: int64;
        WorkName: string;
        WorkIndex: integer;
    end;

    TNodeSeries = class(TNodeData)
    public
        FSeriesInfo: RSeriesInfo;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          ASeriesInfo: RSeriesInfo);
    end;

    TNodeVar = class(TNodeData)
    public
        FSeriesInfo: RSeriesInfo;
        FVar: integer;
        FVarName: string;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          ASeriesInfo: RSeriesInfo; AVar: integer; AVarName: string);
    end;

    TNodeVarProduct = class(TNodeData)
    public
        FSeriesInfo: RSeriesInfo;
        FVar: integer;
        FVarName: string;
        FSerial: integer;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          ASeriesInfo: RSeriesInfo; AVar: integer; AVarName: string; ASerial: integer);
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
end;

constructor TNodeSeries.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode; ASeriesInfo: RSeriesInfo);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    FSeriesInfo := ASeriesInfo;
    FColumn[0].Text := ASeriesInfo.WorkName;
    FColumn[1].Text := TimeToStr(ASeriesInfo.CreatedAt);
    FColumn[2].Text := inttostr(ASeriesInfo.PartyID);
    FColumn[3].Text := inttostr2(ASeriesInfo.WorkIndex);
    FColumn[0].ImageIndex := 3;
end;

constructor TNodeVar.Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
  ASeriesInfo: RSeriesInfo; AVar: integer; AVarName: string);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));

    FSeriesInfo := ASeriesInfo;
    FVar := AVar;
    FVarName := AVarName;
    FColumn[0].Text := inttostr(AVar) + ' ' + AVarName;
    FColumn[0].ImageIndex := 4;

end;

constructor TNodeVarProduct.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode; ASeriesInfo: RSeriesInfo; AVar: integer; AVarName: string;
  ASerial: integer);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    ATreeView.HasChildren[FNode] := false;

    FSeriesInfo := ASeriesInfo;
    FVar := AVar;
    FVarName := AVarName;
    FSerial := ASerial;
    FColumn[0].Text := inttostr(ASerial);
    FColumn[0].ImageIndex := 5;
end;


procedure TNodeYear.Populate;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text :=
          'SELECT DISTINCT month FROM series_info WHERE year = :year;';
        ParamByName('year').Value := FYear;
        open;
        First;
        while not Eof do
        begin
            TNodeMonth.Create(FTreeView, FNode, FYear, FieldValues['month']);
            Next;
        end;
        Close;
        Free;
    end;
    if (FNode.ChildCount = 0) and (FYear = YearOf(now)) then
    begin
        TNodeMonth.Create(FTreeView, FNode, FYear, MonthOf(now));
    end;

end;

procedure TNodeMonth.Populate;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text := 'SELECT DISTINCT day FROM series_info ' +
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
    if (FNode.ChildCount = 0) and (FYear = YearOf(now)) and (FMonth = MonthOf(now))  then
    begin
        TNodeDay.Create(FTreeView, FNode, FYear, MonthOf(now), DayOf(now));
    end;
end;

procedure TNodeDay.Populate;
var
    r: RSeriesInfo;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM series_info ' +
          'WHERE year = :year AND month = :month AND day = :day;';
        ParamByName('year').Value := FYear;
        ParamByName('month').Value := FMonth;
        ParamByName('day').Value := FDay;
        open;
        First;
        while not Eof do
        begin
            r.Year := FYear;
            r.Month := FMonth;
            r.Day := FDay;
            r.CreatedAt := FieldValues['created_at'];
            r.PartyID := FieldValues['party_id'];
            r.WorkName := FieldValues['work_name'];
            r.WorkIndex := FieldValues['work_index'];
            r.SeriesID := FieldValues['series_id'];
            TNodeSeries.Create(FTreeView, FNode, r);
            Next;
        end;
        Free;
    end;

    
end;

procedure TNodeSeries.Populate;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text :=
          'SELECT DISTINCT var, var_name FROM chart_value_info WHERE series_id = :series_id;';
        ParamByName('series_id').Value := FSeriesInfo.SeriesID;
        open;
        First;
        while not Eof do
        begin
            TNodeVar.Create(FTreeView, FNode, FSeriesInfo,
              FieldValues['var'], FieldValues['var_name']);
            Next;
        end;
        Free;
    end;
end;

procedure TNodeVar.Populate;
var n:integer;
begin
    for n in DataModule1.GetSeriesVarProducts(FSeriesInfo.SeriesID, FVar) do
    begin
        TNodeVarProduct.Create(FTreeView, FNode, FSeriesInfo, FVar, FVarName, n);
    end;
end;

end.
