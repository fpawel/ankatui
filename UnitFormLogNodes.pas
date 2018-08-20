unit UnitFormLogNodes;

interface

uses
    System.SysUtils, System.Variants,
    System.Classes,
    FireDAC.Comp.Client, UnitData,
    VirtualTrees, virtual_tree_node;

type

    TNodeYear = class(TNodeData)
    public
        FYear: integer;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; AYear: integer);
        function IsToday: boolean;
    end;

    TNodeMonth = class(TNodeData)
    public
        FYear, FMonth: integer;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          AYear, AMonth: integer);
        function IsToday: boolean;
    end;

    TNodeDay = class(TNodeData)
    public
        FYear, FMonth, FDay: integer;
        procedure Populate; override;
        function IsToday: boolean;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          AYear, AMonth, ADay: integer);
    end;

    TNodeWorkLog = class(TNodeData)
    public
        FCreatedAt: TDateTime;
        FWork: string;
        FWorkID: int64;
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

function IsTodayNode(n:TNodeData): boolean;

implementation

uses dateutils, FireDAC.Stan.PAram, stringutils, richeditutils;

function IsTodayNode(n:TNodeData): boolean;
begin
    result :=
        (n is TNodeYear) AND (n as TNodeYear).IsToday  OR
        (n is TNodeMonth) AND (n as TNodeMonth).IsToday  OR
        (n is TNodeDay) AND (n as TNodeDay).IsToday;

end;

function inttostr2(n: integer): string;
begin
    result := inttostr(n);
    if n < 10 then
        result := '0' + result;
end;

function TNodeYear.IsToday: boolean;
begin
    result := (FYear = YearOf(now));

end;

function TNodeMonth.IsToday: boolean;
begin
    result := (FYear = YearOf(now)) AND (FMonth = MonthOf(now));

end;

function TNodeDay.IsToday: boolean;
begin
    result := (FYear = YearOf(now)) AND (FMonth = MonthOf(now)) AND
      (FDay = DayOf(now));

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


constructor TNodeYear.Create(ATreeView: TVirtualStringTree; AYear: integer);
begin
    inherited Create(ATreeView, ATreeView.AddChild(nil));
    FYear := AYear;
    FColumn[0].ImageIndex := 0;
    FColumn[0].Text := inttostr(FYear);
    if AYear = YearOf(now) then
        FTreeView.Expanded[FNode] := true;

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
    if (AYear = YearOf(now)) AND (AMonth = MonthOf(now)) then
        FTreeView.Expanded[FNode] := true;
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
    if IsToday then
        FTreeView.Expanded[FNode] := true;
end;

constructor TNodeWorkLog.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode; FDQuery: TFDQuery);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    with FDQuery do
    begin

        FWorkID := FieldValues['work_id'];
        FCreatedAt := FieldValues['created_at'];
        FHasError := FieldValues['has_error'];
        FWorkIndex := FieldValues['work_index'];
        FHasChildren := FieldValues['has_children'];
        FWork := FieldValues['work_name'];
    end;

    FColumn[0].Text := FWork;
    FColumn[1].Text := inttostr2(FWorkIndex);
    FColumn[2].Text := FormatDateTime('HH:nn', IncHour(FCreatedAt, 3));
    if FHasError then
        FColumn[0].ImageIndex := 5;
    FPopulated := not FHasChildren;
    FTreeView.HasChildren[FNode] := FHasChildren;
end;

procedure TNodeYear.Populate;
begin
    with DataModule1.FDQueryWorkLogYearMonths do
    begin
        ParamByName('year').Value := FYear;
        open;
        First;
        while not Eof do
        begin
            TNodeMonth.Create(FTreeView, FNode, FYear, FieldValues['month']);
            Next;
        end;
    end;
    if (FNode.ChildCount = 0) and (FYear = YearOf(now)) then
    begin
        TNodeMonth.Create(FTreeView, FNode, FYear, MonthOf(now));
    end;
end;

procedure TNodeMonth.Populate;
begin
    with DataModule1.FDQueryWorkLogYearMonthDays do
    begin
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
        close;
    end;
    if (FNode.ChildCount = 0) and (FYear = YearOf(now)) and (FMonth = MonthOf(now))  then
    begin
        TNodeDay.Create(FTreeView, FNode, FYear, MonthOf(now), DayOf(now));
    end;
end;

procedure TNodeDay.Populate;
var
    Node: PVirtualNode;
    d: TNodeWorkLog;
begin
    with DataModule1.FDQueryWorkLogsYearMonthDay do
    begin
        ParamByName('year').Value := FYear;
        ParamByName('month').Value := FMonth;
        ParamByName('day').Value := FDay;
        open;
        First;
        while not Eof do
        begin
            d := TNodeWorkLog.Create(FTreeView, FNode,
              DataModule1.FDQueryWorkLogsYearMonthDay);

            Node := d.FNode;
            Next;
        end;
        close;
    end;
    if Assigned(Node) then
        FTreeView.Selected[Node] := true;

end;

procedure TNodeWorkLog.Populate;
var
    Node: PVirtualNode;
    d: TNodeWorkLog;
begin
    Node := nil;
    with DataModule1.FDQueryWorksByParentRecordID do
    begin
        ParamByName('parent_work_id').Value := FWorkID;
        open;
        First;
        FTreeView.HasChildren[FNode] := false;
        while not Eof do
        begin
            d := TNodeWorkLog.Create(FTreeView, FNode,
              DataModule1.FDQueryWorksByParentRecordID);
            Node := d.FNode;
            Next;
            FTreeView.HasChildren[FNode] := true;
        end;
        close;
    end;
    if Assigned(Node) then
        FTreeView.Selected[Node] := true;
end;

end.
