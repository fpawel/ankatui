unit CurrentWork;

interface

uses CurrentWorkTreeData, System.Generics.Collections, Vcl.Graphics,
    VirtualTrees, Vcl.comctrls, Vcl.Controls, Vcl.ExtCtrls,
    System.Types, pipe, UnitData;

type
    TOperationCheckState = class
    public
        FOrdinal: integer;
        FCheckState: string;
    end;

    TWorkMsg = class
        FLevel: integer;
        FText: string;
        FWorkIndex: integer;
        FWork: string;
        FProductSerial: integer;
        FCreatedAt: TDAteTime;
    end;

    TCurrentWork = class
        FTreeView: TVirtualStringTree;
        FPanelTopText: TPanel;
        FPipe: TPipe;

        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: boolean; var ImageIndex: TImageIndex);

        procedure TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);

        procedure TreeViewPaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);

        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
          var CellText: string);

        procedure TreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

        procedure AddNode(par: PVirtualNode; op_info: TOperationInfo);
        function RootNodeData: TNodeData;

        procedure Run;

        procedure SetRunError;

        procedure ResetError;

        constructor Create(APipe: TPipe; APanelTopText: TPanel;
          ATreeView: TVirtualStringTree);
    end;

implementation

uses rest.json, System.sysutils, richeditutils, dateutils, stringutils,
    msglevel, variants,
    Winapi.Windows, Winapi.Messages, variantutils;

function checkStateToStr(x: TCheckState): string;
begin
    case x of
        csUncheckedNormal:
            exit('csUncheckedNormal');
        csUncheckedPressed:
            exit('csUncheckedPressed');
        csCheckedNormal:
            exit('csCheckedNormal');
        csCheckedPressed:
            exit('csCheckedPressed');
        csMixedNormal:
            exit('csMixedNormal');
        csMixedPressed:
            exit('csMixedPressed');
        csUncheckedDisabled:
            exit('csUncheckedDisabled');
        csCheckedDisabled:
            exit('csCheckedDisabled');
        csMixedDisabled:
            exit('csMixedDisabled');
    else
        exit('csCheckedNormal')
    end;
end;

function parseCheckState(x: string): TCheckState;
begin
    if x = 'csUncheckedNormal' then
        exit(csUncheckedNormal);

    if x = 'csUncheckedPressed' then
        exit(csUncheckedPressed);

    if x = 'csCheckedNormal' then
        exit(csCheckedNormal);

    if x = 'csCheckedPressed' then
        exit(csCheckedPressed);

    if x = 'csMixedNormal' then
        exit(csMixedNormal);

    if x = 'csMixedPressed' then
        exit(csMixedPressed);

    if x = 'csUncheckedDisabled' then
        exit(csUncheckedDisabled);

    if x = 'csCheckedDisabled' then
        exit(csCheckedDisabled);

    if x = 'csMixedDisabled' then
        exit(csMixedDisabled);

    exit(csCheckedNormal);
end;

constructor TCurrentWork.Create(APipe: TPipe; APanelTopText: TPanel;
  ATreeView: TVirtualStringTree);
begin
    inherited Create;
    FPipe := APipe;
    FTreeView := ATreeView;
    FPanelTopText := APanelTopText;

    FTreeView.OnBeforeCellPaint := TreeViewBeforeCellPaint;
    FTreeView.OnPaintText := TreeViewPaintText;
    FTreeView.OnChecked := TreeViewChecked;
    FTreeView.OnGetText := VirtualStringTree1GetText;
    FTreeView.OnGetImageIndex := VirtualStringTree1GetImageIndex;

    FPipe.Handle('SETUP_CURRENT_WORKS',
        procedure(content: string)
        var
            Node: PVirtualNode;
            p: PTreeData;
        begin
            Node := FTreeView.GetFirst;
            while Assigned(Node) do
            begin
                p := FTreeView.GetNodeData(Node);
                p.x.Free;
                Node := FTreeView.GetNext(Node);
            end;
            FTreeView.Clear;
            AddNode(nil, TJson.JsonToObject<TOperationInfo>(content));

            Node := FTreeView.GetFirst;
            Node := FTreeView.GetNext(Node);
            if Assigned(Node) then
                FTreeView.TreeOptions.MiscOptions :=
                  FTreeView.TreeOptions.MiscOptions + [toCheckSupport]
            else
                FTreeView.TreeOptions.MiscOptions :=
                  FTreeView.TreeOptions.MiscOptions - [toCheckSupport];
        end);

    FPipe.Handle('CURRENT_WORK',
        procedure(content: string)
        var
            d: TNodeData;
            op: TNotifyOperation;
        begin
            op := TJson.JsonToObject<TNotifyOperation>(content);
            d := RootNodeData.FDescendants[op.FOrdinal];
            if op.FName <> d.FInfo.FName then
                exit;

            if op.FRun then
            begin
                // VirtualStringTree1.Selected[d.FNode] := true;
                FTreeView.Expanded[d.FNode] := True;
                d.FInfo.FHasMessage := True;
                d.FInfo.FHasError := false;
            end;
            if (d.FChildren.Count = 0) and op.FRun then
            begin
                FPanelTopText.Caption := d.text;
                FPanelTopText.Font.Color := clNavy;
            end;
            d.FRun := op.FRun;
            FTreeView.RepaintNode(d.FNode);
        end);

end;

procedure TCurrentWork.ResetError;
var
    n: TNodeData;
begin
    for n in RootNodeData.FDescendants do
        if n.FInfo.FHasError then
        begin
            n.FInfo.FHasError := false;
            FTreeView.RepaintNode(n.FNode);
        end;
end;

procedure TCurrentWork.SetRunError;
var
    n: TNodeData;
begin
    for n in RootNodeData.FDescendants do
        if n.FRun then
        begin
            n.FInfo.FHasError := True;
            FTreeView.RepaintNode(n.FNode);
        end;
end;

function TCurrentWork.RootNodeData: TNodeData;
begin
    result := PTreeData(FTreeView.GetNodeData(FTreeView.GetFirst)).x;
end;

procedure TCurrentWork.AddNode(par: PVirtualNode; op_info: TOperationInfo);
var
    Node: PVirtualNode;
    parent_tree_data: PTreeData;
    d: rTreeData;
    i: integer;
begin

    Node := FTreeView.AddChild(par);
    d.x := TNodeData.Create(Node, op_info);

    if par <> nil then
    begin
        parent_tree_data := FTreeView.GetNodeData(par);
        d.x.FParent := parent_tree_data.x;
        parent_tree_data.x.FChildren.Add(d.x);
    end;
    FTreeView.SetNodeData(Node, PTreeData(d));

    for i := 0 to length(op_info.FChildren) - 1 do
    begin
        AddNode(Node, op_info.FChildren[i]);
    end;

    if par = nil then
        d.x.EnumDescendants;

    FTreeView.Expanded[Node] := True;
    FTreeView.CheckType[Node] := ctTriStateCheckBox;
    Node.CheckState := parseCheckState
      (DataModule1.GetCurrentWorkCheckState(op_info.FOrdinal));

    Node := FTreeView.GetFirst;
    while Assigned(Node) do
    begin
        FTreeView.Expanded[Node] := True;
        Node := FTreeView.GetNext(Node);
    end;

end;

procedure TCurrentWork.TreeViewChecked(Sender: TBaseVirtualTree;
Node: PVirtualNode);
var
    p: PTreeData;
    d: TNodeData;
    x: TOperationCheckState;
begin
    p := Sender.GetNodeData(Node);
    d := p.x;
    x := TOperationCheckState.Create;
    x.FOrdinal := d.FInfo.FOrdinal;
    x.FCheckState := checkStateToStr(FTreeView.CheckState[Node]);
    FPipe.WriteMsgJSON('CURRENT_WORK_CHECKED_CHANGED', x);
end;

procedure TCurrentWork.TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
CellPaintMode: TVTCellPaintMode; CellRect: TRect;

var ContentRect: TRect);
var
    p: PTreeData;
    d: TNodeData;
begin

    p := Sender.GetNodeData(Node);
    d := p.x;

    if d.FRun then
    begin
        TargetCanvas.Brush.Color := clMoneyGreen;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;

    if Sender.Selected[Node] then
    begin
        TargetCanvas.Brush.Color := clSkyBlue;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;

    if d.FInfo.FHasError then
    begin
        TargetCanvas.Brush.Color := cl3DLight;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;

    if d.FInfo.FHasMessage then
    begin
        TargetCanvas.Brush.Color := clInfoBk;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;

end;

procedure TCurrentWork.Run;
var
    Node: PVirtualNode;
    p: PTreeData;
    d: TNodeData;
    i: integer;
begin
    Node := FTreeView.GetFirst;
    d := RootNodeData;
    while Assigned(Node) do
    begin
        if FTreeView.Selected[Node] then
        begin
            p := FTreeView.GetNodeData(Node);
            d := p.x;
            break;
        end;
        Node := FTreeView.GetNext(Node);
    end;
    for i := 0 to d.Root.FDescendants.Count - 1 do
    begin
        d.Root.FDescendants[i].FInfo.FHasError := false;
        d.Root.FDescendants[i].FInfo.FHasMessage := false;
        FTreeView.RepaintNode(d.Root.FDescendants[i].FNode);
    end;
    FPipe.WriteMsgJSON('CURRENT_WORK_START', d.NotifyOperation);
end;

procedure TCurrentWork.TreeViewPaintText(Sender: TBaseVirtualTree;

const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
TextType: TVSTTextType);
var
    p: PTreeData;
    d: TNodeData;
begin

    p := Sender.GetNodeData(Node);
    d := p.x;
    if d.FInfo.FHasError and ((not Sender.Selected[Node]) or ((Column > 0)))
    then
    begin
        TargetCanvas.Font.Color := clRed;
    end;

    if Column = 1 then
        TargetCanvas.Font.Color := clNavy;

end;

procedure TCurrentWork.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
var CellText: string);
var
    p: PTreeData;
    d: TNodeData;

begin

    p := Sender.GetNodeData(Node);
    d := p.x;
    case Column of
        0:
            CellText := d.FInfo.FName;
        1:
            CellText := inttostr2(d.FInfo.FOrdinal);
    end;
end;

procedure TCurrentWork.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
var Ghosted: boolean; var ImageIndex: TImageIndex);
var
    p: PTreeData;
    d: TNodeData;

begin
    p := Sender.GetNodeData(Node);
    d := p.x;
    if (Column = 0) and (Kind in [ikNormal, ikSelected]) then
    begin

        if d.FRun then
            ImageIndex := 2
        else if d.FInfo.FHasError then
            ImageIndex := 0
        else if d.FInfo.FHasMessage then
            ImageIndex := 1;

    end;

end;

end.
