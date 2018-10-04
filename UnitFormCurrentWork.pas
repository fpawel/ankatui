unit UnitFormCurrentWork;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, HostAppModels,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VirtualTrees,
    System.ImageList, Vcl.ImgList, CurrentWorkTreeData, Vcl.ExtCtrls;

type
    TFormCurrentWork = class(TForm)
        VirtualStringTree1: TVirtualStringTree;
        ImageList2: TImageList;
        procedure FormCreate(Sender: TObject);
        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: Boolean; var ImageIndex: TImageIndex);
        procedure VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);
        procedure VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
          var CellText: string);
        procedure VirtualStringTree1Change(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1Checking(Sender: TBaseVirtualTree;
          Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
        procedure FormShow(Sender: TObject);
    private
        { Private declarations }
        FAllowCheck: Boolean;
        procedure AddNode(par: PVirtualNode; op_info: TOperationInfo);

        procedure UpdateWorksCheckedConfigDB;
        function GetWork: TOperationInfo;
        function GetWorkName: string;
        function GetSelectedOperation: TOperationInfo;
        procedure SetupWorksTree;
        function RootNodeData: TNodeData;
    public
        { Public declarations }
        procedure SetRunError;
        procedure InitPipe(panel_message: TPanel);
        procedure run;
        property WorkName: string read GetWorkName;
        property AllowCheck: Boolean read FAllowCheck write FAllowCheck;
    end;

var
    FormCurrentWork: TFormCurrentWork;

const
    BS_LEFT = $100;
    BS_RIGHT = $200;
    BS_CENTER = 768;
    BS_TOP = $400;
    BS_BOTTOM = $800;
    BS_VCENTER = 3072;

implementation

{$R *.dfm}

uses rest.json, stringutils, unitdata, UnitHostAppData, Unit1;

procedure TFormCurrentWork.FormCreate(Sender: TObject);
var
    defstyle: dWord;
begin
    VirtualStringTree1.NodeDataSize := SizeOf(RTreeData);
    FAllowCheck := true;

end;

procedure TFormCurrentWork.FormShow(Sender: TObject);
begin
    OnShow := nil;
    if HostAppData.FPipe.Connected then
        SetupWorksTree;

end;

procedure TFormCurrentWork.VirtualStringTree1BeforeCellPaint
  (Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
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

procedure TFormCurrentWork.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
    // Button1.Caption := '   Запустить: [0] Настройка Анкат';
    // if SelectedOperation <> nil then
    // Button1.Caption := Format('   Запустить: [%d] %s',
    // [SelectedOperation.FOrdinal, SelectedOperation.FName]);

end;

procedure TFormCurrentWork.VirtualStringTree1Checking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
    Allowed := AllowCheck;
end;

procedure TFormCurrentWork.VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    p := VirtualStringTree1.GetNodeData(Node);
    p.x.Free;
end;

procedure TFormCurrentWork.VirtualStringTree1GetImageIndex
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
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

procedure TFormCurrentWork.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
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

procedure TFormCurrentWork.VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
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

function TFormCurrentWork.GetSelectedOperation: TOperationInfo;
begin
    if Assigned(VirtualStringTree1.FocusedNode) then
        Result := PTreeData(VirtualStringTree1.GetNodeData
          (VirtualStringTree1.FocusedNode)).x.FInfo
    else
        Result := nil;
end;

procedure TFormCurrentWork.SetRunError;
var
    n: TNodeData;
begin
    for n in RootNodeData.FDescendants do
        if n.FRun then
        begin
            n.FInfo.FHasError := true;
            VirtualStringTree1.RepaintNode(n.FNode);
        end;
end;

procedure TFormCurrentWork.AddNode(par: PVirtualNode; op_info: TOperationInfo);
var
    Node: PVirtualNode;
    parent_tree_data: PTreeData;
    d: RTreeData;
    i: integer;
begin

    Node := VirtualStringTree1.AddChild(par);
    d.x := TNodeData.Create(Node, op_info);

    if par <> nil then
    begin
        parent_tree_data := VirtualStringTree1.GetNodeData(par);
        d.x.FParent := parent_tree_data.x;
        parent_tree_data.x.FChildren.Add(d.x);
    end;
    VirtualStringTree1.SetNodeData(Node, PTreeData(d));

    for i := 0 to length(op_info.FChildren) - 1 do
    begin
        AddNode(Node, op_info.FChildren[i]);
    end;

    if par = nil then
        d.x.EnumDescendants;

    VirtualStringTree1.CheckType[Node] := ctTriStateCheckBox;
    Node.CheckState := TCheckState(DataModule1.GetCurrentWorkCheckState
      (op_info.FOrdinal));

    VirtualStringTree1.Expanded[Node] := (Node = VirtualStringTree1.RootNode) or
      (Node.Parent <> Nil) and (Node.Parent = VirtualStringTree1.RootNode);

end;

procedure TFormCurrentWork.SetupWorksTree;
var
    Node: PVirtualNode;
begin
    VirtualStringTree1.Clear;

    AddNode(nil, HostAppData.RootWork);

    Node := VirtualStringTree1.GetFirst;
    Node := VirtualStringTree1.GetNext(Node);
    if Assigned(Node) then
        VirtualStringTree1.TreeOptions.MiscOptions :=
          VirtualStringTree1.TreeOptions.MiscOptions + [toCheckSupport]
    else
        VirtualStringTree1.TreeOptions.MiscOptions :=
          VirtualStringTree1.TreeOptions.MiscOptions - [toCheckSupport];

end;

procedure TFormCurrentWork.InitPipe(panel_message: TPanel);
begin

    HostAppData.FPipe.Handle('CURRENT_WORK',
        function(content: string): string
        var
            d: TNodeData;
            op: TNotifyOperation;
        begin
            Result := '';

            op := TJson.JsonToObject<TNotifyOperation>(content);
            d := RootNodeData.FDescendants[op.FOrdinal];
            if op.FName <> d.FInfo.FName then
                exit;

            if op.FRun then
            begin
                VirtualStringTree1.Expanded[d.FNode] := true;
                d.FInfo.FHasMessage := true;
                d.FInfo.FHasError := false;
            end;
            if (d.FChildren.Count = 0) and op.FRun then
            begin
                panel_message.Caption := d.text;
                panel_message.Font.Color := clNavy;
            end;
            d.FRun := op.FRun;
            VirtualStringTree1.RepaintNode(d.FNode);
        end);

end;

procedure TFormCurrentWork.UpdateWorksCheckedConfigDB;
var
    Node: PVirtualNode;
    d: PTreeData;
    xs: array of integer;
    n: integer;
begin
    Node := VirtualStringTree1.GetFirst;
    while Assigned(Node) do
    begin
        d := VirtualStringTree1.GetNodeData(Node);
        n := d.x.FInfo.FOrdinal;
        if length(xs) <= n then
            SetLength(xs, n + 1);
        xs[n] := ord(Node.CheckState);
        Node := VirtualStringTree1.GetNext(Node);
    end;
    DataModule1.SetWorksChecked(xs);

end;

function TFormCurrentWork.RootNodeData: TNodeData;
begin
    Result := PTreeData(VirtualStringTree1.GetNodeData
      (VirtualStringTree1.GetFirst)).x;
end;

function TFormCurrentWork.GetWork: TOperationInfo;
begin
    Result := GetSelectedOperation;
    if Result = nil then
        Result := RootNodeData.FInfo;
end;

function TFormCurrentWork.GetWorkName: string;
begin
    exit(GetWork.FName);
end;

procedure TFormCurrentWork.run;
var
    i: integer;
begin
    for i := 0 to RootNodeData.FDescendants.Count - 1 do
    begin
        RootNodeData.FDescendants[i].FInfo.FHasError := false;
        RootNodeData.FDescendants[i].FInfo.FHasMessage := false;
        VirtualStringTree1.RepaintNode(RootNodeData.FDescendants[i].FNode);
    end;
    UpdateWorksCheckedConfigDB;
    HostAppData.FPipe.WriteMsgStr('RUN_MAIN_WORK', inttostr(GetWork.FOrdinal));

end;

end.
