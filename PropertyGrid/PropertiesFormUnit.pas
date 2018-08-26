unit PropertiesFormUnit;

// Virtual Treeview sample form demonstrating following features:
// - Property page like string tree with individual node editors.
// - Incremental search.
// Written by Mike Lischke.
{$WARN UNSAFE_CODE OFF} // Prevent warnins that are not applicable

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, VirtualTrees, ImgList, ExtCtrls, UITypes, System.ImageList,
    PropertyValueEditors, config;

const
    // Helper message to decouple node change handling from edit handling.
    WM_STARTEDITING = WM_USER + 778;

type

    TPropertiesForm = class(TForm)
        VST3: TVirtualStringTree;
        TreeImages: TImageList;
        procedure FormCreate(Sender: TObject);
        procedure VST3Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
        procedure VST3CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; out EditLink: IVTEditLink);
        procedure VST3Editing(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; var Allowed: Boolean);
        procedure VST3GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
          var HintText: string);
        procedure VST3GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: Boolean; var Index: TImageIndex);
        procedure VST3GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
        procedure VST3InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
          var ChildCount: Cardinal);
        procedure VST3InitNode(Sender: TBaseVirtualTree;
          ParentNode, Node: PVirtualNode;
          var InitialStates: TVirtualNodeInitStates);
        procedure VST3PaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
        procedure VST3IncrementalSearch(Sender: TBaseVirtualTree;
          Node: PVirtualNode; const SearchText: string; var Result: Integer);
        procedure VST3FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
        procedure VST3DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
          Node: PVirtualNode; Column: TColumnIndex; const Text: string;
          const CellRect: TRect; var DefaultDraw: Boolean);
        procedure VST3BeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);
        procedure FormHide(Sender: TObject);

    private
        FConfig: TConfig;
        FHandlePropertyValueChanged: THandlePropertyValueChanged;
        procedure WMStartEditing(var Message: TMessage);
          message WM_STARTEDITING;
    public

        procedure SetConfig(AConfig: TConfig);
        procedure SetPropertyValueChanged(AHandlePropertyValueChanged:THandlePropertyValueChanged);
    end;

var
    PropertiesForm: TPropertiesForm;

    // ----------------------------------------------------------------------------------------------------------------------

implementation

uses
    Math, PropertyUtils, stringgridutils;

{$R *.DFM}
// ----------------- TPropertiesForm ------------------------------------------------------------------------------------

procedure TPropertiesForm.FormCreate(Sender: TObject);
begin
    // We assign these handlers manually to keep the demo source code compatible
    // with older Delphi versions after using UnicodeString instead of WideString.
    // VST3.OnGetText := VST3GetText;
    // VST3.OnGetHint := VST3GetHint;
    // VST3.OnIncrementalSearch := VST3IncrementalSearch;

    // Always tell the tree how much data space per node it must allocated for us. We can do this here, in the
    // object inspector or in the OnGetNodeDataSize event.
    VST3.NodeDataSize := SizeOf(PConfigProperty);
    // The VCL (D7 and lower) still uses 16 color image lists. We create a high color version explicitely because it
    // looks so much nicer.
    ConvertToHighColor(TreeImages);

end;

procedure TPropertiesForm.SetConfig(AConfig: TConfig);
begin
    VST3.Clear;
    VST3.RootNodeCount := 0;
    FConfig := AConfig;
    VST3.RootNodeCount := Length(FConfig);
end;

procedure TPropertiesForm.SetPropertyValueChanged(AHandlePropertyValueChanged:THandlePropertyValueChanged);
begin
    FHandlePropertyValueChanged := AHandlePropertyValueChanged;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3InitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
    Data: PConfigProperty;
begin
    Data := Sender.GetNodeData(Node);
    if Node.Parent = Sender.RootNode then
        ChildCount := Length(FConfig[Node.Index].FProperties)
    else
        ChildCount := 0;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

var
    Data: PConfigProperty;
    section_index: Integer;

begin

    if ParentNode = nil then
    begin
        InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
        Sender.SetNodeData(Node, TConfigProperty.Create);
        Data := Sender.GetNodeData(Node);
        Data.FValue := FConfig[Node.Index].FSectionName;
    end
    else
    begin

        Sender.SetNodeData(Node, FConfig[Node.Parent.Index].FProperties
          [Node.Index]);

    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

var
    Data: PConfigProperty;

begin
    if TextType <> ttNormal then
        exit;

    Data := Sender.GetNodeData(Node);

    if (Node.Parent = Sender.RootNode) then
    begin
        if Column = 0 then
            CellText := FConfig[Node.Index].FHint;
        exit;
    end;

    case Column of
        0:
            begin
                CellText := FConfig[Node.Parent.Index].FProperties
                  [Node.Index].FHint;
            end;
        1:
            begin
                CellText := Data.FValue;
            end;
        2:
            begin
                CellText := Data.FError;
            end;
        3:
            if Data.FMinSet then
            begin

                CellText := Floattostr(Data.FMin);
            end;
        4:
            if Data.FMaxSet then
            begin

                CellText := Floattostr(Data.FMax);
            end;
        5:
            begin
                CellText := Data.FDefaultValue;
            end;
    end;

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3GetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

var
    Data: PConfigProperty;

begin

    if Node.Parent <> Sender.RootNode then
    begin
        Data := Sender.GetNodeData(Node);
        HintText := Data.FHint;

        if Data.FMinSet  then
            HintText := HintText + #13 + 'минимум: ' + floattostr(Data.FMin);

        if Data.FMaxSet  then
            HintText := HintText + #13 + 'максимум: ' + floattostr(Data.FMax);

        HintText := HintText + #13 + 'по умолчанию: ' + Data.FDefaultValue;

        if Data.FError <> '' then
            HintText := HintText + #13#13 + '"' + Data.FValue + '": ' +
              Data.FError;

    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var Index: TImageIndex);

var
    Data: PConfigProperty;

begin
    if (Kind in [ikNormal, ikSelected]) and (Column = 0) then
    begin
        if Node.Parent = Sender.RootNode then
            Index := 1 // root nodes, this is an open folder
        else
        begin
            Data := Sender.GetNodeData(Node);
            if Data.FType <> '' then
                Index := 3
            else
                Index := 2;
        end;
    end;
end;


// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3Editing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

var
    Data: PConfigProperty;

begin
    with Sender do
    begin
        Data := GetNodeData(Node);
        Allowed := (Node.Parent <> RootNode) and (Column = 1) and
          (Data.FType <> '');
    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
    Data: PConfigProperty;
begin
    with TargetCanvas do
    begin
        Data := Sender.GetNodeData(Node);

        if (Node.Parent = Sender.RootNode) and FConfig[Node.Index].HasError or
          (Data.FError <> '') then
        begin
            Brush.Color := cl3dlight;
        end;

        FillRect(CellRect);

    end;
end;

procedure TPropertiesForm.VST3Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

begin
    with Sender do
    begin
        // Start immediate editing as soon as another node gets focused.
        if Assigned(Node) and (Node.Parent <> RootNode) and
          not(tsIncrementalSearching in TreeStates) then
        begin
            // We want to start editing the currently selected node. However it might well happen that this change event
            // here is caused by the node editor if another node is currently being edited. It causes trouble
            // to start a new edit operation if the last one is still in progress. So we post us a special message and
            // in the message handler we then can start editing the new node. This works because the posted message
            // is first executed *after* this event and the message, which triggered it is finished.
            PostMessage(Self.Handle, WM_STARTEDITING, WPARAM(Node), 0);
        end;
    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3CreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);

// This is the callback of the tree control to ask for an application defined edit link. Providing one here allows
// us to control the editing process up to which actual control will be created.
// TPropertyEditLink implements an interface and hence benefits from reference counting. We don't need to keep a
// reference to free it. As soon as the tree finished editing the class will be destroyed automatically.
var x : TPropertyEditLink;
begin
    x := TPropertyEditLink.Create;
    x.SetPropertyValueChangedHandler(self.FHandlePropertyValueChanged);
    EditLink := x;
end;

procedure TPropertiesForm.VST3DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
    Data: PConfigProperty;
    R: TRect;
begin
    Data := Sender.GetNodeData(Node);
    if (Column = 1) and (Data.FType = VtcBool) then
    begin
        R := CellRect;
        R.Left := R.Left - 9;
        R.Right := R.Right - 9;
        DrawCheckbox(Sender, TargetCanvas, R, Data.FValue <> '0', '');
        DefaultDraw := false;
    end;

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);

var
    Data: PConfigProperty;

begin
    // Make the root nodes underlined and draw changed nodes in bold style.
    if Node.Parent = Sender.RootNode then
        TargetCanvas.Font.Style := [fsUnderline];
    Data := Sender.GetNodeData(Node);
    if (Node.Parent = Sender.RootNode) and FConfig[Node.Index].HasError or
      (Data.FError <> '') then
    begin
        TargetCanvas.Font.Color := clRed;
    end;

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3IncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; var Result: Integer);

var
    S, PropText: string;

begin
    S := SearchText;

    if Node.Parent = Sender.RootNode then
    begin
        // root nodes
        PropText := FConfig[Node.Index].FHint;
    end
    else
        PropText := FConfig[Node.Parent.Index].FProperties[Node.Index]
          .FPropertyName;

    // By using StrLIComp we can specify a maximum length to compare. This allows us to find also nodes
    // which match only partially. Don't forget to specify the shorter string length as search length.
    Result := StrLIComp(PChar(S), PChar(PropText),
      Min(Length(S), Length(PropText)))
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.FormHide(Sender: TObject);
begin
    VST3.CancelEditNode;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.WMStartEditing(var Message: TMessage);

// This message was posted by ourselves from the node change handler above to decouple that change event and our
// intention to start editing a node. This is necessary to avoid interferences between nodes editors potentially created
// for an old edit action and the new one we start here.

var
    Node: PVirtualNode;

begin
    Node := Pointer(Message.WPARAM);
    // Note: the test whether a node can really be edited is done in the OnEditing event.
    VST3.EditNode(Node, 1);
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    Data: PConfigProperty;

begin
    Data := Sender.GetNodeData(Node);
    Finalize(Data);
end;

end.
