unit UnitFormLog;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Comp.Client, UnitData,
    VirtualTrees, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, HTMLUn2, HtmlView,
    System.ImageList, UnitFormLogNodes, Vcl.ImgList, virtual_tree_node;

type
    TOnRenderMessages = reference to procedure;

    TFormLog = class(TForm)
        ImageList1: TImageList;
        Splitter1: TSplitter;
        VirtualStringTree1: TVirtualStringTree;
        RichEdit1: TRichEdit;
        procedure FormCreate(Sender: TObject);
        procedure VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);
        procedure VirtualStringTree1Change(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
          Node: PVirtualNode; var Allowed: boolean);
        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: boolean; var ImageIndex: TImageIndex);
        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
          var CellText: string);
        procedure VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
        procedure RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
          var Handled: boolean);

    private
        { Private declarations }
    public
        { Public declarations }
        FOnRenderMessages: TOnRenderMessages;

    end;

var
    FormLog: TFormLog;

implementation

{$R *.dfm}

uses dateutils, FireDAC.Stan.PAram, stringutils, richeditutils,
    variantutils, stringgridutils;

function inttostr2(n: integer): string;
begin
    result := inttostr(n);
    if n < 10 then
        result := '0' + result;
end;

procedure TFormLog.FormCreate(Sender: TObject);
var
    d: RTreeData;
    v:boolean;
begin
    v := false;
    VirtualStringTree1.NodeDataSize := SizeOf(RTreeData);
    with DataModule1.FDQueryWorkLogYears do
    begin
        open;
        First;
        while not Eof do
        begin
            TNodeYear.Create(VirtualStringTree1, FieldValues['year']);
            v := true;
            Next;
        end;
        Close;
    end;
    if not v then
        TNodeYear.Create(VirtualStringTree1, YearOf(now));


end;

procedure TFormLog.RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
begin
    RichEdit_PopupMenu(TRichEdit(Sender));
    Handled := true;
end;

procedure TFormLog.VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
    p: PTreeData;
    d: TNodeData;
begin
    p := Sender.GetNodeData(Node);
    d := p.X;

    if Sender.Selected[Node] then
    begin
        TargetCanvas.Brush.Color := clSkyBlue;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;
end;

procedure TFormLog.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
    s, str_message: string;
    product_serial: variant;

    created_at: TDateTime;
    i, level, work_index: integer;
begin
    if not Assigned(Node) then
        exit;
    p := Sender.GetNodeData(Node);

    if p.X is TNodeWorkLog then
    begin
        RichEdit1.Align := alClient;
        RichEdit1.Visible := true;
        DataModule1.PrintWorkLog(RichEdit1, (p.X as TNodeWorkLog).FworkID);
        if Assigned(FOnRenderMessages) then
            FOnRenderMessages;
        exit;
    end;

    if p.X is TNodeDay then
    begin
        RichEdit1.Align := alClient;
        RichEdit1.Visible := true;

        with p.X as TNodeDay do
            DataModule1.PrintDayLog(RichEdit1, FDay, FMonth, FYear);
        if Assigned(FOnRenderMessages) then
            FOnRenderMessages;
        exit;
    end;
    RichEdit1.Lines.Clear;
end;

procedure TFormLog.VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    freeNodeData(VirtualStringTree1, Node.FirstChild);
    VirtualStringTree1.DeleteChildren(Node);
    p.X.FPopulated := false;
end;

procedure TFormLog.VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: boolean);
var
    p: PTreeData;
    d: RTreeData;
    v: variant;
begin
    p := Sender.GetNodeData(Node);
    if p.X.FPopulated then
        exit;
    p.X.Populate;
    p.X.FPopulated := node.ChildCount > 0;
end;

procedure TFormLog.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: boolean; var ImageIndex: TImageIndex);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2] then
        case Kind of
            ikNormal, ikSelected:
                ImageIndex := p.X.FColumn[Column].ImageIndex;

        end;

end;

procedure TFormLog.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2] then
        CellText := p.X.FColumn[Column].Text;
end;

procedure TFormLog.VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
    p: PTreeData;

begin
    p := Sender.GetNodeData(Node);
    if not Column in [0, 1, 2] then
        exit;

    case Column of
        0:
            begin

                if p.X is TNodeWorkLog then
                begin
                    if not Sender.Selected[Node] then
                    begin
                        TargetCanvas.Font.Color := clTeal;
                        if (p.X as TNodeWorkLog).FHasError then
                            TargetCanvas.Font.Color := clRed;
                    end;

                    TargetCanvas.Font.Style := [fsItalic];

                end;

            end;
    end;

end;

end.
