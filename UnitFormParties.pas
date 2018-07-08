unit UnitFormParties;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Comp.Client, UnitData,
    VirtualTrees, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, HTMLUn2, HtmlView,
    System.ImageList, UnitFormPartiesNodes, Vcl.ImgList;

type

    TFormParties = class(TForm)
        ImageList1: TImageList;
        Panel1: TPanel;
        HtmlViewer1: THtmlViewer;
        RichEdit1: TRichEdit;
        Splitter1: TSplitter;
        VirtualStringTree1: TVirtualStringTree;
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

        function PartyHTML(n: TNodeParty): string;
    private
        { Private declarations }
        procedure Refresh;
    public
        { Public declarations }
    end;

var
    FormParties: TFormParties;

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

procedure TFormParties.FormCreate(Sender: TObject);
begin
    VirtualStringTree1.NodeDataSize := SizeOf(RTreeData);
    RichEdit1.Visible := false;
    HtmlViewer1.Visible := false;
    Refresh;
end;

procedure TFormParties.Refresh;
var
  year: Integer;
begin
    VirtualStringTree1.Clear;
    for year in DataModule1.PartiesYears do
        TNodeYear.Create(VirtualStringTree1, year);

end;

function TFormParties.PartyHTML(n: TNodeParty): string;
var
    i, j: integer;
    v1, v2: TKeyValue;
    coef, serial: integer;
    coefs, coefs_products: TArray<integer>;
    s: string;
begin
    coefs := DataModule1.PartyCoefsWithProducts(n.FPartyID);
    coefs_products := DataModule1.PartyProductsWithCoefs(n.FPartyID);

    result := '<html> <head> <title> Партия</title> ' +
      '<style TYPE="text/css"> ' +
      'table, th, td { border-collapse: collapse; }' +
      'th, td { font-size: 14px; padding: 5px 8px;} ' +
      '.col2 { color: #000080; font-weight: bold; } </style> </head> <body>';

    result := result + Format('<h3>Параметры партии №%d от %s</h3>',
      [n.FPartyID, DateToStr(n.FCreatedAt)]);
    result := result + '<table border="0" > ';

    i := 0;
    while i < length(n.FValues) do
    begin
        v1 := n.FValues[i];
        if i = length(n.FValues) - 1 then
        begin
            result := result +
              Format('<tr><td align="right">%s:</td><td align="left" class="col2">%s</td></tr>',
              [v1.Key, v1.Value]);
        end
        else
        begin
            i := i + 1;
            v2 := n.FValues[i];
            result := result +
              Format('<tr><td align="right">%s:</td><td align="left" class="col2">%s</td>'
              + '<td align="right">%s:</td><td align="left" class="col2">%s</td></tr>',
              [v1.Key, v1.Value, v2.Key, v2.Value]);
        end;
        i := i + 1;
    end;
    result := result + '</table>';

    if length(coefs_products) > 0 then
    begin
        result := result + '<h3>Коэффициенты</h3>';
        result := result + '<table border="1" > ';

        result := result + '<tr>';
        result := result + '<th>№</th>';
        for serial in coefs_products do
            result := result + Format('<th>%s</th>', [inttostr2(serial)]);
        result := result + '</tr>';

        for coef in coefs do
        begin
            result := result + '<tr>';
            result := result + Format('<th>%s</th>', [inttostr2(coef)]);
            for serial in coefs_products do
                result := result + Format('<td align="right">%s</td>',
                  [DataModule1.ProductCoeffValue(n.FPartyID, serial, coef)]);
            result := result + '</tr>';

        end;
        result := result + '</table> ';
    end;
    result := result + '</body></html>';
end;

procedure TFormParties.RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
begin
    RichEdit_PopupMenu(TRichEdit(Sender));
    Handled := True;
end;

procedure TFormParties.VirtualStringTree1BeforeCellPaint
  (Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
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

procedure TFormParties.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
    s, str_message: string;
    product_serial: variant;

    created_at: TDateTime;
    i, level, work_index: integer;
begin
    RichEdit1.Visible := false;
    HtmlViewer1.Visible := false;
    if not Assigned(Node) then
        exit;
    p := Sender.GetNodeData(Node);
    if p.X is TNodeParty then
    begin
        HtmlViewer1.LoadFromString(PartyHTML(p.X as TNodeParty));
        HtmlViewer1.Align := alClient;
        HtmlViewer1.Visible := True;
        exit;
    end;

    if p.X is TNodePartyDayLog then
    begin
        RichEdit1.Align := alClient;
        RichEdit1.Visible := True;
        with p.X as TNodePartyDayLog do
            DataModule1.PrintDayLog(RichEdit1, Fday, Fmonth, Fyear);
        exit;
    end;

    if p.X is TNodeWorkLog then
    begin
        RichEdit1.Align := alClient;
        RichEdit1.Visible := True;
        DataModule1.PrintWorkLog(RichEdit1, (p.X as TNodeWorkLog).FRecordID);
        exit;
    end;

end;

procedure TFormParties.VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    freeNodeData(VirtualStringTree1, Node.FirstChild);
    VirtualStringTree1.DeleteChildren(Node);
    p.X.FPopulated := false;
end;

procedure TFormParties.VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
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
    p.X.FPopulated := True;
end;

procedure TFormParties.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
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

procedure TFormParties.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2] then
        CellText := p.X.FColumn[Column].Text;
end;

procedure TFormParties.VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
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

                if p.X is TNodeParty then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clNavy;

                end
                else if p.X is TNodeProduct then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clMaroon;
                end

                else if (p.X is TNodePartyDayLog) or (p.X is TNodePartyLogsRoot) then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clTeal;
                    TargetCanvas.Font.Style := [fsItalic];

                end

                else if p.X is TNodeWorkLog then
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
