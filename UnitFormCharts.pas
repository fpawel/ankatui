unit UnitFormCharts;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Comp.Client, UnitData,
    VirtualTrees, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, HTMLUn2, HtmlView,
    System.ImageList, UnitFormChartsNodes, Vcl.ImgList, VclTee.TeeGDIPlus,
    VclTee.TeEngine, VclTee.TeeProcs, VclTee.Chart, VclTee.Series;

type

    TFormCharts = class(TForm)
        ImageList1: TImageList;
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

    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    FormCharts: TFormCharts;

implementation

{$R *.dfm}

uses dateutils, FireDAC.Stan.PAram, stringutils, richeditutils,
    variantutils, stringgridutils, UnitFormChartSeries;

var
    FormChart1: TFormChartSeries;
    DateTimeFmtStngs: TFormatSettings;

function inttostr2(n: integer): string;
begin
    result := inttostr(n);
    if n < 10 then
        result := '0' + result;
end;

procedure GetSeriesValues(SeriesID: int64);
begin
    FormChart1.NewChart;
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text :=
          'SELECT * FROM chart_value_info WHERE series_id = :series_id;';
        ParamByName('series_id').Value := SeriesID;
        open;
        First;
        while not Eof do
        begin
            FormChart1.AddValue(FieldValues['product_serial'],
                FieldValues['var'], FieldValues['value'],
                    StrToDateTime(FieldValues['created_at'], DateTimeFmtStngs));
            Next;
        end;
        Close;
        Free;
    end;
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

procedure TFormCharts.FormCreate(Sender: TObject);
var
    year: integer;

begin
    VirtualStringTree1.NodeDataSize := SizeOf(RTreeData);
    for year in DataModule1.SeriesYears do
        TNodeYear.Create(VirtualStringTree1, year);
    Application.CreateForm(TFormChartSeries, FormChart1);
    with FormChart1 do
    begin
        Parent := self;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := false;
        Font.Assign(self.Font);
    end;

    DateTimeFmtStngs := TFormatSettings.Create(GetThreadLocale);
    DateTimeFmtStngs.DateSeparator := '.';
    DateTimeFmtStngs.ShortDateFormat := 'dd/MM/yyyy';
    DateTimeFmtStngs.TimeSeparator := ':';
    DateTimeFmtStngs.LongTimeFormat := 'h:mm:ss';
end;

procedure TFormCharts.RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
begin
    RichEdit_PopupMenu(TRichEdit(Sender));
    Handled := true;
end;

procedure TFormCharts.VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
    if Sender.Selected[Node] then
    begin
        TargetCanvas.Brush.Color := clSkyBlue;
        TargetCanvas.FillRect(CellRect);
    end;
end;

procedure TFormCharts.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;

    ser: TFastLineSeries;
    product_serial: integer;

begin

    FormChart1.Visible := false;
    if not Assigned(Node) then
        exit;
    p := Sender.GetNodeData(Node);
    if p.X is TNodeSeries then
        begin
            GetSeriesValues((p.X as TNodeSeries).FSeriesInfo.SeriesID);
            FormChart1.Visible := true;
        end;

end;

procedure TFormCharts.VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    freeNodeData(VirtualStringTree1, Node.FirstChild);
    VirtualStringTree1.DeleteChildren(Node);
    p.X.FPopulated := false;
end;

procedure TFormCharts.VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: boolean);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if p.X.FPopulated then
        exit;
    p.X.Populate;
    p.X.FPopulated := Node.ChildCount > 0;
end;

procedure TFormCharts.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
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

procedure TFormCharts.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2, 3] then
        CellText := p.X.FColumn[Column].Text;
end;

procedure TFormCharts.VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
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

                if p.X is TNodeSeries then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clNavy;

                end;

            end;

    end;

end;

end.
