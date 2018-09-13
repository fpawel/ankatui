unit UnitFormCurrentChart;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VclTee.TeEngine,
    VclTee.TeeProcs, VclTee.Chart, Vcl.StdCtrls, Vcl.ExtCtrls,
    System.Generics.collections, System.Generics.Defaults, models,
    VclTee.Series, Vcl.ComCtrls,
    Vcl.ToolWin, System.ImageList, Vcl.ImgList;

type
    ProductVar = record
        ProductSerial, VarID: integer;
    end;

    TFormCurrentChart = class(TForm)
        Panel14: TPanel;
        Panel1: TPanel;
        Panel4: TPanel;
        Panel8: TPanel;
        Panel10: TPanel;
        ImageList1: TImageList;
        ListBox1: TListBox;
        PanelConsolePlaceholder: TPanel;
        Chart1: TChart;
        Splitter1: TSplitter;
        procedure FormCreate(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure FormHide(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        { Private declarations }
        FSeries: TDictionary<ProductVar, TFastLineSeries>;

    public
        { Public declarations }
        procedure AddValue(product_serial, var_id: integer; value: double);

        procedure Setup(par: TwinControl);
        procedure NewChart;
    end;

var
    FormCurrentChart: TFormCurrentChart;

implementation

{$R *.dfm}

uses UnitData, stringutils, dateutils, StrUtils, Unit1;

procedure TFormCurrentChart.FormCreate(Sender: TObject);
begin
    FSeries := TDictionary<ProductVar, TFastLineSeries>.create;
    Chart1.title.Visible := false;
    Width := Form1.FIni.ReadInteger('FormCurrentChart', 'Width', Width);
end;

procedure TFormCurrentChart.FormHide(Sender: TObject);
begin
    Splitter1.Parent := nil;
    Splitter1.Visible := false;
    Parent := nil;

end;

procedure TFormCurrentChart.FormResize(Sender: TObject);
begin
    Form1.FIni.WriteInteger('FormCurrentChart', 'Width', Width);
end;

procedure TFormCurrentChart.FormShow(Sender: TObject);
begin
    Splitter1.Parent := Parent;
    Splitter1.Visible := true;
    Splitter1.Left := 0;

end;

procedure TFormCurrentChart.ListBox1Click(Sender: TObject);
var
    i, dev_var: integer;
    k: ProductVar;
    xs: array of ProductVar;
begin
    SetLength(xs, 0);
    Chart1.RemoveAllSeries;
    for i := 0 to ListBox1.Items.Count - 1 do
    begin
        if not ListBox1.Selected[i] then
            Continue;
        dev_var := DataModule1.GetDeviceVarByName(ListBox1.Items[i]);
        for k in FSeries.Keys do
        begin
            if k.VarID = dev_var then
            begin
                SetLength(xs, length(xs) + 1);
                xs[length(xs) - 1] := k;
            end;
        end;
    end;
    TArray.Sort<ProductVar>(xs, TDelegatedComparer<ProductVar>.Construct(
        function(const a, b: ProductVar): integer
        begin
            Result := TComparer<integer>.Default.Compare(a.VarID, b.VarID);
            if Result = 0 then
                Result := TComparer<integer>.Default.Compare(a.ProductSerial,
                  b.ProductSerial);
        end));
    for i := 0 to length(xs) - 1 do
    begin
        Chart1.AddSeries(FSeries[xs[i]]);
    end;
end;

procedure TFormCurrentChart.NewChart;
var
    ser: TFastLineSeries;
    k: ProductVar;
begin
    Chart1.RemoveAllSeries;
    for ser in FSeries.Values do
    begin
        ser.Free;
    end;
    FSeries.Clear;
    ListBox1.Clear;
    // Panel12.Caption := format('%s %s', [datetimetostr(now), FChartTitle]);
end;

function CompareVars(List: TStringList; Index1, Index2: integer): integer;
var
    d1, d2: integer;
begin
    d1 := DataModule1.GetDeviceVarByName(List[Index1]);
    d2 := DataModule1.GetDeviceVarByName(List[Index2]);

    if d1 < d2 then
        Result := -1
    else if d1 > d2 then
        Result := 1
    else
        Result := 0;
end;

procedure TFormCurrentChart.AddValue(product_serial, var_id: integer;
value: double);
var
    varName: string;
    ser: TFastLineSeries;
    k: ProductVar;
    sl: TStringList;
    n,i: integer;
    sel:array of boolean;
begin

    varName := DataModule1.GetDeviceVarName(var_id);
    k.ProductSerial := product_serial;
    k.VarID := var_id;
    if ListBox1.Items.IndexOf(varName) = -1 then
    begin
        n := ListBox1.Items.Add(varName);
        ListBox1.Selected[n] := n = 0;

        SetLength(sel, ListBox1.Items.Count);
        for I := 0 to ListBox1.Items.Count-1 do
            sel[i] := ListBox1.Selected[i];


        sl := TStringList.create;
        sl.Assign(ListBox1.Items);
        sl.CustomSort(CompareVars);
        ListBox1.Items.Assign(sl);
        sl.Free;

        for I := 0 to ListBox1.Items.Count-1 do
            ListBox1.Selected[i] := sel[i] ;
    end;
    if not FSeries.TryGetValue(k, ser) then
    begin
        ser := TFastLineSeries.create(nil);
        ser.XValues.DateTime := true;
        ser.title := IntToStr(product_serial) + ' ' + varName;
        FSeries.Add(k, ser);

    end;
    ser.AddXY(now, value);

    with ListBox1 do
    begin
        if Selected[Items.IndexOf(varName)] then
        begin
            if ser.ParentChart = nil then
                Chart1.AddSeries(ser);
        end;
    end;

end;

procedure TFormCurrentChart.Setup(par: TwinControl);
begin
    Parent := par;
    Align := alRight;
    BorderStyle := bsNone;
    Visible := true;
    Left := 100500;
    BringToFront;
end;

end.
